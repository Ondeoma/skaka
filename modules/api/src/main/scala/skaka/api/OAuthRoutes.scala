package skaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{`Content-Type`, Authorization, Location}
import skaka.auth.{ApiTokens, Crypto}
import skaka.db.*
import skaka.shared.*

import java.time.Instant

/**
 * 認可を求める外部のサービス（OAUTH_CLIENTS で登録する）. redirectUri は完全一致で照合する.
 * scopes はそのサービスが求めてよいスコープ（read・write）.
 */
final case class OAuthClient(id: String, secret: String, redirectUri: String, name: String, scopes: List[String] = List(TokenScope.Read))

/**
 * 外部のサービス（skuaka）への認可（OAuth 2.0 の認可コード + PKCE. RFC 6749・7636・7009）. 兄弟の skawau と同じ形.
 * 発行するトークンは、そのユーザーの API トークン（「<サービス名> 連携」. 設定の API トークンの一覧から失効できる）.
 * 取り決めは docs/skuaka-integration.md.
 */
final class OAuthRoutes(oauth: OAuthStore, clients: List[OAuthClient], val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport:

  private val CodeTtlSeconds = 60L
  private val CsrfCookie     = "skaka_oauth_csrf"

  private def client(id: String): Option[OAuthClient] = clients.find(_.id == id)
  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")
  private def esc(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&#39;")

  /** redirect_uri に結果を付けて戻す. */
  private def back(uri: String, params: (String, Option[String])*): IO[Response[IO]] =
    val q = params.collect { case (k, Some(v)) => s"$k=${enc(v)}" }.mkString("&")
    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(uri + (if uri.contains("?") then "&" else "?") + q))))

  /** 求められたスコープ（空白区切り. 省略すると read）. 発行するトークンは1つのスコープなので、write を含めば write. */
  private def scopes(p: String => Option[String]): List[String] =
    p("scope").map(_.split(' ').toList.map(_.trim).filter(_.nonEmpty).distinct).filter(_.nonEmpty).getOrElse(List(TokenScope.Read))

  private def tokenScope(ss: List[String]): String = if ss.contains(TokenScope.Write) then TokenScope.Write else TokenScope.Read

  // ---------------------------------------------------------------- 許可の画面（サーバーで HTML を組み立てる）

  /** 許可の画面. CSRF は Cookie と hidden の値の照合で防ぐ（form-action の CSP は付けない. 連携元へのリダイレクトの連鎖で止まるため）. */
  private def page(title: String, body: String): IO[Response[IO]] =
    val html = s"""<!DOCTYPE html><html lang="ja"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1">
      |<title>${esc(title)} | skaka</title><link rel="icon" href="/favicon.svg"><style>
      |:root{color-scheme:light dark}body{margin:0;font-family:system-ui,-apple-system,"Hiragino Sans","Noto Sans JP",sans-serif;background:#f1f5f9;color:#1e293b;
      |display:flex;min-height:100vh;align-items:center;justify-content:center;padding:1rem}
      |@media(prefers-color-scheme:dark){body{background:#0f172a;color:#e2e8f0}.card{background:#1c273d!important;border-color:#283548!important}}
      |.card{background:#fff;border:1px solid #e2e8f0;border-radius:14px;padding:1.6rem 1.8rem;max-width:440px;width:100%;
      |box-shadow:0 11px 15px -7px rgba(0,0,0,.2),0 24px 38px 3px rgba(0,0,0,.14)}
      |h1{font-size:1.15rem;margin:0 0 .8rem}p{line-height:1.7;margin:.4rem 0}.muted{color:#64748b;font-size:.85rem}
      |.logo{display:flex;align-items:center;gap:.5rem;color:#2563eb;font-weight:800;margin-bottom:.6rem}.logo img{width:28px;height:28px}
      |.actions{display:flex;gap:.6rem;justify-content:flex-end;margin-top:1.2rem}button{font:inherit;border-radius:8px;padding:.5rem 1.1rem;cursor:pointer;
      |border:1px solid #cbd5e1;background:transparent;color:inherit}button.primary{background:#2563eb;border-color:#2563eb;color:#fff}
      |ul{padding-left:1.2rem;margin:.4rem 0}</style></head><body><div class="card"><div class="logo"><img src="/logo.svg" alt="">skaka</div>$body</div></body></html>""".stripMargin
    IO.pure(Response[IO](Status.Ok).withEntity(html)(using EntityEncoder.stringEncoder).putHeaders(`Content-Type`(MediaType.text.html, Charset.`UTF-8`),
      "Cache-Control" -> "no-store", "X-Frame-Options" -> "DENY",
      "Content-Security-Policy" -> "default-src 'none'; style-src 'unsafe-inline'; img-src 'self'; frame-ancestors 'none'"))

  private def errorPage(msg: String): IO[Response[IO]] =
    page("認可できません", s"<h1>認可できません</h1><p>${esc(msg)}</p><p class=\"muted\">連携元のサービスの設定を確認してください。</p>").map(_.withStatus(Status.BadRequest))

  /** 認可の要求の確認（client_id・redirect_uri が不正ならリダイレクトしない）. (クライアント, 戻り先, state, code_challenge). */
  private def validate(p: String => Option[String]): Either[IO[Response[IO]], (OAuthClient, String, Option[String], String)] =
    (p("client_id").flatMap(client), p("redirect_uri")) match
      case (None, _) => Left(errorPage("登録されていないクライアントです（client_id）"))
      case (Some(c), r) if !r.contains(c.redirectUri) => Left(errorPage("戻り先（redirect_uri）が登録されたものと一致しません"))
      case (Some(c), _) =>
        val state = p("state")
        if !p("response_type").contains("code") then Left(back(c.redirectUri, "error" -> Some("unsupported_response_type"), "state" -> state))
        else if !p("code_challenge_method").contains("S256") || !p("code_challenge").exists(_.matches("^[A-Za-z0-9_-]{43}$")) then
          Left(back(c.redirectUri, "error" -> Some("invalid_request"), "error_description" -> Some("PKCE（S256）が必要です"), "state" -> state))
        else if !scopes(p).forall(c.scopes.contains) then Left(back(c.redirectUri, "error" -> Some("invalid_scope"), "state" -> state))
        else Right((c, c.redirectUri, state, p("code_challenge").get))

  // ---------------------------------------------------------------- トークンの発行・失効（クライアント認証は Basic）

  private def oauthError(status: Status, code: String, desc: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(Json.obj("error" -> code.asJson, "error_description" -> desc.asJson))
      .putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache").putHeaders(
        (if status == Status.Unauthorized then List[Header.ToRaw]("WWW-Authenticate" -> "Basic realm=\"skaka\"") else Nil)*))

  private def basicClient(req: Request[IO]): Option[OAuthClient] =
    req.headers.get[Authorization].collect { case Authorization(BasicCredentials(id, secret)) => (id, secret) }
      .flatMap((id, secret) => client(id).filter(c => Crypto.constantTimeEquals(c.secret, secret)))

  private def form(req: Request[IO])(f: Map[String, String] => IO[Response[IO]]): IO[Response[IO]] =
    req.as[UrlForm].attempt.flatMap {
      case Left(_)  => oauthError(Status.BadRequest, "invalid_request", "application/x-www-form-urlencoded で送ってください")
      case Right(u) => f(u.values.view.mapValues(_.headOption.getOrElse("")).toMap)
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "oauth" / "authorize" =>
      validate(param(req, _)) match
        case Left(r) => r
        case Right((c, _, _, _)) =>
          sessions.fromRequest(req).map(_.filter(_.token.isEmpty)).flatMap {
            case None =>
              // ログインしてからこの画面に戻る
              IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(s"/auth/login?returnTo=${enc(req.uri.renderString)}"))))
            case Some(p) =>
              val csrf   = Crypto.randomToken(24)
              val hidden = List("client_id", "redirect_uri", "state", "code_challenge", "code_challenge_method", "response_type", "scope")
                .flatMap(k => param(req, k).map(v => s"""<input type="hidden" name="$k" value="${esc(v)}">""")).mkString
              val name  = esc(c.name)
              val write = tokenScope(scopes(param(req, _))) == TokenScope.Write
              val what  = if write then "skaka の会議室を作成・変更し、会議の記録を読み取り" else "skaka の会議室と会議の記録を読み取り"
              page(s"$name との連携", s"""<h1>$name との連携</h1>
                |<p><b>$name</b> が、あなた（${esc(p.user.name)}）の権限で${what}ます。</p>
                |<ul class="muted">${if write then "<li>予定に Web 会議（skaka の会議室）を付けるのに使います</li>" else ""}
                |<li>会議に入ること・会議の中身を見ることはできません</li>
                |<li>設定の「API トークンと MCP」から、いつでも連携をやめられます</li></ul>
                |<form method="post" action="/oauth/authorize">$hidden<input type="hidden" name="csrf" value="$csrf">
                |<div class="actions"><button type="submit" name="decision" value="deny">拒否する</button>
                |<button type="submit" name="decision" value="allow" class="primary">許可する</button></div></form>""".stripMargin)
                .map(_.addCookie(ResponseCookie(CsrfCookie, csrf, path = Some("/oauth"), httpOnly = true, secure = cfg.cookieSecure,
                  sameSite = Some(SameSite.Strict), maxAge = Some(600))))
          }

    case req @ POST -> Root / "oauth" / "authorize" =>
      req.as[UrlForm].attempt.flatMap {
        case Left(_) => errorPage("リクエストの内容が正しくありません")
        case Right(u) =>
          val p = (k: String) => u.getFirst(k).map(_.trim).filter(_.nonEmpty)
          validate(p) match
            case Left(r) => r
            case Right((c, redirect, state, challenge)) =>
              val csrfOk = req.cookies.find(_.name == CsrfCookie).exists(ck => p("csrf").exists(v => Crypto.constantTimeEquals(ck.content, v)))
              sessions.fromRequest(req).map(_.filter(_.token.isEmpty)).flatMap {
                case None               => errorPage("ログインが切れました。連携元の画面からやり直してください")
                case Some(_) if !csrfOk => errorPage("画面の有効期限が切れました。連携元の画面からやり直してください")
                case Some(s) if p("decision").contains("allow") =>
                  val code = Crypto.randomToken(32)
                  oauth.createCode(Crypto.sha256(code), OAuthCode(c.id, redirect, s.user.id, challenge, tokenScope(scopes(p))),
                    Instant.now().plusSeconds(CodeTtlSeconds)) *> back(redirect, "code" -> Some(code), "state" -> state)
                case Some(_) => back(redirect, "error" -> Some("access_denied"), "state" -> state)
              }.map(_.removeCookie(ResponseCookie(CsrfCookie, "", path = Some("/oauth"))))
      }

    case req @ POST -> Root / "oauth" / "token" =>
      basicClient(req) match
        case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
        case Some(c) =>
          form(req) { f =>
            (f.get("grant_type"), f.get("code"), f.get("redirect_uri"), f.get("code_verifier")) match
              case (g, _, _, _) if !g.contains("authorization_code") =>
                oauthError(Status.BadRequest, if g.isEmpty then "invalid_request" else "unsupported_grant_type", "grant_type は authorization_code です")
              case (_, Some(code), Some(uri), Some(verifier)) if verifier.length >= 43 && verifier.length <= 128 =>
                oauth.consumeCode(Crypto.sha256(code)).flatMap {
                  case Some(oc) if oc.clientId == c.id && oc.redirectUri == uri && Crypto.constantTimeEquals(oc.codeChallenge, Crypto.pkceChallenge(verifier)) =>
                    val (token, prefix) = ApiTokens.generate()
                    oauth.replaceToken(oc.userId, c.id, s"${c.name} 連携", prefix, Crypto.sha256(token), oc.scope) *>
                      Ok(Json.obj("access_token" -> token.asJson, "token_type" -> "Bearer".asJson, "scope" -> oc.scope.asJson))
                        .map(_.putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache"))
                  case _ => oauthError(Status.BadRequest, "invalid_grant", "認可コードが無効です（期限切れ・使用済み・不一致）")
                }
              case _ => oauthError(Status.BadRequest, "invalid_request", "code・redirect_uri・code_verifier（43〜128文字）を指定してください")
          }

    case req @ POST -> Root / "oauth" / "revoke" =>
      basicClient(req) match
        case None    => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
        case Some(c) => form(req)(f => f.get("token").filter(_.nonEmpty).traverse_(t => oauth.revokeToken(Crypto.sha256(t), c.id)) *> Ok(Json.obj()))
  }
