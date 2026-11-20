package skaka.api

import sbirds.http.{Errors, HttpParts, OidcLoginFlow}
import skaka.core.ErrorCode
import sbirds.shared.AppError
import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.{Crypto, LoginResult}
import skaka.auth.ApiKeys
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** ログイン（OIDC・開発用）・ログアウト・自分の情報・ユーザーの検索. */
final class AuthRoutes(d: ApiRoutes.Deps) extends RouteSupport {

  import d.{oidc, users, rooms, cfg}
  val sessions: SessionAuth = d.sessions

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" => getAuthLogin(req)
    case req @ GET -> Root / "auth" / "callback" => getAuthCallback(req)
    case req @ POST -> Root / "auth" / ("logout" | "dev-login") if !fromScreen(req) => rejectFromOtherSite(req)
    case req @ POST -> Root / "auth" / "logout" => postAuthLogout(req)
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty => postAuthDevLogin(req)
    case GET -> Root / "api" / "client-config" => getClientConfig()
    case req @ GET -> Root / "api" / "me" => getMe(req)
    case req @ PATCH -> Root / "api" / "me" => patchMe(req)
    case req @ PUT -> Root / "api" / "me" / "avatar" => putMeAvatar(req)
    case req @ DELETE -> Root / "api" / "me" / "avatar" => deleteMeAvatar(req)
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" => getUserAvatar(req, id)
    case req @ GET -> Root / "api" / "users" => getUsers(req)
    case req @ GET -> Root / "api" / "tokens" => getTokens(req)
    case req @ POST -> Root / "api" / "tokens" => postTokens(req)
    case req @ DELETE -> Root / "api" / "tokens" / UuidVar(id) => deleteToken(req, id)
  }

  private def getAuthLogin(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.login(flow)(req)

  private def getAuthCallback(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.callback(flow)(req)

  /** OIDC のログインの流れ（sbirds.http.OidcLoginFlow）に差し込む、skaka の部分. */
  private val flow = new OidcLoginFlow.Host[LoginResult] {
    val idp = oidc.map(OidcLoginFlow.Idp(_))

    /**
     * ログインを始めたブラウザの印（state）. IdP から戻ったとき、同じブラウザかを確かめる
     * （攻撃者が途中で止めたコールバックの URL を踏ませて、攻撃者のアカウントでログインさせる「ログイン CSRF」を防ぐ）.
     */
    val cookie = OidcLoginFlow.StateCookie("skaka_login", cfg.cookieSecure, maxAge = 600)
    override def idpErrorParam: Boolean = true

    def returnTo(req: Request[IO]): String = SafePath.orElse(param(req, "returnTo"))
    def saveState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] =
      users.saveLoginState(state, verifier, nonce, returnTo)
    def takeState(state: String): IO[Option[(String, String, String)]] = users.takeLoginState(state)
    def redirect(to: String): IO[Response[IO]] = AuthRoutes.this.redirect(to)

    def withoutIdp(req: Request[IO], back: String): IO[Response[IO]] =
      if cfg.devLogin then AuthRoutes.this.redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
      else err(ErrorCode.OidcNotConfigured)

    def failed(req: Request[IO], f: OidcLoginFlow.Failure): IO[Response[IO]] = {
      import OidcLoginFlow.Failure.*
      f match {
        // IdP の error・error_description は URL で差し替えられるので画面には出さず、ログにだけ残す
        case IdpError(e, description) =>
          page(ErrorCode.IdpRefused, logged = OidcLoginFlow.idpErrorLog(e, description))
        case NotConfigured => err(ErrorCode.OidcNotConfigured)
        case ParamsMissing => err(ErrorCode.LoginCodeStateRequired)
        case BrowserMismatch => page(ErrorCode.LoginBrowserMismatch)
        case Expired => page(ErrorCode.LoginExpired)
        // 画面には、本人に知らせてよい理由（LoginDenied）だけを出す. ほかの詳細はサーバーのログへ
        case Denied(e) => page(ErrorCode.LoginDenied, e.getMessage)
        case Failed(e) => page(ErrorCode.LoginFailed, logged = e.getMessage)
      }
    }

    def loggedIn(req: Request[IO], res: LoginResult, back: String): IO[Response[IO]] = {
      val id = AuthRoutes.verifiedEmailOnly(res)
      users.upsert(id.sub, id.name, id.email, id.picture).flatMap { u =>
        if u.disabled then page(ErrorCode.UserDisabled)
        else AuthRoutes.this.loggedIn(u, Some(res.idToken)).flatMap(c => AuthRoutes.this.redirect(back, Some(c)))
      }
    }
  }

  private def rejectFromOtherSite(req: Request[IO]): IO[Response[IO]] =
    err(ErrorCode.ScreenOnly)

  private def postAuthLogout(req: Request[IO]): IO[Response[IO]] =
    sessions.revoke(req).flatMap {
      (idToken, c) =>
        OidcLoginFlow.logoutRedirect(oidc, idToken, cfg.publicUrl + "/", fallback = "/login?loggedOut=1").flatMap {
          to =>
            Ok(Json.obj("redirect" -> to.asJson)).map(_.addCookie(c))
        }
    }

  private def postAuthDevLogin(req: Request[IO]): IO[Response[IO]] =
    body[Json](req) { j =>
      val c = j.hcursor
      val name = c.get[String]("name").getOrElse("").trim
      if name.isEmpty then err(ErrorCode.NameRequired)
      else
        for {
          u <- users.upsert(
            s"dev:$name",
            name,
            c.get[String]("email").toOption.map(_.trim).filter(_.contains("@")),
            None
          )
          ck <- loggedIn(u, None)
          r <- Ok(
            Json.obj("redirect" -> SafePath.orElse(c.get[String]("returnTo").toOption).asJson)
          ).map(_.addCookie(ck))
        } yield r
    }

  private def getClientConfig(): IO[Response[IO]] =
    Ok(ClientConfigDto(
      cfg.devLogin && oidc.isEmpty,
      oidc.isDefined,
      cfg.version,
      d.skuaka.map(_.base),
      d.recorder.map(_.client.cfg.url),
      d.stt.isDefined,
      d.minutes.map(_.url)
    ))

  private def getMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => sessions.appNames(req).flatMap(n => Ok(u.me.copy(appNames = n))))

  private def patchMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      body[UpdateMeReq](req) { r =>
        val tz = r.timezone.map(_.trim).filter(_.nonEmpty)
        if tz.exists(z => scala.util.Try(java.time.ZoneId.of(z)).isFailure) then
          err(ErrorCode.TimezoneInvalid)
        else if r.name.exists(_.trim.length > 100) then err(ErrorCode.NameTooLong)
        else users.updateMe(u.id, r.name, tz) *> me(u)
      }
    )

  private def putMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req) { u =>
      req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
        if data.length > AvatarImage.MaxBytes then err(ErrorCode.AvatarTooLarge)
        else
          AvatarImage.detect(data).fold(err(ErrorCode.AvatarTypeUnsupported)) { ct =>
            users.saveAvatar(u.id, ct, data) *> me(u)
          }
      }
    }

  private def deleteMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => users.deleteAvatar(u.id) *> me(u))

  private def getUserAvatar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req) { _ =>
      users.avatar(id).flatMap {
        case None => err(ErrorCode.AvatarNotFound)
        case Some((ct, data)) =>
          // JSON の変換が優先されないよう、バイト列のまま返す
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
            headers.`Content-Type`(MediaType.unsafeParse(ct)),
            "Cache-Control" -> "private, max-age=31536000, immutable",
            "X-Content-Type-Options" -> "nosniff",
            "Content-Security-Policy" -> "default-src 'none'"
          ))
      }
    }

  private def getUsers(req: Request[IO]): IO[Response[IO]] =
    authed(req)(_ => users.search(param(req, "q").getOrElse(""), 20).flatMap(Ok(_)))

  private def getTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u => users.tokens(u.id).flatMap(Ok(_)))

  private def postTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      body[ApiTokenReq](req) { r =>
        val name = r.name.trim
        if name.isEmpty || name.length > 100 then err(ErrorCode.ApiTokenNameInvalid)
        else if !TokenScope.all.contains(r.scope) then err(ErrorCode.ApiTokenScopeInvalid)
        else if r.expiresInDays.exists(d => d < 1 || d > 3650) then err(ErrorCode.ApiTokenExpiryInvalid)
        else {
          val (token, prefix) = ApiKeys.generate()
          users.createToken(
            u.id,
            name,
            prefix,
            Crypto.sha256(token),
            r.scope,
            r.expiresInDays.map(d => Instant.now().plusSeconds(d * 86400L))
          )
            .flatMap(t => Created(ApiTokenCreated(t, token)))
        }
      }
    )

  private def deleteToken(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u => users.revokeToken(u.id, id).ifM(NoContent(), err(ErrorCode.ApiTokenNotFound)))

  // ログインまわりの 302 はキャッシュさせない
  override protected def redirect(to: String, cookie: Option[ResponseCookie] = None): IO[Response[IO]] =
    IO.pure(HttpParts.redirect(to, cookie, noStore = true))

  /** 最新の自分の情報（アイコン・名前を変えた直後に画面へ返す）. */
  private def me(u: User): IO[Response[IO]] = users.find(u.id).flatMap(x => Ok(x.getOrElse(u).me))

  /** ログインした人の会議室を用意してから、セッションを発行する. */
  private def loggedIn(u: User, idToken: Option[String]): IO[ResponseCookie] =
    rooms.ensurePersonal(u) *> sessions.issue(u, idToken)
      .flatTap(_ =>
        sbirds.observe.Metrics.count("skaka.logins", "via" -> (if idToken.isDefined then "oidc" else "dev"))
      )

  /**
   * ログインに失敗したときの画面（ブラウザで開くので HTML. コード付きでログに出す）.
   * shown は画面に出す文言（省くとコードの既定の文言）、logged はログだけに出す詳細.
   */
  private def page(code: ErrorCode, shown: String | Null = null, logged: String | Null = null): IO[Response[IO]] = {
    val esc = (s: String) => s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    val title = "ログインできませんでした"
    val msg = Option(shown).getOrElse(code.message)
    Errors.log(AppError(code, Option(logged).orElse(Option(shown)))) *> IO.pure(TextResponse(
      Status.fromInt(code.status).getOrElse(Status.InternalServerError),
      s"""<!doctype html><html lang="ja"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(
          title
        )}</title>
         |<body style="font-family:system-ui,sans-serif;max-width:480px;margin:12vh auto;padding:0 1rem">
         |<h1 style="font-size:1.3rem">${esc(title)}</h1><p>${esc(
          msg
        )}</p><p style="color:#888;font-size:.8rem">${code.code}</p><p><a href="/auth/login">もう一度ログインする</a></p></body></html>""".stripMargin,
      MediaType.text.html
    ))
  }

}

object AuthRoutes {

  /**
   * IdP が確かめていないと明示したメールアドレス（email_verified = false）は使わない（録画の共有の相手をメールで探すため）.
   * 名前がそのメールアドレスから作られていたときは sub にする.
   */
  def verifiedEmailOnly(res: sbirds.auth.LoginResult): sbirds.auth.Identity = {
    val id = res.identity
    val unverified = sbirds.auth.IdToken.decodeClaims(res.idToken).toOption
      .exists(_.hcursor.get[Boolean]("email_verified").contains(false))
    if !unverified then id
    else id.copy(email = None, name = if id.email.contains(id.name) then id.sub else id.name)
  }
}
