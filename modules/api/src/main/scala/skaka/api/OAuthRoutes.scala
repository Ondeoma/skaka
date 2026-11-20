package skaka.api

import cats.effect.*
import org.http4s.*
import sbirds.auth.Crypto
import sbirds.oauth.{OAuthCode, OAuthProvider}
import skaka.auth.ApiKeys
import skaka.db.*
import skaka.shared.*

import java.util.UUID

/**
 * 認可を求める外部のサービス（OAUTH_CLIENTS で登録する）. redirectUri は完全一致で照合する.
 * scopes はそのサービスが求めてよいスコープ（read・write）.
 */
type OAuthClient = sbirds.oauth.OAuthClient
val OAuthClient = sbirds.oauth.OAuthClient

/**
 * 外部のサービス（skuaka）への認可（OAuth 2.0 の認可コード + PKCE. 流れは sbirds.oauth.OAuthProvider）. 兄弟の skawau と同じ形.
 * 発行するトークンは、そのユーザーの API トークン（「<サービス名> 連携」. 設定の API トークンの一覧から失効できる）.
 * 取り決めは docs/skuaka-integration.md.
 */
final class OAuthRoutes(oauth: OAuthStore, clients: List[OAuthClient], sessions: SessionAuth, cfg: ApiConfig) {

  private val host = new OAuthProvider.Host[Principal] {
    val settings = OAuthProvider.Settings("skaka", cfg.cookieSecure, unreadableForm = "リクエストの内容が正しくありません")
    val codes = oauth

    def client(id: String): IO[Option[OAuthClient]] = IO.pure(clients.find(_.id == id))

    // 許可は画面のログイン（Cookie のセッション）だけ. API トークンでは許可させない
    def browserSession(req: Request[IO]): IO[Option[Principal]] =
      sessions.fromRequest(req).map(_.filter(_.token.isEmpty))

    def userId(p: Principal): UUID = p.user.id

    override def scopes(c: OAuthClient, requested: Option[String]): Option[List[String]] =
      OAuthProvider.requestedScopes(c.scopes, requested, List(TokenScope.Read))

    /** 発行するトークンは1つのスコープなので、write を含めば write. */
    override def codeScope(c: OAuthClient, scopes: List[String]): String =
      if scopes.contains(TokenScope.Write) then TokenScope.Write else TokenScope.Read

    def consent(req: Request[IO], c: OAuthClient, p: Principal, scopes: List[String]): IO[OAuthProvider.Consent] = {
      val name = OAuthProvider.escape(c.name)
      val write = scopes.contains(TokenScope.Write)
      val what = if write then "skaka の会議室を作成・変更し、会議の記録を読み取り" else "skaka の会議室と会議の記録を読み取り"
      IO.pure(OAuthProvider.Consent(
        s"$name との連携",
        s"""<h1>$name との連携</h1>
           |<p><b>$name</b> が、あなた（${OAuthProvider.escape(p.user.name)}）の権限で${what}ます。</p>
           |<ul class="muted">${
            if write then "<li>予定に Web 会議（skaka の会議室）を付けるのに使います</li>" else ""
          }
           |<li>会議に入ること・会議の中身を見ることはできません</li>
           |<li>設定の「API トークンと MCP」から、いつでも連携をやめられます</li></ul>""".stripMargin
      ))
    }

    /** 画面の枠（ロゴの画像を出すので、lib の既定の枠ではなく自前. CSP に img-src 'self' を足す）. */
    override def page(title: String, body: String, status: Status): IO[Response[IO]] = {
      val html =
        s"""<!DOCTYPE html><html lang="ja"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1">
           |<title>${OAuthProvider.escape(title)} | skaka</title><link rel="icon" href="/favicon.svg"><style>
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
      IO.pure(OAuthProvider.htmlResponse(
        html,
        status,
        "default-src 'none'; style-src 'unsafe-inline'; img-src 'self'; frame-ancestors 'none'"
      ))
    }

    def issue(req: Request[IO], c: OAuthClient, oc: OAuthCode): IO[OAuthProvider.Token] = {
      val (token, prefix) = ApiKeys.generate()
      oauth.replaceToken(oc.userId, c.id, s"${c.name} 連携", prefix, Crypto.sha256(token), oc.scope)
        .as(OAuthProvider.Token(token, oc.scope))
    }

    def revoke(req: Request[IO], c: OAuthClient, token: String): IO[Unit] =
      oauth.revokeToken(Crypto.sha256(token), c.id)
  }

  val routes: HttpRoutes[IO] = new OAuthProvider(host).routes
}
