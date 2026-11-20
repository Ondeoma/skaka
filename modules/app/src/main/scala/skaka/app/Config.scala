package skaka.app

import sbirds.http.Env.*
import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import skaka.api.*
import sbirds.auth.OidcConfig
import skaka.shared.IceServer

import scala.concurrent.duration.*

/** 設定は環境変数から読む. */
object Config {

  /** OAUTH_CLIENTS: [{"id":"skuaka","secret":"…（16文字以上）","redirectUri":"https://…/api/integrations/skaka/callback","name":"skuaka","scopes":["read","write"]}] */
  private val oauthItemError =
    "OAUTH_CLIENTS の各要素には id・secret（16文字以上）・redirectUri（https://）を指定してください（scopes は read・write）"

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）の URL でしか使わせない（sbirds.http.Env.devLogin）
      devLogin <- devLogin(pub)
      // IP ごとの回数の制限（RATE_LIMIT_AUTH・RATE_LIMIT_PUBLIC・TRUSTED_PROXIES. 0 で制限しない）
      rate <- sbirds.http.RateLimitConfig.fromEnv()
      ice <- opt("ICE_SERVERS").fold(Right(IceConfig().servers))(j =>
        io.circe.parser.decode[List[IceServer]](j).left.map(e =>
          s"ICE_SERVERS を読めません（JSON の配列 [{\"urls\": [...]}]）: ${e.getMessage}"
        )
      )
      clients <-
        OAuthClient.fromEnv(OAuthClient.Rules(itemError = oauthItemError, scopeError = Some(_ => oauthItemError)))
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match {
        case (Some(i), Some(c), Some(s)) =>
          Right(Some(OidcConfig(
            i,
            c,
            s,
            s"$pub/auth/callback",
            opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
            Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")),
            list("OIDC_ALLOWED_GROUPS").toSet
          )))
        case (None, None, None) if devLogin => Right(None)
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      skawau <- (opt("SKAWAU_URL"), opt("SKAWAU_CLIENT_ID"), opt("SKAWAU_CLIENT_SECRET")) match {
        case (Some(u), Some(id), Some(sec)) =>
          Right(Some(SkawauConfig(
            u.stripSuffix("/"),
            opt("SKAWAU_INTERNAL_URL").getOrElse(u).stripSuffix("/"),
            id,
            sec,
            opt("RECORDING_DIR").getOrElse(java.nio.file.Paths.get(
              sys.props("java.io.tmpdir"),
              "skaka-recordings"
            ).toString),
            int("RECORDING_MAX_MB").getOrElse(8192).toLong * 1024 * 1024
          )))
        case (None, None, None) => Right(None)
        case _ => Left("skawau と連携するときは SKAWAU_URL・SKAWAU_CLIENT_ID・SKAWAU_CLIENT_SECRET を設定してください")
      }
    } yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
      oidc = oidc,
      api = ApiConfig(
        cookieSecure = pub.startsWith("https://"),
        publicUrl = pub,
        sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin,
        ice = IceConfig(ice, list("TURN_URLS"), opt("TURN_SECRET"), int("TURN_TTL_HOURS").getOrElse(12).hours),
        oauthClients = clients,
        rateLimit = rate
      ),
      staticDir = opt("STATIC_DIR"),
      skuakaUrl = opt("SKUAKA_URL").map(_.stripSuffix("/")),
      secretKey = opt("SECRET_KEY"),
      skawau = skawau,
      whisper = opt("WHISPER_URL").map(u =>
        WhisperConfig(
          u.stripSuffix("/"),
          opt("WHISPER_MODEL").getOrElse("whisper-1"),
          opt("WHISPER_LANGUAGE").getOrElse("ja"),
          opt("WHISPER_API_KEY"),
          int("WHISPER_CONCURRENCY").getOrElse(2).max(1).min(32),
          opt("WHISPER_PROMPT")
        )
      ),
      skakapo = opt("SKAKAPO_URL").map(u =>
        SkakapoConfig(
          u.stripSuffix("/"),
          opt("SKAKAPO_INTERNAL_URL").getOrElse(u).stripSuffix("/"),
          opt("SKAKAPO_REQUEST_TYPE").getOrElse("other")
        )
      )
    )).left.map(new IllegalArgumentException(_))
  )
}
