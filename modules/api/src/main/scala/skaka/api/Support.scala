package skaka.api

import cats.effect.*
import org.http4s.*
import sbirds.http.RateLimitConfig
import skaka.db.*
import skaka.shared.*

import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(cookieSecure: Boolean,
                           publicUrl: String,
                           sessionTtl: FiniteDuration = 14.days,
                           cookieName: String = "skaka_session",
                           devLogin: Boolean = false,
                           version: String = "0.1.0",
                           ice: IceConfig = IceConfig(),
                           oauthClients: List[OAuthClient] = Nil,
                           rateLimit: RateLimitConfig = RateLimitConfig())

/** WebRTC の ICE サーバーの設定（sbirds-lib）. */
export sbirds.http.IceConfig

/** 誰の要求か. token は API トークンで呼ばれたときの (トークンID, スコープ). */
final case class Principal(user: User, token: Option[(UUID, String)]) {
  def canWrite: Boolean = token.forall(_._2 == TokenScope.Write)
}

/** 文字列の応答（JSON 以外. circe の EntityEncoder に取られないよう明示する）. */
object TextResponse {
  def apply(status: Status, body: String, media: MediaType = MediaType.text.plain): Response[IO] =
    Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder).withContentType(headers.`Content-Type`(
      media,
      Charset.`UTF-8`
    ))
}

/** タイムゾーン（利用者の設定の値. 読めなければ日本時間）. */
object Zones {
  val Default: java.time.ZoneId = java.time.ZoneId.of("Asia/Tokyo")

  def of(tz: String): java.time.ZoneId = scala.util.Try(java.time.ZoneId.of(tz)).getOrElse(Default)
}

/** 入力の確認. */
object Check {
  def text(v: String, what: String, max: Int): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")

  def name(v: String, what: String, max: Int): Either[String, String] = {
    val t = v.trim
    if t.isEmpty then Left(s"$what を入れてください") else text(t, what, max)
  }
}
