package skaka.api

import cats.effect.*
import org.http4s.*
import skaka.auth.TurnCredentials
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(cookieSecure: Boolean,
                           publicUrl: String,
                           sessionTtl: FiniteDuration = 14.days,
                           cookieName: String = "skaka_session",
                           devLogin: Boolean = false,
                           version: String = "0.1.0",
                           ice: IceConfig = IceConfig(),
                           oauthClients: List[OAuthClient] = Nil)

/**
 * WebRTC の ICE サーバー. servers は STUN（と固定の TURN）. turnUrls と turnSecret があれば、
 * coturn の use-auth-secret（TURN REST API）の一時的な認証情報（ttl の間有効）を参加者ごとに作る.
 */
final case class IceConfig(servers: List[IceServer] = List(IceServer(List("stun:stun.l.google.com:19302"))),
                           turnUrls: List[String] = Nil,
                           turnSecret: Option[String] = None,
                           ttl: FiniteDuration = 12.hours) {
  def forPeer(name: String, now: Instant): List[IceServer] =
    servers ++ turnSecret.filter(_ => turnUrls.nonEmpty).toList.map { secret =>
      val user = s"${now.getEpochSecond + ttl.toSeconds}:$name"
      IceServer(turnUrls, Some(user), Some(TurnCredentials.password(secret, user)))
    }
}

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

/** 入力の確認. */
object Check {
  def text(v: String, what: String, max: Int): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")

  def name(v: String, what: String, max: Int): Either[String, String] = {
    val t = v.trim
    if t.isEmpty then Left(s"$what を入れてください") else text(t, what, max)
  }
}
