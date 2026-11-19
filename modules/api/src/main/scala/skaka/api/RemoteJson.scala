package skaka.api

import cats.effect.*
import io.circe.Json
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.Authorization

/**
 * 兄弟のサービス（skuaka・skawau）の JSON の API を呼ぶ. 失敗は (HTTP のステータス, 文言). 文言は相手の {"error": …}、
 * 接続できなければ 502.
 */
object RemoteJson {
  def run(
    http: Client[IO],
    req: Request[IO],
    service: String
  ): IO[Either[(Int, String), Json]] =
    http.run(req).use { r =>
      r.bodyText.compile.string.map { s =>
        val j = if s.isEmpty then Json.Null else io.circe.parser.parse(s).getOrElse(Json.fromString(s.take(300)))
        if r.status.isSuccess then Right(j)
        else Left(r.status.code -> j.hcursor.get[String]("error").getOrElse(r.status.reason))
      }
    }.handleError(e => Left(502 -> s"$service に接続できません: ${e.getMessage}"))

  def bearer(
    token: String
  ): Authorization = Authorization(Credentials.Token(AuthScheme.Bearer, token))
}
