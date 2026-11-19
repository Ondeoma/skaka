package skaka.api

import cats.effect.*
import fs2.Stream
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{`Content-Length`, `Content-Type`, Authorization}

import java.util.UUID

/** skawau の API. 失敗は (HTTP のステータス, 文言). */
final class SkawauClient(val cfg: SkawauConfig, http: Client[IO], upload: Client[IO]) {

  private def uri(path: String): Uri = Uri.unsafeFromString(cfg.internalUrl + path)
  private def basic = Authorization(BasicCredentials(cfg.clientId, cfg.clientSecret))
  private def bearer(t: String) = RemoteJson.bearer(t)

  private def send(c: Client[IO], req: Request[IO]): IO[Either[(Int, String), Json]] = RemoteJson.run(c, req, "skawau")

  /** 認可コードをトークンに替える. (トークン, スコープ). */
  def exchange(code: String, redirect: String, verifier: String): IO[Either[String, (String, List[String])]] = {
    val form = UrlForm(
      "grant_type" -> "authorization_code",
      "code" -> code,
      "redirect_uri" -> redirect,
      "code_verifier" -> verifier
    )
    send(http, Request[IO](Method.POST, uri("/oauth/token")).withEntity(form).putHeaders(basic)).map {
      case Left((_, m)) => Left(m)
      case Right(j) =>
        j.hcursor.get[String]("access_token").toOption.toRight("skawau の応答を読めません")
          .map(_ -> j.hcursor.get[String]("scope").getOrElse("read").split(' ').toList)
    }
  }

  def revoke(token: String): IO[Unit] =
    send(
      http,
      Request[IO](Method.POST, uri("/oauth/revoke")).withEntity(UrlForm("token" -> token)).putHeaders(basic)
    ).void

  def call(token: String, method: Method, path: String, body: Option[Json] = None): IO[Either[(Int, String), Json]] = {
    val req = Request[IO](method, uri(path)).putHeaders(bearer(token))
    send(http, body.fold(req)(req.withEntity(_)))
  }

  /** ファイルの中身を書き換える（skawau に新しい版が残る）. */
  def write(token: String, fileId: UUID, data: Array[Byte], mime: String): IO[Either[(Int, String), Json]] = {
    val ct = MediaType.parse(mime).toOption.getOrElse(MediaType.application.`octet-stream`)
    send(
      http,
      Request[IO](Method.PUT, uri(s"/api/files/$fileId/content")).withEntity(data)(using EntityEncoder.byteArrayEncoder)
        .withContentType(`Content-Type`(ct)).putHeaders(bearer(token))
    )
  }

  /** ファイルを置く（folder の下の path のフォルダに. 同じ名前があれば名前を変えて）. 応答は skawau の NodeDto. */
  def put(token: String,
          folder: UUID,
          path: String,
          name: String,
          body: Stream[IO, Byte],
          length: Long,
          mime: String): IO[Either[(Int, String), Json]] = {
    val u =
      uri(s"/api/files/$folder/upload").withQueryParams(Map("path" -> path, "name" -> name, "conflict" -> "rename"))
    val ct = MediaType.parse(mime).toOption.getOrElse(MediaType.application.`octet-stream`)
    send(
      upload,
      Request[IO](
        Method.PUT,
        u
      ).withBodyStream(body).putHeaders(bearer(token), `Content-Length`(length), `Content-Type`(ct))
    )
  }
}
