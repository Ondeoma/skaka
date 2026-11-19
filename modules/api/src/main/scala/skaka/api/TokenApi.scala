package skaka.api

import cats.effect.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client

/**
 * 利用者の API トークンで呼ぶ兄弟のサービス（skuaka・skakapo）. base は画面から開く URL、internal はこのサーバーから呼ぶ URL.
 * env は URL を設定する環境変数の名前（設定されていないときの文言）、tokenPrefix はそのサービスの API トークンの先頭.
 */
final class TokenApi(
  val service: String,
  val env: String,
  val base: String,
  internal: String,
  val tokenPrefix: String,
  http: Client[IO]) {

  def uri(
    path: String
  ): Either[String, Uri] = Uri.fromString(internal + path).left.map(_.message)

  def call(
    token: String,
    method: Method,
    path: String,
    body: Option[Json] = None
  ): IO[Either[(Int, String), Json]] =
    uri(path) match {
      case Left(e) => IO.pure(Left(500 -> e))
      case Right(u) =>
        val req = Request[IO](method, u).putHeaders(RemoteJson.bearer(token))
        RemoteJson.run(http, body.fold(req)(req.withEntity(_)), service)
    }

  /** 本文をそのまま送る（ファイルのアップロードなど）. */
  def send(
    token: String,
    req: Request[IO]
  ): IO[Either[(Int, String), Json]] =
    RemoteJson.run(http, req.putHeaders(RemoteJson.bearer(token)), service)
}
