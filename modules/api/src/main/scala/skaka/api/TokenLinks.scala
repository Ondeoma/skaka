package skaka.api

import cats.data.EitherT
import cats.effect.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import skaka.auth.SecretBox
import skaka.db.*
import skaka.shared.*

/**
 * 利用者の API トークンで呼ぶ兄弟のサービス（skuaka・skakapo）. base は画面から開く URL、internal はこのサーバーから呼ぶ URL.
 * env は URL を設定する環境変数の名前（設定されていないときの文言）、tokenPrefix はそのサービスの API トークンの先頭.
 */
final class TokenApi(val service: String, val env: String, val base: String, internal: String, val tokenPrefix: String, http: Client[IO]):

  def uri(path: String): Either[String, Uri] = Uri.fromString(internal + path).left.map(_.message)

  def call(token: String, method: Method, path: String, body: Option[Json] = None): IO[Either[(Int, String), Json]] =
    uri(path) match
      case Left(e) => IO.pure(Left(500 -> e))
      case Right(u) =>
        val req = Request[IO](method, u).putHeaders(RemoteJson.bearer(token))
        RemoteJson.run(http, body.fold(req)(req.withEntity(_)), service)

  /** 本文をそのまま送る（ファイルのアップロードなど）. */
  def send(token: String, req: Request[IO]): IO[Either[(Int, String), Json]] =
    RemoteJson.run(http, req.putHeaders(RemoteJson.bearer(token)), service)

type SkuakaClient = TokenApi

object SkuakaClient:
  def apply(base: String, http: Client[IO]): TokenApi = new TokenApi("skuaka", "SKUAKA_URL", base, base, "skuaka_", http)

/**
 * API トークンでの連携（skuaka・skakapo で共通）: トークンの登録・確かめ・解除（GET・PUT・DELETE /api/{service}）と、
 * 登録したトークンでそのサービスを呼ぶ補助.
 */
final class TokenLinks(api: Option[TokenApi], links: LinkStore, box: SecretBox, val sessions: SessionAuth, service: String) extends RouteSupport:

  type Res[A] = EitherT[IO, (Status, String), A]

  def fail[A](s: Status, m: String): Res[A] = EitherT.leftT(s -> m)

  def run(r: Res[Response[IO]]): IO[Response[IO]] = r.value.flatMap(_.fold((s, m) => err(s, m), IO.pure))

  def available: Boolean = api.isDefined

  /** 連携の設定とトークン（なければ 409）. */
  def linked(u: User): Res[(TokenApi, String, TokenLink)] = linkedFor(u.id)

  def linkedFor(userId: java.util.UUID): Res[(TokenApi, String, TokenLink)] =
    api match
      case None => fail(Status.ServiceUnavailable, s"$service との連携が設定されていません")
      case Some(c) =>
        EitherT(links.get(userId).map {
          case None => Left(Status.Conflict -> s"$service と連携していません。設定の画面で $service の API トークンを登録してください")
          case Some(l) =>
            box.open(l.tokenEnc).map(t => (c, t, l))
              .toRight(Status.Conflict -> s"$service の API トークンを読めません（サーバーの SECRET_KEY が変わりました）。設定の画面で登録し直してください")
        })

  /** 読み書きのトークン（参照のみなら 403. what は断るときの文言の「〜を」）. */
  def writable(u: User, what: String): Res[(TokenApi, String)] =
    linked(u).flatMap((c, t, l) =>
      if l.canWrite then EitherT.rightT((c, t)) else fail(Status.Forbidden, s"$service の API トークンが参照のみのため、${what}変えられません"))

  /** 呼んだ結果を、こちらの応答のステータスにする（401 なら登録し直しを促す）. */
  def result(r: IO[Either[(Int, String), Json]]): Res[Json] =
    EitherT(r.map(_.left.map {
      case (401, _) => Status.Conflict -> s"$service の API トークンが使えません（失効・期限切れ）。設定の画面で登録し直してください"
      case (403, m) => Status.Forbidden -> s"$service: $m"
      case (404, m) => Status.NotFound -> s"$service: $m"
      case (_, m)   => Status.BadGateway -> s"$service: $m"
    }))

  def call(c: TokenApi, token: String, method: Method, path: String, body: Option[Json] = None): Res[Json] =
    result(c.call(token, method, path, body))

  /** API トークンを確かめる: 持ち主の名前と、書けるか（参照のみのトークンは PATCH /api/me が 403）. */
  private def verify(c: TokenApi, token: String): IO[Either[String, (String, Boolean)]] =
    c.call(token, Method.GET, "/api/me").flatMap {
      case Left((401, _)) => IO.pure(Left(s"$service の API トークンが正しくないか、期限が切れています"))
      case Left((_, m))   => IO.pure(Left(s"$service: $m"))
      case Right(me) =>
        c.call(token, Method.PATCH, "/api/me", Some(Json.obj())).map(w => Right(me.hcursor.get[String]("name").getOrElse("") -> !w.left.exists(_._1 == 403)))
    }

  def dto(u: User): IO[TokenLinkDto] =
    links.get(u.id).map(l => TokenLinkDto(api.isDefined, api.map(_.base), l.isDefined, l.map(_.account), l.map(_.prefix), l.exists(_.canWrite)))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / `service` => authed(req)(u => dto(u).flatMap(Ok(_)))

    case req @ PUT -> Root / "api" / `service` =>
      interactive(req)(u => body[TokenLinkReq](req) { r =>
        val token = r.token.trim
        api match
          case None => err(Status.ServiceUnavailable, s"$service との連携が設定されていません")
          case Some(c) if !token.startsWith(c.tokenPrefix) => err(Status.BadRequest, s"$service の API トークン（${c.tokenPrefix} で始まる値）を入れてください")
          case Some(c) =>
            verify(c, token).flatMap {
              case Left(m) => err(Status.BadRequest, m)
              case Right((name, write)) =>
                links.set(u.id, TokenLink(box.seal(token), token.take(c.tokenPrefix.length + 6), name, write)) *> dto(u).flatMap(Ok(_))
            }
      })

    case req @ DELETE -> Root / "api" / `service` => interactive(req)(u => links.delete(u.id) *> NoContent())
  }
