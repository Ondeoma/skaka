package skaka.api

import skaka.core.ErrorCode
import sbirds.shared.AppError
import cats.data.EitherT
import cats.effect.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import skaka.auth.SecretBox
import skaka.db.*
import skaka.shared.*

type SkuakaClient = TokenApi

object SkuakaClient {
  def apply(
    base: String,
    http: Client[IO]
  ): TokenApi = new TokenApi("skuaka", "SKUAKA_URL", base, base, "skuaka_", http)
}

/**
 * API トークンでの連携（skuaka・skakapo で共通）: トークンの登録・確かめ・解除（GET・PUT・DELETE /api/{service}）と、
 * 登録したトークンでそのサービスを呼ぶ補助.
 */
final class TokenLinks(
  api: Option[TokenApi],
  links: LinkStore,
  box: SecretBox,
  val sessions: SessionAuth,
  service: String) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / `service` => getLink(req)
    case req @ PUT -> Root / "api" / `service` => putLink(req)
    case req @ DELETE -> Root / "api" / `service` => deleteLink(req)
  }

  private def getLink(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u => dto(u).flatMap(Ok(_)))

  private def putLink(
    req: Request[IO]
  ): IO[Response[IO]] =
    interactive(req)(u =>
      body[TokenLinkReq](req) { r =>
        val token = r.token.trim
        api match {
          case None => err(ErrorCode.LinkNotConfigured, s"$service との連携が設定されていません")
          case Some(c) if !token.startsWith(c.tokenPrefix) =>
            err(ErrorCode.LinkTokenFormatInvalid, s"$service の API トークン（${c.tokenPrefix} で始まる値）を入れてください")
          case Some(c) =>
            verify(c, token).flatMap {
              case Left(m) => err(ErrorCode.LinkTokenInvalid, m)
              case Right((name, write)) =>
                links.set(u.id, TokenLink(box.seal(token), token.take(c.tokenPrefix.length + 6), name, write)) *>
                  dto(u).flatMap(Ok(_))
            }
        }
      }
    )

  private def deleteLink(
    req: Request[IO]
  ): IO[Response[IO]] =
    interactive(req)(u => links.delete(u.id) *> NoContent())

  type Res[A] = EitherT[IO, AppError, A]

  def reject[A](
    e: AppError
  ): Res[A] = EitherT.leftT(e)

  def run(
    r: Res[Response[IO]]
  ): IO[Response[IO]] = r.value.flatMap(_.fold(
    fail,
    IO.pure
  ))

  def available: Boolean = api.isDefined

  /** 連携の設定とトークン（なければ 409）. */
  def linked(
    u: User
  ): Res[(TokenApi, String, TokenLink)] = linkedFor(u.id)

  def linkedFor(
    userId: java.util.UUID
  ): Res[(TokenApi, String, TokenLink)] =
    api match {
      case None => reject(AppError(ErrorCode.LinkNotConfigured, s"$service との連携が設定されていません"))
      case Some(c) =>
        EitherT(links.get(userId).map {
          case None =>
            Left(AppError(ErrorCode.LinkNotLinked, s"$service と連携していません。設定の画面で $service の API トークンを登録してください"))
          case Some(l) =>
            box.open(l.tokenEnc).map(t => (c, t, l))
              .toRight(AppError(
                ErrorCode.LinkTokenUnreadable,
                s"$service の API トークンを読めません（サーバーの SECRET_KEY が変わりました）。設定の画面で登録し直してください"
              ))
        })
    }

  /** 読み書きのトークン（参照のみなら 403. what は断るときの文言の「〜を」）. */
  def writable(
    u: User,
    what: String
  ): Res[(TokenApi, String)] =
    linked(u).flatMap(
      (
        c,
        t,
        l
      ) =>
        if l.canWrite then EitherT.rightT((c, t))
        else reject(AppError(ErrorCode.LinkTokenReadOnly, s"$service の API トークンが参照のみのため、${what}変えられません"))
    )

  /** 呼んだ結果を、こちらの応答のステータスにする（401 なら登録し直しを促す）. */
  def result(
    r: IO[Either[(Int, String), Json]]
  ): Res[Json] =
    EitherT(r.map(_.left.map {
      case (401, _) => AppError(ErrorCode.LinkTokenRejected, s"$service の API トークンが使えません（失効・期限切れ）。設定の画面で登録し直してください")
      case (403, m) => AppError(ErrorCode.LinkForbidden, s"$service: $m")
      case (404, m) => AppError(ErrorCode.LinkNotFound, s"$service: $m")
      case (_, m) => AppError(ErrorCode.LinkBadGateway, s"$service: $m")
    }))

  def call(
    c: TokenApi,
    token: String,
    method: Method,
    path: String,
    body: Option[Json] = None
  ): Res[Json] =
    result(c.call(token, method, path, body))

  /** API トークンを確かめる: 持ち主の名前と、書けるか（参照のみのトークンは PATCH /api/me が 403）. */
  private def verify(
    c: TokenApi,
    token: String
  ): IO[Either[String, (String, Boolean)]] =
    c.call(token, Method.GET, "/api/me").flatMap {
      case Left((401, _)) => IO.pure(Left(s"$service の API トークンが正しくないか、期限が切れています"))
      case Left((_, m)) => IO.pure(Left(s"$service: $m"))
      case Right(me) =>
        c.call(token, Method.PATCH, "/api/me", Some(Json.obj())).map(w =>
          Right(me.hcursor.get[String]("name").getOrElse("") -> !w.left.exists(_._1 == 403))
        )
    }

  def dto(
    u: User
  ): IO[TokenLinkDto] =
    links.get(u.id).map(l =>
      TokenLinkDto(api.isDefined, api.map(_.base), l.isDefined, l.map(_.account), l.map(_.prefix), l.exists(_.canWrite))
    )

}
