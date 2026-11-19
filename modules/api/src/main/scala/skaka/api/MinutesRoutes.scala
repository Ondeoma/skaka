package skaka.api

import sbirds.shared.AppError
import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skaka.shared.*

import java.util.UUID

/** 議事録・要約のルートと、skakapo の API トークンの登録（skakapo を設定したときだけ）. */
final class MinutesRoutes(
  svc: MinutesService,
  val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = svc.link.routes <+> HttpRoutes.of[IO] {
    case req @ POST -> Root / "api" / "meetings" / UuidVar(id) / "minutes" => postMeetingMinutes(req, id)
    case req @ POST -> Root / "api" / "minutes" / UuidVar(id) / "refresh" => postMinuteRefresh(req, id)
    case req @ POST -> Root / "api" / "minutes" / UuidVar(id) / "file" => postMinuteFile(req, id)
    case req @ DELETE -> Root / "api" / "minutes" / UuidVar(id) => deleteMinute(req, id)
  }

  private def postMeetingMinutes(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u => done(svc.start(u, id))(Created(_)))

  private def postMinuteRefresh(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u => done(svc.refresh(u, id))(_ => Accepted(Json.obj())))

  private def postMinuteFile(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u => done(svc.saveFile(u, id))(_ => Accepted(Json.obj())))

  private def deleteMinute(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u => done(svc.delete(u, id))(_ => NoContent()))

  private def done[A](
    r: IO[Either[AppError, A]]
  )(
    f: A => IO[Response[IO]]
  ): IO[Response[IO]] =
    r.flatMap(_.fold(
      fail,
      f
    ))

}
