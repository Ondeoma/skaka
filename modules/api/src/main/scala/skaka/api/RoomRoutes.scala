package skaka.api

import skaka.core.ErrorCode
import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 会議室（作成・変更・削除・入る）と会議の履歴. */
final class RoomRoutes(d: ApiRoutes.Deps) extends RouteSupport {

  import d.{rooms, meetings, hub, cfg}
  val sessions: SessionAuth = d.sessions

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ------------------------------------------------------------ 会議室

    case req @ GET -> Root / "api" / "rooms" => getRooms(req)
    case req @ POST -> Root / "api" / "rooms" => postRooms(req)
    case req @ GET -> Root / "api" / "rooms" / code => getRoom(req, code)
    case req @ PUT -> Root / "api" / "rooms" / code => putRoom(req, code)
    case req @ DELETE -> Root / "api" / "rooms" / code => deleteRoom(req, code)
    case req @ PUT -> Root / "api" / "rooms" / code / "event" => putRoomEvent(req, code)
    case req @ DELETE -> Root / "api" / "rooms" / code / "recent" => deleteRoomRecent(req, code)
    case req @ GET -> Root / "api" / "rooms" / code / "live" => getRoomLive(req, code)
    case req @ GET -> Root / "api" / "rooms" / code / "entry" => getRoomEntry(req, code)
    case req @ POST -> Root / "api" / "rooms" / code / "join" => postRoomJoin(req, code)
    case req @ GET -> Root / "api" / "meetings" => getMeetings(req)
    case req @ GET -> Root / "api" / "meetings" / UuidVar(id) => getMeeting(req, id)
  }

  private def getRooms(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => rooms.mine(u.id).flatMap(dtos(u.id, _)).flatMap(Ok(_)))

  private def postRooms(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      body[RoomReq](req)(r =>
        valid(validReq(r))(ok =>
          rooms.create(u.id, ok, event = ok.event).flatMap(x => dto(u.id, x, u.brief)).flatMap(Created(_))
        )
      )
    )

  private def getRoom(req: Request[IO], code: String): IO[Response[IO]] =
    authed(req)(u =>
      withRoom(code)((r, o) => dto(u.id, r, o).flatMap(Ok(_)))
    )

  private def putRoom(req: Request[IO], code: String): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, code)((r, o) =>
        body[RoomReq](req)(x =>
          valid(validReq(x)) { ok =>
            val next =
              r.copy(name = ok.name, description = ok.description, guestAccess = ok.guestAccess, lobby = ok.lobby)
            rooms.update(r.id, ok) *> hub.roomChanged(next) *> dto(u.id, next, o).flatMap(Ok(_))
          }
        )
      )
    )

  private def deleteRoom(req: Request[IO], code: String): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, code) {
        (r, _) =>
          if r.personal then err(ErrorCode.PersonalRoomUndeletable)
          else hub.close(r.id, "会議室が削除されました") *> rooms.delete(r.id) *> NoContent()
      }
    )

  private def putRoomEvent(req: Request[IO], code: String): IO[Response[IO]] =
    authed(req)(u =>
      owned(u, code)((r, o) =>
        body[Option[RoomEventDto]](req)(ev =>
          valid(ev.traverse(validEvent)) { ok =>
            val rename = ok.map(_.title.take(100)).filter(t =>
              !r.personal && t != r.name && r.event.exists(_.title.take(100) == r.name)
            )
            val next = r.copy(event = ok, name = rename.getOrElse(r.name))
            rooms.setEvent(r.id, ok, rename) *> dto(u.id, next, o).flatMap(Ok(_))
          }
        )
      )
    )

  private def deleteRoomRecent(req: Request[IO], code: String): IO[Response[IO]] =
    authed(req)(u =>
      withRoom(code)((r, _) => rooms.forget(u.id, r.id) *> NoContent())
    )

  private def getRoomLive(req: Request[IO], code: String): IO[Response[IO]] =
    authed(req)(_ =>
      withRoom(code)((r, _) => liveDto(r).flatMap(Ok(_)))
    )

  private def getRoomEntry(req: Request[IO], code: String): IO[Response[IO]] =
    optional(req)(me =>
      withRoom(code) {
        (r, o) =>
          if me.isEmpty && !r.guestAccess then err(ErrorCode.NotLoggedIn)
          else
            (hub.live(r.id), sessions.appNames(req)).flatMapN((l, names) =>
              Ok(RoomEntryDto(
                r.code,
                r.name,
                if me.isEmpty then o.copy(email = None) else o,
                r.guestAccess,
                l.fold(0)(_.peers.size),
                l.exists(_.locked),
                // 表示名（タブの題名に使う）
                me.map(_.me.copy(appNames = names)),
                r.event
              ))
            )
      }
    )

  private def postRoomJoin(req: Request[IO], code: String): IO[Response[IO]] =
    if sessions.bearer(req).isDefined then err(ErrorCode.JoinFromScreenOnly)
    else if !fromScreen(req) then err(ErrorCode.ScreenOnly)
    else
      optional(req)(me =>
        withRoom(code)((r, _) =>
          body[JoinReq](req) { j =>
            val guest = me.isEmpty
            if guest && !r.guestAccess then err(ErrorCode.NotLoggedIn)
            else
              valid(if guest then Check.name(j.name.getOrElse(""), "名前", 50).map(Some(_)) else Right(None)) {
                name =>
                  for {
                    now <- IO.realTimeInstant
                    _ <- me.traverse_(u => rooms.visited(u.id, r.id))
                    tk <- hub.issue(Ticket(r, me, name, now.plusSeconds(Hub.TicketTtl.toSeconds)))
                    res <- Ok(JoinDto(tk._1, cfg.ice.forPeer(me.fold("guest")(_.id.toString), now), tk._2))
                  } yield res
              }
          }
        )
      )

  private def getMeetings(req: Request[IO]): IO[Response[IO]] =
    authed(req) { u =>
      val limit = limitParam(req)
      param(req, "room") match {
        case Some(c) => withRoom(c)((r, _) =>
            meetings.list(u.id, Some(r.id), param(req, "before").flatMap(instant), limit).flatMap(Ok(_))
          )
        case None => meetings.list(u.id, None, param(req, "before").flatMap(instant), limit).flatMap(Ok(_))
      }
    }

  private def getMeeting(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u =>
      meetings.detail(u.id, id).flatMap(_.fold(err(ErrorCode.MeetingNotFound))(m =>
        (d.recordings.forMeeting(u.id, id), d.minutes.fold(IO.pure(List.empty[MinutesDto]))(_.forMeeting(u.id, id)))
          .flatMapN((rs, ms) => Ok(m.copy(recordings = rs, minutes = ms)))
      ))
    )

  /** 会議室の表示（入っている人数を付ける）. */
  def dtos(me: UUID, rs: List[(Room, UserBrief)]): IO[List[RoomDto]] =
    hub.counts.map(c =>
      rs.map((r, o) => r.dto(o, Some(me), cfg.publicUrl, c.getOrElse(r.id, 0)))
    )

  def dto(me: UUID, r: Room, owner: UserBrief): IO[RoomDto] = dtos(me, List(r -> owner)).map(_.head)

  private def validReq(r: RoomReq): Either[String, RoomReq] =
    (Check.name(r.name, "会議室の名前", 100), Check.text(r.description, "説明", 2000), r.event.traverse(validEvent))
      .mapN((n, ds, ev) => r.copy(name = n, description = ds, event = ev))

  /** 予定の写し（skuaka の予定. 日時は ISO 8601）. */
  private def validEvent(e: RoomEventDto): Either[String, RoomEventDto] =
    (
      Check.name(e.title, "予定のタイトル", 200),
      Either.cond(
        Seq(e.start, e.end).forall(s =>
          scala.util.Try(Instant.parse(s)).isSuccess || s.matches("""\d{4}-\d{2}-\d{2}""")
        ),
        (),
        "予定の日時は ISO 8601 で指定してください"
      )
    ).mapN((t, _) => e.copy(title = t))

  /** コードの会議室（なければ 404）. */
  private def withRoom(code: String)(f: (Room, UserBrief) => IO[Response[IO]]): IO[Response[IO]] =
    RoomCode.parse(code).fold(IO.pure(None))(rooms.byCode).flatMap {
      case Some((r, o)) => f(r, o)
      case None => err(ErrorCode.RoomNotFoundByCode)
    }

  private def owned(u: User, code: String)(f: (Room, UserBrief) => IO[Response[IO]]): IO[Response[IO]] =
    withRoom(code)((r, o) => if r.ownerId == u.id then f(r, o) else err(ErrorCode.RoomOwnerOnly))

  private def liveDto(r: Room): IO[LiveDto] =
    hub.live(r.id).map(l =>
      LiveDto(
        r.code,
        l.toList.flatMap(_.peers),
        l.fold(0)(_.waiting.size),
        l.exists(_.locked),
        l.flatMap(_.startedAt).map(_.toString)
      )
    )

  private def instant(s: String): Option[Instant] = scala.util.Try(Instant.parse(s)).toOption

}
