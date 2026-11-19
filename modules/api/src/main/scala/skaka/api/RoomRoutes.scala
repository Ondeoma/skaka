package skaka.api

import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 会議室（作成・変更・削除・入る）と会議の履歴. */
final class RoomRoutes(d: ApiRoutes.Deps) extends RouteSupport:
  import d.{rooms, meetings, hub, cfg}
  val sessions: SessionAuth = d.sessions

  /** 会議室の表示（入っている人数を付ける）. */
  def dtos(me: UUID, rs: List[(Room, UserBrief)]): IO[List[RoomDto]] =
    hub.counts.map(c => rs.map((r, o) => r.dto(o, Some(me), cfg.publicUrl, c.getOrElse(r.id, 0))))

  def dto(me: UUID, r: Room, owner: UserBrief): IO[RoomDto] = dtos(me, List(r -> owner)).map(_.head)

  private def validReq(r: RoomReq): Either[String, RoomReq] =
    (Check.name(r.name, "会議室の名前", 100), Check.text(r.description, "説明", 2000), r.event.traverse(validEvent))
      .mapN((n, ds, ev) => r.copy(name = n, description = ds, event = ev))

  /** 予定の写し（skuaka の予定. 日時は ISO 8601）. */
  private def validEvent(e: RoomEventDto): Either[String, RoomEventDto] =
    (Check.name(e.title, "予定のタイトル", 200), Either.cond(Seq(e.start, e.end).forall(s => scala.util.Try(Instant.parse(s)).isSuccess || s.matches("""\d{4}-\d{2}-\d{2}""")),
      (), "予定の日時は ISO 8601 で指定してください")).mapN((t, _) => e.copy(title = t))

  /** コードの会議室（なければ 404）. */
  private def withRoom(code: String)(f: (Room, UserBrief) => IO[Response[IO]]): IO[Response[IO]] =
    RoomCode.parse(code).fold(IO.pure(None))(rooms.byCode).flatMap {
      case Some((r, o)) => f(r, o)
      case None         => err(Status.NotFound, "会議室が見つかりません（URL・コードを確かめてください）")
    }

  private def owned(u: User, code: String)(f: (Room, UserBrief) => IO[Response[IO]]): IO[Response[IO]] =
    withRoom(code)((r, o) => if r.ownerId == u.id then f(r, o) else err(Status.Forbidden, "会議室の持ち主だけができます"))

  private def liveDto(r: Room): IO[LiveDto] =
    hub.live(r.id).map(l => LiveDto(r.code, l.toList.flatMap(_.peers), l.fold(0)(_.waiting.size), l.exists(_.locked), l.flatMap(_.startedAt).map(_.toString)))

  private def instant(s: String): Option[Instant] = scala.util.Try(Instant.parse(s)).toOption

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ------------------------------------------------------------ 会議室

    case req @ GET -> Root / "api" / "rooms" =>
      authed(req)(u => rooms.mine(u.id).flatMap(dtos(u.id, _)).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "rooms" =>
      authed(req)(u => body[RoomReq](req)(r => valid(validReq(r))(ok =>
        rooms.create(u.id, ok, event = ok.event).flatMap(x => dto(u.id, x, u.brief)).flatMap(Created(_)))))

    case req @ GET -> Root / "api" / "rooms" / code =>
      authed(req)(u => withRoom(code)((r, o) => dto(u.id, r, o).flatMap(Ok(_))))

    case req @ PUT -> Root / "api" / "rooms" / code =>
      authed(req)(u => owned(u, code)((r, o) => body[RoomReq](req)(x => valid(validReq(x)) { ok =>
        val next = r.copy(name = ok.name, description = ok.description, guestAccess = ok.guestAccess, lobby = ok.lobby)
        rooms.update(r.id, ok) *> hub.roomChanged(next) *> dto(u.id, next, o).flatMap(Ok(_))
      })))

    case req @ DELETE -> Root / "api" / "rooms" / code =>
      authed(req)(u => owned(u, code) { (r, _) =>
        if r.personal then err(Status.BadRequest, "自分の会議室は削除できません")
        else hub.close(r.id, "会議室が削除されました") *> rooms.delete(r.id) *> NoContent()
      })

    // 結び付ける予定の写しを変える（skuaka で予定の日時・タイトルを変えたとき. null で外す）.
    // 会議室の名前が前の予定のタイトルのまま（手で変えていない）なら、名前も新しいタイトルに合わせる
    case req @ PUT -> Root / "api" / "rooms" / code / "event" =>
      authed(req)(u => owned(u, code)((r, o) => body[Option[RoomEventDto]](req)(ev => valid(ev.traverse(validEvent)) { ok =>
        val rename = ok.map(_.title.take(100)).filter(t => !r.personal && t != r.name && r.event.exists(_.title.take(100) == r.name))
        val next   = r.copy(event = ok, name = rename.getOrElse(r.name))
        rooms.setEvent(r.id, ok, rename) *> dto(u.id, next, o).flatMap(Ok(_))
      })))

    // 最近の会議室から外す
    case req @ DELETE -> Root / "api" / "rooms" / code / "recent" =>
      authed(req)(u => withRoom(code)((r, _) => rooms.forget(u.id, r.id) *> NoContent()))

    case req @ GET -> Root / "api" / "rooms" / code / "live" =>
      authed(req)(_ => withRoom(code)((r, _) => liveDto(r).flatMap(Ok(_))))

    // 入る前の画面（ログインしていない人にも. ゲストを受け付けない会議室では、ログインしていない人には名前を見せない）
    case req @ GET -> Root / "api" / "rooms" / code / "entry" =>
      optional(req)(me => withRoom(code) { (r, o) =>
        if me.isEmpty && !r.guestAccess then err(Status.Unauthorized, "ログインしてください")
        else hub.live(r.id).flatMap(l => Ok(RoomEntryDto(r.code, r.name, if me.isEmpty then o.copy(email = None) else o, r.guestAccess, l.fold(0)(_.peers.size), l.exists(_.locked),
          me.map(_.me), r.event)))
      })

    // 入る: 切符を発行する（WebSocket /ws/{ticket} で使う）
    case req @ POST -> Root / "api" / "rooms" / code / "join" =>
      if sessions.bearer(req).isDefined then err(Status.Forbidden, "会議には画面から入ってください")
      else if !fromScreen(req) then err(Status.Forbidden, "画面以外からの要求は受け付けません")
      else optional(req)(me => withRoom(code)((r, _) => body[JoinReq](req) { j =>
        val guest = me.isEmpty
        if guest && !r.guestAccess then err(Status.Unauthorized, "ログインしてください")
        else
          valid(if guest then Check.name(j.name.getOrElse(""), "名前", 50).map(Some(_)) else Right(None)) { name =>
            for
              now <- IO.realTimeInstant
              _   <- me.traverse_(u => rooms.visited(u.id, r.id))
              tk  <- hub.issue(Ticket(r, me, name, now.plusSeconds(Hub.TicketTtl.toSeconds)))
              res <- Ok(JoinDto(tk._1, cfg.ice.forPeer(me.fold("guest")(_.id.toString), now), tk._2))
            yield res
          }
      }))

    // ------------------------------------------------------------ 会議の履歴

    case req @ GET -> Root / "api" / "meetings" =>
      authed(req) { u =>
        val limit = param(req, "limit").flatMap(_.toIntOption).map(_.max(1).min(200)).getOrElse(50)
        param(req, "room") match
          case Some(c) => withRoom(c)((r, _) => meetings.list(u.id, Some(r.id), param(req, "before").flatMap(instant), limit).flatMap(Ok(_)))
          case None    => meetings.list(u.id, None, param(req, "before").flatMap(instant), limit).flatMap(Ok(_))
      }

    case req @ GET -> Root / "api" / "meetings" / UuidVar(id) =>
      authed(req)(u => meetings.detail(u.id, id).flatMap(_.fold(err(Status.NotFound, "会議が見つかりません"))(m =>
        (d.recordings.forMeeting(u.id, id), d.minutes.fold(IO.pure(List.empty[MinutesDto]))(_.forMeeting(u.id, id)))
          .flatMapN((rs, ms) => Ok(m.copy(recordings = rs, minutes = ms))))))
  }

/** 会議中のやり取り（WebSocket）. 切符は POST /api/rooms/{code}/join で発行する. */
object SignalRoutes:
  /** 1つのメッセージの上限（SDP は数 KB. それより大きいものは読まない）. */
  val MaxFrame = 64 * 1024

  def apply(hub: Hub, ws: WebSocketBuilder[IO]): HttpRoutes[IO] =
    val dsl = org.http4s.dsl.io
    import dsl.*
    HttpRoutes.of[IO] {
      case GET -> Root / "ws" / ticket =>
        hub.connect(ticket).flatMap {
          case None => Forbidden(ErrorDto("入るための切符が使えません（期限切れ）。入り直してください"))
          case Some((peer, q)) =>
            Flood.create.flatMap { flood =>
              // 接続中の数（メトリクス）は、送る側のストリームが続いている間
              val conn = Stream.bracket(skaka.observe.Metrics.upDown("skaka.ws.connections", 1))(_ => skaka.observe.Metrics.upDown("skaka.ws.connections", -1))
              val send = conn >> (Stream.fromQueueNoneTerminated(q).map(m => WebSocketFrame.Text(m.asJson.noSpaces)) ++ Stream.emit(WebSocketFrame.Close()))
              val receive: fs2.Pipe[IO, WebSocketFrame, Unit] = _.collect { case WebSocketFrame.Text(t, _) if t.length <= MaxFrame => t }
                .evalMap(t => decode[ClientMsg](t).fold(_ => IO.unit, m => flood.allow(m).ifM(hub.receive(peer, m), IO.unit)))
                .onFinalize(hub.disconnect(peer))
              ws.withOnClose(hub.disconnect(peer)).build(send, receive)
            }
        }
    }

/**
 * 1つの接続から受け付けるメッセージの数の上限（10秒ごと. 超えた分は捨てる）. 全員に送られるチャット・リアクションは少なめに.
 * 中継（SDP・ICE）は、大人数で入った直後にまとめて届くので多めにしてある.
 */
final class Flood private (ref: Ref[IO, (Long, Int, Int)]):
  import Flood.*

  def allow(m: ClientMsg): IO[Boolean] = IO.monotonic.flatMap { t =>
    val now   = t.toMillis
    val broad = m match
      case _: ClientMsg.Chat | _: ClientMsg.React => true
      case _                                      => false
    ref.modify { (start, all, chat) =>
      val (s, a, c) = if now - start >= WindowMillis then (now, 0, 0) else (start, all, chat)
      val ok        = a < MaxMessages && (!broad || c < MaxBroadcasts)
      ((s, a + 1, if broad then c + 1 else c), ok)
    }
  }

object Flood:
  val WindowMillis  = 10000L
  val MaxMessages   = 3000
  val MaxBroadcasts = 40

  def create: IO[Flood] = IO.monotonic.flatMap(t => Ref.of[IO, (Long, Int, Int)]((t.toMillis, 0, 0))).map(new Flood(_))
