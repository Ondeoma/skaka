package skaka.api

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import io.circe.{ACursor, Json}
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skaka.core.RoomLinks
import skaka.db.*
import skaka.shared.*

import java.time.{Instant, LocalDate, ZoneId}
import java.util.UUID
import scala.util.Try

/**
 * 兄弟のサービス（skuaka・skawau）の JSON の API を呼ぶ. 失敗は (HTTP のステータス, 文言). 文言は相手の {"error": …}、
 * 接続できなければ 502.
 */
object RemoteJson:
  def run(http: Client[IO], req: Request[IO], service: String): IO[Either[(Int, String), Json]] =
    http.run(req).use { r =>
      r.bodyText.compile.string.map { s =>
        val j = if s.isEmpty then Json.Null else io.circe.parser.parse(s).getOrElse(Json.fromString(s.take(300)))
        if r.status.isSuccess then Right(j) else Left(r.status.code -> j.hcursor.get[String]("error").getOrElse(r.status.reason))
      }
    }.handleError(e => Left(502 -> s"$service に接続できません: ${e.getMessage}"))

  def bearer(token: String): Authorization = Authorization(Credentials.Token(AuthScheme.Bearer, token))

/** 予定（skuaka の EventDto）から、変更の要求（EventReq）を作る. 知らない項目はそのまま渡す. */
object SkuakaEvents:
  def toReq(e: Json, location: String): Json =
    val c = e.hcursor
    def f(k: String): Json = c.downField(k).focus.getOrElse(Json.Null)
    Json.obj(
      "calendarId" -> f("calendarId"), "title" -> f("title"), "description" -> f("description"), "location" -> location.asJson,
      "allDay" -> f("allDay"), "start" -> f("start"), "end" -> f("end"), "timezone" -> f("timezone"), "color" -> f("color"),
      "recurrence" -> f("recurrence"), "reminders" -> f("reminders"), "version" -> f("version"),
      "attendees" -> c.downField("attendees").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.downField("user").get[UUID]("id").toOption).asJson
    )

  def roomEvent(e: ACursor): Option[RoomEventDto] =
    (e.get[UUID]("id"), e.get[String]("title"), e.get[String]("start"), e.get[String]("end"), e.get[Boolean]("allDay")).mapN(RoomEventDto.apply).toOption

/** skuaka（カレンダー）との連携: API トークンの登録、予定の一覧、予定に会議室を付ける、会議の予定を立てる. */
final class SkuakaRoutes(d: ApiRoutes.Deps, roomRoutes: RoomRoutes) extends RouteSupport:
  import d.{links, rooms, box, cfg}
  val sessions: SessionAuth = d.sessions

  /** トークンの登録と、登録したトークンで skuaka を呼ぶ補助（skakapo と共通）. */
  val link = new TokenLinks(d.skuaka, links, box, d.sessions, "skuaka")
  import link.{Res, fail, run, linked, writable}

  private def sk(c: SkuakaClient, token: String, method: Method, path: String, body: Option[Json] = None): Res[Json] =
    link.call(c, token, method, path, body)

  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")

  private def zone(u: User): ZoneId = Try(ZoneId.of(u.timezone)).getOrElse(ZoneId.of("Asia/Tokyo"))

  /** 予定に会議室を付ける: 場所に会議室の URL を入れる（場所があれば後ろに足す）. すでに付いていればその会議室. */
  def attach(u: User, eventId: UUID): Res[RoomDto] =
    for
      ct        <- writable(u, "予定を")
      (c, t)     = ct
      e         <- sk(c, t, Method.GET, s"/api/events/$eventId")
      ec         = e.hcursor
      loc        = ec.get[String]("location").getOrElse("")
      found     <- EitherT.liftF(RoomLinks.find(cfg.publicUrl, loc, ec.get[String]("description").getOrElse("")).flatTraverse(rooms.byCode))
      room <- found match
                case Some((r, o)) => EitherT.liftF(roomRoutes.dto(u.id, r, o))
                case None =>
                  if !ec.get[Boolean]("canEdit").getOrElse(false) then fail(Status.Forbidden, "この予定を変える権限がありません")
                  else
                    val title = ec.get[String]("title").getOrElse("会議")
                    for
                      r  <- EitherT.liftF(rooms.create(u.id, RoomReq(title.take(100)), event = SkuakaEvents.roomEvent(ec)))
                      url = s"${cfg.publicUrl}/r/${r.code}"
                      _  <- sk(c, t, Method.PUT, s"/api/events/$eventId?scope=all", Some(SkuakaEvents.toReq(e, if loc.trim.isEmpty then url else s"$loc / $url")))
                             .leftSemiflatTap(_ => rooms.delete(r.id))
                      dto <- EitherT.liftF(roomRoutes.dto(u.id, r, u.brief))
                    yield dto
    yield room

  /** 会議の予定を立てる: 会議室を作り、skuaka の自分のメインのカレンダーに予定を作って招待する. */
  def schedule(u: User, r: ScheduleReq): Res[ScheduleDto] =
    for
      ct        <- writable(u, "予定を")
      (c, t)     = ct
      title     <- EitherT.fromEither[IO](Check.name(r.title, "タイトル", 200).left.map(Status.BadRequest -> _))
      _         <- EitherT.fromEither[IO](Check.text(r.description, "説明", 5000).left.map(Status.BadRequest -> _))
      se        <- EitherT.fromEither[IO]((Try(Instant.parse(r.start)).toOption, Try(Instant.parse(r.end)).toOption) match
                     case (Some(s), Some(e)) if e.isAfter(s) => Right(s -> e)
                     case (Some(_), Some(_))                 => Left(Status.BadRequest -> "終わりは始まりより後にしてください")
                     case _                                  => Left(Status.BadRequest -> "日時は ISO 8601（UTC）で指定してください"))
      (s, e)     = se
      me        <- sk(c, t, Method.GET, "/api/me")
      cals      <- sk(c, t, Method.GET, "/api/calendars")
      myId       = me.hcursor.get[UUID]("id").toOption
      cal       <- EitherT.fromOption[IO](
                     cals.asArray.toList.flatten.map(_.hcursor).find(x => x.get[Boolean]("primary").contains(true) &&
                       x.downField("owner").get[UUID]("id").toOption == myId)
                       .orElse(cals.asArray.toList.flatten.map(_.hcursor).find(x => x.get[String]("role").exists(Set("owner", "edit"))))
                       .flatMap(_.get[UUID]("id").toOption),
                     Status.Conflict -> "skuaka に予定を作れるカレンダーがありません")
      emails     = r.attendees.map(_.trim).filter(_.contains("@")).distinct
      found     <- emails.traverse(m => sk(c, t, Method.GET, s"/api/users?q=${enc(m)}").map(js =>
                     m -> js.asArray.toList.flatten.map(_.hcursor).find(_.get[String]("email").exists(_.equalsIgnoreCase(m))).flatMap(_.get[UUID]("id").toOption)))
      room      <- EitherT.liftF(rooms.create(u.id, RoomReq(title.take(100), r.description.take(2000), r.guestAccess, r.lobby)))
      url        = s"${cfg.publicUrl}/r/${room.code}"
      desc       = (if r.description.trim.isEmpty then "" else r.description.trim + "\n\n") + s"Web会議（skaka）: $url"
      req        = Json.obj("calendarId" -> cal.asJson, "title" -> title.asJson, "description" -> desc.asJson, "location" -> url.asJson,
                     "allDay" -> false.asJson, "start" -> s.toString.asJson, "end" -> e.toString.asJson, "timezone" -> u.timezone.asJson,
                     "color" -> Json.Null, "recurrence" -> Json.Null, "attendees" -> found.flatMap(_._2).filterNot(myId.contains).distinct.asJson,
                     "reminders" -> List(10).asJson, "version" -> Json.Null)
      ev        <- sk(c, t, Method.POST, "/api/events", Some(req)).leftSemiflatTap(_ => rooms.delete(room.id))
      ref       <- EitherT.fromOption[IO](SkuakaEvents.roomEvent(ev.hcursor), Status.BadGateway -> "skuaka の応答を読めません")
      _         <- EitherT.liftF(rooms.setEvent(room.id, Some(ref)))
      dto       <- EitherT.liftF(roomRoutes.dto(u.id, room.copy(event = Some(ref)), u.brief))
    yield ScheduleDto(dto, ref.eventId, found.collect { case (m, Some(_)) => m }, found.collect { case (m, None) => m })

  /** 期間の予定（自分の予定. 不参加と返事したもの・予定あり・なしだけ見えるものを除く）. room は会議室のコード. */
  def upcoming(u: User, from: LocalDate, to: LocalDate, tz: String): Res[List[UpcomingDto]] =
    for
      ln        <- linked(u)
      (c, t, _)  = ln
      js        <- sk(c, t, Method.GET, s"/api/events?from=$from&to=$to&tz=${enc(tz)}")
      os         = js.asArray.toList.flatten.map(_.hcursor).filterNot(o => o.get[Boolean]("busy").contains(true) || o.get[String]("myResponse").contains("declined"))
      byEvent   <- EitherT.liftF(os.flatMap(_.get[UUID]("eventId").toOption).distinct.traverse(id => rooms.forEvent(id).map(id -> _)).map(_.toMap))
    yield os.flatMap { o =>
      (o.get[UUID]("eventId"), o.get[String]("title"), o.get[String]("start"), o.get[String]("end")).mapN { (id, title, s, e) =>
        val loc = o.get[String]("location").getOrElse("")
        UpcomingDto(id, title, loc, o.get[Boolean]("allDay").getOrElse(false), s, e, o.get[Boolean]("recurring").getOrElse(false),
          o.get[Boolean]("canEdit").getOrElse(false), o.get[String]("myResponse").toOption,
          RoomLinks.find(cfg.publicUrl, loc).orElse(byEvent.get(id).flatten))
      }.toOption
    }

  /** skuaka の予定に会議室を付けた・会議の予定を立てた数（メトリクス）. */
  private def count(action: String): IO[Unit] = skaka.observe.Metrics.count("skaka.skuaka.events", "action" -> action)

  private def date(s: Option[String]): Option[LocalDate] = s.flatMap(x => Try(LocalDate.parse(x)).toOption)

  val routes: HttpRoutes[IO] = link.routes <+> HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "skuaka" / "events" =>
      authed(req) { u =>
        val tz   = param(req, "tz").filter(z => Try(ZoneId.of(z)).isSuccess).getOrElse(zone(u).getId)
        val from = date(param(req, "from")).getOrElse(LocalDate.now(ZoneId.of(tz)))
        val to   = date(param(req, "to")).getOrElse(from.plusDays(7))
        if !to.isAfter(from) || to.isAfter(from.plusDays(62)) then err(Status.BadRequest, "期間は1〜62日にしてください")
        else run(upcoming(u, from, to, tz).semiflatMap(Ok(_)))
      }

    case req @ POST -> Root / "api" / "skuaka" / "events" / UuidVar(id) / "room" =>
      authed(req)(u => skaka.observe.Telemetry.span("skuaka.attach")(run(attach(u, id).semiflatTap(_ => count("attached")).semiflatMap(Ok(_)))))

    case req @ POST -> Root / "api" / "skuaka" / "schedule" =>
      authed(req)(u => body[ScheduleReq](req)(r => skaka.observe.Telemetry.span("skuaka.schedule")(run(schedule(u, r).semiflatTap(_ => count("scheduled")).semiflatMap(Created(_))))))
  }
