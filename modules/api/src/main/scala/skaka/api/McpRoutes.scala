package skaka.api

import skaka.core.ErrorCode
import cats.effect.*
import cats.syntax.all.*
import io.circe.{ACursor, Json}
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import sbirds.http.Errors
import sbirds.mcp.{McpCaller, McpServer, McpTool}
import sbirds.mcp.McpSchema.{bool, int, obj, schema, str, strs}
import skaka.shared.*

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId}
import java.util.{Locale, UUID}
import scala.util.Try

/**
 * MCP（Model Context Protocol）のサーバー. POST /mcp に JSON-RPC を送る（Streamable HTTP. 応答は JSON で、SSE は使わない）.
 * 認証は API トークン（Bearer）だけ. 各ツールは REST API をそのトークンで内部から呼ぶので、権限は REST API と同じになる.
 * 日時はトークンの持ち主のタイムゾーンで読み書きする（オフセット付きの ISO 8601 も受け付ける）.
 */
final class McpRoutes(api: HttpRoutes[IO], sessions: SessionAuth, publicUrl: String, version: String)
  extends Http4sDsl[IO] {

  // POST /mcp（JSON-RPC）と GET・DELETE /mcp（405）は sbirds.mcp.McpServer
  lazy val routes: HttpRoutes[IO] = McpServer.routes(host) <+> HttpRoutes.of[IO] {
    case GET -> Root / "api" / "endpoints" => getEndpoints()
  }

  private def getEndpoints(): IO[Response[IO]] =
    Ok(ApiCatalog.all.map(e =>
      Json.obj("method" -> e.method.asJson, "path" -> e.path.asJson, "description" -> e.description.asJson)
    ))

  // ------------------------------------------------------------------ REST API の内部呼び出し

  private type Caller = McpCaller
  private def path(segments: String*): Uri = McpCaller.path(segments*)

  // ------------------------------------------------------------------ 日時

  private final case class Ctx(call: Caller, me: MeDto) {
    val zone: ZoneId = Zones.of(me.timezone)
    def today: LocalDate = LocalDate.now(zone)
  }

  private val dayNames = Vector("月", "火", "水", "木", "金", "土", "日")
  private def dayText(d: LocalDate): String =
    f"${d.getYear}%d/${d.getMonthValue}%02d/${d.getDayOfMonth}%02d(${dayNames(d.getDayOfWeek.getValue - 1)})"
  private val hm = DateTimeFormatter.ofPattern("HH:mm", Locale.ROOT)

  private def at(c: Ctx, iso: String): String =
    Try(Instant.parse(iso)).toOption.fold(iso)(i => {
      val z = i.atZone(c.zone); s"${dayText(z.toLocalDate)} ${z.format(hm)}"
    })

  /** 時刻だけ（13:05）. */
  private def clock(c: Ctx, iso: String): String =
    Try(Instant.parse(iso)).toOption.fold(iso)(_.atZone(c.zone).format(hm))

  private def span(c: Ctx, allDay: Boolean, s: String, e: String): String =
    if allDay then if s == e then s"$s 終日" else s"$s〜$e 終日"
    else
      (Try(Instant.parse(s)).toOption, Try(Instant.parse(e)).toOption) match {
        case (Some(a), Some(b)) =>
          val (x, y) = (a.atZone(c.zone), b.atZone(c.zone))
          if x.toLocalDate == y.toLocalDate then s"${dayText(x.toLocalDate)} ${x.format(hm)}–${y.format(hm)}"
          else s"${at(c, s)}〜${at(c, e)}"
        case _ => s"$s〜$e"
      }

  /** 日時（2026-11-16T10:00、2026-11-16 10:00、オフセット付き）を UTC にする. */
  private def instant(s: String, zone: ZoneId): Either[String, Instant] = {
    val t = s.trim.replace(' ', 'T')
    Try(Instant.parse(t)).toOption
      .orElse(Try(java.time.OffsetDateTime.parse(t).toInstant).toOption)
      .orElse(Try(LocalDateTime.parse(t).atZone(zone).toInstant).toOption)
      .toRight(s"日時は yyyy-MM-ddTHH:mm で指定してください: $s")
  }

  private def date(s: String): Either[String, LocalDate] =
    Try(LocalDate.parse(s.trim.take(10))).toEither.left.map(_ => s"日付は yyyy-MM-dd で指定してください: $s")

  // ------------------------------------------------------------------ 引数

  private type Result = McpTool.Result
  private type Tool = McpTool[Ctx]

  private def Tool(name: String, description: String, schema: Json, readOnly: Boolean, destructive: Boolean = false)(
    run: (Ctx, ACursor) => Result): Tool = McpTool(name, description, schema, readOnly, destructive)(run)

  private def need(a: ACursor, k: String): Either[String, String] =
    opt(a, k).filter(_.nonEmpty).toRight(s"$k を指定してください")
  private def opt(a: ACursor, k: String): Option[String] = a.get[String](k).toOption.map(_.trim)
  private def uuid(s: String): Either[String, UUID] =
    Try(UUID.fromString(s.trim)).toEither.left.map(_ => s"ID が正しくありません: $s")
  private def code(a: ACursor): Either[String, String] =
    need(a, "room").flatMap(r => RoomCode.parse(r).toRight(s"会議室のコード・URL が正しくありません: $r"))

  private def lift[A](e: Either[String, A]): IO[Either[String, A]] = IO.pure(e)
  extension [A](io: IO[Either[String, A]]) {
    private def andThen[B](f: A => IO[Either[String, B]]): IO[Either[String, B]] =
      io.flatMap(_.fold(e => IO.pure(Left(e)), f))
    private def mapR[B](f: A => B): IO[Either[String, B]] = io.map(_.map(f))
  }

  // ------------------------------------------------------------------ 表示

  private def roomLine(c: Ctx, r: RoomDto): String = {
    val tags = List(
      Option.when(r.personal)("自分の会議室"),
      Option.when(!r.isOwner)(s"持ち主 ${r.owner.name}"),
      Option.when(r.guestAccess)("ゲスト可"),
      Option.when(r.lobby)("待機室"),
      Option.when(r.live > 0)(s"いま${r.live}人"),
      r.event.map(e => s"予定 ${e.title} ${span(c, e.allDay, e.start, e.end)}")
    ).flatten
    s"- ${r.name}（${r.code}）${r.url}${if tags.nonEmpty then tags.mkString(" ［", "・", "］") else ""}"
  }

  /** 録画の1行. */
  private def recordingLine(c: Ctx, r: RecordingDto): String = {
    val size = if r.bytes >= 1024 * 1024 then f"${r.bytes / 1048576.0}%.1f MB" else s"${r.bytes / 1024} KB"
    s"- 録画「${r.title}」（${r.by}, ${clock(c, r.startedAt)}〜${r.endedAt.fold("")(clock(c, _))}, $size）${RecordingStatus.label(r.status)}" +
      r.fileUrl.fold("")(u => s" $u") + r.error.fold("")(e => s"（$e）") + s" recordingId=${r.id}"
  }

  private def meetingLine(c: Ctx, m: MeetingDto): String =
    s"- ${at(c, m.startedAt)}〜${m.endedAt.fold("（開催中）")(clock(c, _))} ${m.roomName}（${m.roomCode}） 参加${m.participants}人" +
      s"${if m.messages > 0 then s"・チャット${m.messages}件" else ""} meetingId=${m.id}"

  private def roomReq(a: ACursor, cur: Option[RoomDto]): RoomReq =
    RoomReq(
      opt(a, "name").orElse(cur.map(_.name)).getOrElse(""),
      opt(a, "description").orElse(cur.map(_.description)).getOrElse(""),
      a.get[Boolean]("guestAccess").toOption.orElse(cur.map(_.guestAccess)).getOrElse(false),
      a.get[Boolean]("lobby").toOption.orElse(cur.map(_.lobby)).getOrElse(false)
    )

  private val roomProps = List(
    "name" -> str("会議室の名前"),
    "description" -> str("説明"),
    "guestAccess" -> bool("ログインしない人（ゲスト）もリンクから入れるか（主催者が許可してから入る）"),
    "lobby" -> bool("メンバーも待機室で主催者の許可を待つか")
  )

  // ------------------------------------------------------------------ ツール

  private val tools: List[Tool] = List(
    Tool("get_me", "自分の情報・タイムゾーン・今日の日付.", schema(Nil), readOnly = true) {
      (c, _) =>
        IO.pure(Right(
          s"${c.me.name}（${c.me.email.getOrElse("メールなし")}）id=${c.me.id}\nタイムゾーン: ${c.zone.getId}\n今日: ${dayText(c.today)}"
        ))
    },
    Tool("list_rooms", "自分の会議室と、最近入った会議室. 入っている人数も分かる.", schema(Nil), readOnly = true) {
      (c, _) =>
        c.call.as[List[RoomDto]](Method.GET, path("api", "rooms")).mapR(rs =>
          if rs.isEmpty then "会議室はありません" else rs.map(roomLine(c, _)).mkString("\n")
        )
    },
    Tool(
      "get_room",
      "会議室の情報と、いま入っている人.",
      schema(List("room"), "room" -> str("会議室のコード（abc-defg-hij）か URL")),
      readOnly = true
    ) {
      (c, a) =>
        lift(code(a)).andThen(k =>
          c.call.as[RoomDto](Method.GET, path("api", "rooms", k)).andThen(r =>
            c.call.as[LiveDto](Method.GET, path("api", "rooms", k, "live")).mapR { l =>
              {
                val who = l.peers.map(p =>
                  s"  - ${p.name}${
                      if p.host then "（主催者）" else ""
                    }${if p.guest then "（ゲスト）" else ""}" +
                    s"${if !p.media.audio then " マイクオフ" else ""}${if p.media.screen.isDefined then " 画面共有中" else ""}${
                        if p.media.hand then " ✋" else ""
                      }"
                )
                (roomLine(c, r) :: Option.when(r.description.nonEmpty)(s"説明: ${r.description}").toList :::
                  (if l.peers.isEmpty then List("いまは誰もいません")
                   else s"いま入っている人（${l.startedAt.fold("")(s => at(c, s) + "から")}）:" :: who) :::
                  Option.when(l.waiting > 0)(s"待機室: ${l.waiting}人").toList).mkString("\n")
              }
            }
          )
        )
    },
    Tool("create_room", "会議室を作る. URL を相手に送れば、その会議室で会議ができる.", schema(List("name"), roomProps*), readOnly = false) {
      (c, a) =>
        c.call.as[RoomDto](Method.POST, path("api", "rooms"), Some(roomReq(a, None).asJson)).mapR(r =>
          s"会議室を作りました\n${roomLine(c, r)}"
        )
    },
    Tool(
      "update_room",
      "会議室の名前・説明・ゲストの可否・待機室を変える（持ち主だけ）. 指定した項目だけ変わる.",
      schema(List("room"), (("room" -> str("会議室のコードか URL")) :: roomProps)*),
      readOnly = false
    ) {
      (c, a) =>
        lift(code(a)).andThen(k =>
          c.call.as[RoomDto](Method.GET, path("api", "rooms", k)).andThen(cur =>
            c.call.as[RoomDto](Method.PUT, path("api", "rooms", k), Some(roomReq(a, Some(cur)).asJson)).mapR(r =>
              s"変更しました\n${roomLine(c, r)}"
            )
          )
        )
    },
    Tool(
      "delete_room",
      "会議室を削除する（持ち主だけ. 自分の会議室は削除できない. 会議中の人は退出する）.",
      schema(List("room"), "room" -> str("会議室のコードか URL")),
      readOnly = false,
      destructive = true
    ) {
      (c, a) =>
        lift(code(a)).andThen(k => c.call(Method.DELETE, path("api", "rooms", k)).mapR(_ => "削除しました"))
    },
    Tool(
      "list_meetings",
      "会議の履歴（自分の会議室の会議と、参加した会議. 新しい順）.",
      schema(Nil, "room" -> str("会議室のコードか URL（省くとすべて）"), "limit" -> int("件数（既定 20）")),
      readOnly = true
    ) {
      (c, a) =>
        val room = opt(a, "room").filter(_.nonEmpty).map(r => RoomCode.parse(r).toRight(s"会議室のコード・URL が正しくありません: $r"))
        lift(room.sequence).andThen { r =>
          val u = path("api", "meetings").withQueryParam("limit", a.get[Int]("limit").getOrElse(20).toString)
          c.call.as[List[MeetingDto]](Method.GET, r.fold(u)(u.withQueryParam("room", _)))
            .mapR(ms => if ms.isEmpty then "会議の履歴はありません" else ms.map(meetingLine(c, _)).mkString("\n"))
        }
    },
    Tool(
      "get_meeting",
      "会議の参加者（入った・出た時刻）・文字起こし（話した人の名前付き）・チャットの記録と、作った議事録. 議事録や要約を作るのに使える.",
      schema(List("meetingId"), "meetingId" -> str("会議の ID（list_meetings の meetingId）")),
      readOnly = true
    ) {
      (c, a) =>
        lift(need(a, "meetingId").flatMap(uuid)).andThen(id =>
          c.call.as[MeetingDetailDto](Method.GET, path("api", "meetings", id.toString)).mapR { d =>
            (meetingLine(c, d.meeting) :: "参加者:" ::
              d.participants.map(p =>
                s"- ${p.name}${if p.guest then "（ゲスト）" else ""} ${clock(c, p.joinedAt)}〜${p.leftAt.fold("")(clock(c, _))}"
              ) :::
              (if d.transcript.isEmpty then Nil
               else "文字起こし:" :: d.transcript.map(t => s"[${clock(c, t.at)}] ${t.name}: ${t.text}")) :::
              (if d.chat.isEmpty then List("チャット: なし")
               else "チャット:" :: d.chat.map(m => s"[${clock(c, m.at)}] ${m.name}: ${m.text}")) :::
              d.minutes.find(_.body.nonEmpty).toList.flatMap(m => List(s"議事録（skakapo. ${m.by}）:", m.body)) :::
              d.recordings.map(recordingLine(c, _))).mkString("\n")
          }
        )
    },
    Tool(
      "list_recordings",
      "会議の録画（保存先は skawau. 自分が録画したもの・自分の会議室のもの・参加した会議のもの. 新しい順）. URL から skawau で再生できる.",
      schema(
        Nil,
        "room" -> str("会議室のコードか URL（省くとすべて）"),
        "meetingId" -> str("会議の ID（省くとすべて）"),
        "limit" -> int("件数（既定 20）")
      ),
      readOnly = true
    ) {
      (c, a) =>
        val room = opt(a, "room").filter(_.nonEmpty).map(r => RoomCode.parse(r).toRight(s"会議室のコード・URL が正しくありません: $r"))
        val mid = opt(a, "meetingId").filter(_.nonEmpty).map(uuid)
        lift((room.sequence, mid.sequence).tupled).andThen {
          (r, m) =>
            {
              val u0 = path("api", "recordings").withQueryParam("limit", a.get[Int]("limit").getOrElse(20).toString)
              val u = m.fold(r.fold(u0)(u0.withQueryParam("room", _)))(id => u0.withQueryParam("meeting", id.toString))
              c.call.as[List[RecordingDto]](Method.GET, u).mapR(rs =>
                if rs.isEmpty then "録画はありません" else rs.map(recordingLine(c, _)).mkString("\n")
              )
            }
        }
    },
    Tool(
      "list_upcoming",
      "skuaka（カレンダー）の自分の予定と、その会議室. 会議室がある予定は URL から入れる（設定で skuaka と連携している必要がある）.",
      schema(Nil, "from" -> str("開始日 yyyy-MM-dd（既定は今日）"), "days" -> int("何日分（既定 7、62まで）")),
      readOnly = true
    ) {
      (c, a) =>
        lift(opt(a, "from").filter(_.nonEmpty).map(date).getOrElse(Right(c.today))).andThen { f =>
          val t = f.plusDays(a.get[Int]("days").getOrElse(7).max(1).min(62).toLong)
          c.call.as[List[UpcomingDto]](
            Method.GET,
            path("api", "skuaka", "events").withQueryParam("from", f.toString).withQueryParam("to", t.toString)
              .withQueryParam("tz", c.zone.getId)
          ).mapR { es =>
            if es.isEmpty then s"${dayText(f)}〜 の予定はありません"
            else
              es.map(e =>
                s"- ${span(c, e.allDay, e.start, e.end)} ${e.title}" +
                  e.room.fold(if e.canEdit then "（会議室なし. add_room_to_event で付けられる）" else "")(r =>
                    s" 会議室 $publicUrl/r/$r"
                  ) +
                  s" eventId=${e.eventId}"
              ).mkString("\n")
          }
        }
    },
    Tool(
      "schedule_meeting",
      "Web会議の予定を立てる: 会議室を作り、skuaka（カレンダー）の自分のメインのカレンダーに予定を作って参加者を招待する. 予定の場所に会議室の URL が入る.",
      schema(
        List("title", "start"),
        "title" -> str("タイトル"),
        "start" -> str("開始 yyyy-MM-ddTHH:mm（自分のタイムゾーン）"),
        "end" -> str("終わり（省くと durationMinutes 分）"),
        "durationMinutes" -> int("長さ（分. 既定 30）"),
        "attendees" -> strs("招待する人（メールアドレス. skuaka にいる人だけ招待される）"),
        "description" -> str("説明"),
        "guestAccess" -> bool("ゲストも入れるか（既定 false）"),
        "lobby" -> bool("待機室を使うか（既定 false）")
      ),
      readOnly = false
    ) {
      (c, a) =>
        (need(a, "title"), need(a, "start").flatMap(instant(_, c.zone))).tupled match {
          case Left(e) => IO.pure(Left(e))
          case Right((title, s)) =>
            val e = opt(a, "end").filter(_.nonEmpty).map(instant(_, c.zone)).getOrElse(Right(s.plusSeconds(60L *
              a.get[Int]("durationMinutes").getOrElse(30).max(5))))
            lift(e).andThen { end =>
              val req = ScheduleReq(
                title,
                s.toString,
                end.toString,
                opt(a, "description").getOrElse(""),
                a.get[List[String]]("attendees").getOrElse(Nil),
                a.get[Boolean]("guestAccess").getOrElse(false),
                a.get[Boolean]("lobby").getOrElse(false)
              )
              c.call.as[ScheduleDto](Method.POST, path("api", "skuaka", "schedule"), Some(req.asJson)).mapR { d =>
                (s"予定を作りました: ${span(c, false, s.toString, end.toString)} $title" :: s"会議室: ${d.room.url}" ::
                  s"eventId=${d.eventId}" ::
                  Option.when(d.invited.nonEmpty)(s"招待: ${d.invited.mkString(", ")}").toList :::
                  Option.when(
                    d.notFound.nonEmpty
                  )(s"skuaka にいないため招待できなかった人: ${d.notFound.mkString(", ")}").toList).mkString("\n")
              }
            }
        }
    },
    Tool(
      "add_room_to_event",
      "skuaka の予定に会議室を付ける（予定の場所に会議室の URL を入れる. すでにあればその会議室）.",
      schema(List("eventId"), "eventId" -> str("skuaka の予定の ID（list_upcoming の eventId）")),
      readOnly = false
    ) {
      (c, a) =>
        lift(need(a, "eventId").flatMap(uuid)).andThen(id =>
          c.call.as[RoomDto](Method.POST, path("api", "skuaka", "events", id.toString, "room")).mapR(r =>
            s"会議室: ${r.url}\n${roomLine(c, r)}"
          )
        )
    },
    Tool("list_api_endpoints", "REST API の一覧. 専用のツールがない操作は call_api で行える.", schema(Nil), readOnly = true) {
      (_, _) =>
        IO.pure(Right(ApiCatalog.all.map(e => s"${e.method} ${e.path} — ${e.description}").mkString("\n")))
    },
    Tool(
      "call_api",
      "任意の REST API を呼ぶ（list_api_endpoints を参照）. 権限はこのトークンの持ち主と同じ. 日時は UTC の ISO 8601.",
      schema(
        List("method", "path"),
        "method" -> str("GET / POST / PUT / PATCH / DELETE"),
        "path" -> str("/api/ で始まるパス（クエリを含めてよい）"),
        "body" -> obj("JSON の本文")
      ),
      readOnly = false
    ) {
      (c, a) =>
        (need(a, "method").map(_.toUpperCase), need(a, "path")).tupled match {
          case Left(e) => IO.pure(Left(e))
          case Right((m, p)) =>
            (
              Method.fromString(m).toOption.filter(x => Set("GET", "POST", "PUT", "PATCH", "DELETE")(x.name)),
              Uri.fromString(p).toOption.filter(u =>
                u.path.renderString.startsWith("/api/") && !u.path.renderString.contains("..")
              )
            ) match {
              case (Some(method), Some(uri)) =>
                c.call(method, uri, a.downField("body").focus.filterNot(_.isNull)).mapR(_.spaces2)
              case (None, _) => IO.pure(Left("method は GET / POST / PUT / PATCH / DELETE です"))
              case _ => IO.pure(Left("path は /api/ で始めてください"))
            }
        }
    }
  )

  // ------------------------------------------------------------------ MCP の枠（sbirds.mcp.McpServer）に差し込む、skaka の部分

  private lazy val host = new McpServer.Host[Principal, Ctx] {
    val settings = McpServer.Settings("skaka", toolFailed = ErrorCode.McpToolFailed, version = version, maxBatch = 50)
    val tools = McpRoutes.this.tools
    def session(req: Request[IO], auth: Authorization) =
      sessions.fromRequest(Request[IO](Method.POST, Uri.unsafeFromString("/mcp")).putHeaders(auth))
    def unauthorized = Errors.respond(ErrorCode.ApiTokenRequired)
    def context(req: Request[IO], auth: Authorization, p: Principal) = Ctx(new McpCaller(api, auth), p.user.me)
    def canWrite(p: Principal) = p.canWrite
    def instructions(p: Principal) =
      ("skaka（チームの Web会議）の会議室・会議の履歴を扱うツールです。会議室は URL（/r/abc-defg-hij）で入り、" +
        "相手に URL を送れば会議ができます。日時はこのトークンの持ち主のタイムゾーン（get_me で分かる）で yyyy-MM-ddTHH:mm の形で渡します。" +
        "会議の予定は schedule_meeting で skuaka（カレンダー）に作れ、list_upcoming で今後の予定と会議室が分かります。" +
        "会議のチャットの記録は list_meetings・get_meeting で、録画は list_recordings で読めます。専用のツールがない操作は list_api_endpoints と call_api で行えます。" +
        (if p.canWrite then "" else "このトークンは参照のみなので、変更するツールは使えません。"))
  }
}
