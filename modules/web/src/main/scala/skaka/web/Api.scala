package skaka.web

import io.circe.*
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalajs.dom
import skaka.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/** バックエンドの REST API（共通部分は sbirds-lib の ApiBase. 更新の要求に付ける X-Skaka は Main で Fetch.extraHeaders に入れる）. */
object Api extends sbirds.web.ApiBase {

  /** ログインの画面へ（戻ってくる先は今の画面）. */
  def login(): Unit =
    dom.window.location.href = loginUrl(dom.window.location.pathname + dom.window.location.search)

  /** 401 でもログインへ移らない要求（呼んだ側で扱う）. */
  private def requestNoLogin(method: dom.HttpMethod, url: String, body: Option[Json] = None): Future[Option[Json]] =
    dom.fetch(url, init(method, body.map(_.noSpaces))).toFuture.flatMap { res =>
      if (res.status == 401) Future.failed(ApiError(401, "ログインしてください")) else handle(res)
    }

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  def devLogin(name: String, email: String, returnTo: String): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "returnTo" -> returnTo.asJson)
    )
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def users(q: String): Future[List[UserBrief]] = get[List[UserBrief]]("/api/users?q=" + enc(q))
  def updateMe(r: UpdateMeReq): Future[MeDto] = patch[MeDto]("/api/me", r.asJson)

  // ---------------------------------------------------------------- 会議室

  def rooms(): Future[List[RoomDto]] = get[List[RoomDto]]("/api/rooms")
  def room(code: String): Future[RoomDto] = get[RoomDto](s"/api/rooms/${enc(code)}")
  def createRoom(r: RoomReq): Future[RoomDto] = post[RoomDto]("/api/rooms", r.asJson)
  def updateRoom(code: String, r: RoomReq): Future[RoomDto] = put[RoomDto](s"/api/rooms/${enc(code)}", r.asJson)
  def deleteRoom(code: String): Future[Unit] = delete(s"/api/rooms/${enc(code)}")
  def forgetRoom(code: String): Future[Unit] = delete(s"/api/rooms/${enc(code)}/recent")

  /** 入る前の画面の情報（ログインしていなくても読める. 401 でもログインへは移らない）. */
  def entry(code: String): Future[RoomEntryDto] =
    requestNoLogin(dom.HttpMethod.GET, s"/api/rooms/${enc(code)}/entry").map(as[RoomEntryDto])
  def join(code: String, guestName: Option[String]): Future[JoinDto] =
    requestNoLogin(
      dom.HttpMethod.POST,
      s"/api/rooms/${enc(code)}/join",
      Some(JoinReq(guestName).asJson)
    ).map(as[JoinDto])

  // ---------------------------------------------------------------- 会議の履歴

  def meetings(room: Option[String] = None, before: Option[String] = None): Future[List[MeetingDto]] =
    get[List[MeetingDto]]("/api/meetings?limit=50" + room.fold("")(r => "&room=" + enc(r)) +
      before.fold("")(b => "&before=" + enc(b)))
  def meeting(id: UUID): Future[MeetingDetailDto] = get[MeetingDetailDto](s"/api/meetings/$id")

  // ---------------------------------------------------------------- skuaka（カレンダー）

  /** API トークンでの連携（service は skuaka・skakapo）. */
  def tokenLink(service: String): Future[TokenLinkDto] = get[TokenLinkDto](s"/api/$service")
  def linkToken(service: String, token: String): Future[TokenLinkDto] =
    put[TokenLinkDto](s"/api/$service", TokenLinkReq(token).asJson)
  def unlinkToken(service: String): Future[Unit] = delete(s"/api/$service")
  def upcoming(from: String, to: String): Future[List[UpcomingDto]] =
    get[List[UpcomingDto]](s"/api/skuaka/events?from=$from&to=$to&tz=${enc(Time.timezone())}")
  def attachRoom(eventId: UUID): Future[RoomDto] = post[RoomDto](s"/api/skuaka/events/$eventId/room")
  def schedule(r: ScheduleReq): Future[ScheduleDto] = post[ScheduleDto]("/api/skuaka/schedule", r.asJson)

  // ---------------------------------------------------------------- skawau（ファイル共有. 録画の保存先）と録画

  def skawau(): Future[SkawauLinkDto] = get[SkawauLinkDto]("/api/skawau")
  def unlinkSkawau(): Future[Unit] = delete("/api/integrations/skawau/connection")

  /** skawau の許可の画面へ（戻ると returnTo に skawau=connected などを付けて戻る）. */
  def connectSkawauUrl(returnTo: String): String = "/api/integrations/skawau/connect?returnTo=" + enc(returnTo)

  def startRecording(r: RecordingStartReq): Future[RecordingDto] = post[RecordingDto]("/api/recordings", r.asJson)
  def finishRecording(id: UUID, durationMs: Long): Future[RecordingDto] =
    post[RecordingDto](s"/api/recordings/$id/finish", RecordingFinishReq(durationMs).asJson)
  def retryRecording(id: UUID): Future[RecordingDto] = post[RecordingDto](s"/api/recordings/$id/retry")
  def deleteRecording(id: UUID): Future[Unit] = delete(s"/api/recordings/$id")

  /** 本文をそのまま（JSON にせず）送り、応答の JSON を返す. 失敗は ApiError（status 0 は通信の失敗）. */
  private def putBlob(url: String,
                      data: dom.Blob,
                      contentType: String,
                      method: dom.HttpMethod = dom.HttpMethod.PUT): Future[Option[Json]] = {
    val i = init(method)
    i.headers = js.Dictionary[String]("X-Skaka" -> "1", "Content-Type" -> contentType)
    i.body = data
    dom.fetch(url, i).toFuture.recover { case e => throw ApiError(0, e.getMessage) }.flatMap { res =>
      if (res.ok) res.text().toFuture.map(t => if (t.isEmpty) None else parse(t).toOption)
      else res.text().toFuture.recover(_ => "").flatMap { t =>
        Future.failed(ApiError(
          res.status,
          parse(t).toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"${res.status}")
        ))
      }
    }
  }

  /** 録画の断片を送る（本文はそのまま）. */
  def putChunk(id: UUID, seq: Int, data: dom.Blob): Future[Unit] =
    putBlob(s"/api/recordings/$id/chunks/$seq", data, "application/octet-stream").map(_ => ())

  /** 文字起こしの音声を送る（key は切符と一緒に受け取った鍵、agoMs は話し始めてからの時間）. 文字にできなければ None. */
  def speech(key: String, agoMs: Long, wav: dom.Blob): Future[Option[TranscriptLine]] =
    putBlob(
      s"/api/speech/${enc(key)}?ago=$agoMs",
      wav,
      "audio/wav",
      dom.HttpMethod.POST
    ).map(_.flatMap(_.as[TranscriptLine].toOption))

  // skakapo（議事録・要約）
  def requestMinutes(meetingId: UUID): Future[MinutesDto] = post[MinutesDto](s"/api/meetings/$meetingId/minutes")
  def refreshMinutes(id: UUID): Future[Unit] = post[Json](s"/api/minutes/$id/refresh").map(_ => ())
  def deleteMinutes(id: UUID): Future[Unit] = delete(s"/api/minutes/$id")
  def fileMinutes(id: UUID): Future[Unit] = post[Json](s"/api/minutes/$id/file").map(_ => ())

  /** アイコンを保存する（画像そのものを送る）・消す. */
  def uploadAvatar(image: dom.Blob): Future[MeDto] = putRaw[MeDto]("/api/me/avatar", image, Some(image.`type`))
  def deleteAvatar(): Future[MeDto] = request(dom.HttpMethod.DELETE, "/api/me/avatar").map(as[MeDto])
}
