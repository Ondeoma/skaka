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

/** API の呼び出しのエラー. */
final case class ApiError(status: Int, message: String) extends Exception(message)

/** バックエンドの REST API. 401 はログインへ（loginOn401 = false なら呼んだ側で扱う）、エラーは {"error": ...} の文言にする. */
object Api {

  /** fetch の設定（同じオリジンの Cookie を付ける. body は JSON の文字列）. */
  def init(method: dom.HttpMethod, body: Option[String] = None): dom.RequestInit = {
    val i = new dom.RequestInit {}
    i.method = method
    i.credentials = dom.RequestCredentials.`same-origin`
    // 更新の要求には X-Skaka を付ける（サーバーはこれのない更新を CSRF として断る）
    val hs = js.Dictionary[String]("X-Skaka" -> "1")
    body.foreach { b =>
      hs("Content-Type") = "application/json"
      i.body = b
    }
    i.headers = hs
    i
  }

  private def request(method: dom.HttpMethod, url: String, body: Option[Json] = None, loginOn401: Boolean = true): Future[Option[Json]] = {
    dom.fetch(url, init(method, body.map(_.noSpaces))).toFuture.flatMap { res =>
      if (res.status == 401 && loginOn401) {
        if (!dom.window.location.pathname.startsWith("/login")) login()
        Future.failed(ApiError(401, "ログインしてください"))
      } else if (!res.ok) {
        res.text().toFuture.recover(_ => "").flatMap { t =>
          val msg = parse(t).toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"${res.status} ${res.statusText}")
          Future.failed(ApiError(res.status, msg))
        }
      } else if (res.status == 204) Future.successful(None)
      else res.text().toFuture.map(t => if (t.isEmpty) None else Some(parse(t).fold(throw _, identity)))
    }
  }

  /** ログインの画面へ（戻ってくる先は今の画面）. */
  def login(): Unit = {
    val back = dom.window.location.pathname + dom.window.location.search
    dom.window.location.href = "/auth/login?returnTo=" + js.URIUtils.encodeURIComponent(back)
  }

  private def as[A: Decoder](j: Option[Json]): A = j.getOrElse(Json.Null).as[A].fold(e => throw ApiError(0, e.getMessage), identity)

  def get[A: Decoder](url: String): Future[A] = request(dom.HttpMethod.GET, url).map(as[A])
  def post[A: Decoder](url: String, body: Json = Json.obj()): Future[A] = request(dom.HttpMethod.POST, url, Some(body)).map(as[A])
  def put[A: Decoder](url: String, body: Json): Future[A] = request(dom.HttpMethod.PUT, url, Some(body)).map(as[A])
  def patch[A: Decoder](url: String, body: Json): Future[A] = request(dom.HttpMethod.PATCH, url, Some(body)).map(as[A])
  def delete(url: String): Future[Unit] = request(dom.HttpMethod.DELETE, url).map(_ => ())

  def enc(s: String): String = js.URIUtils.encodeURIComponent(s)

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  def devLogin(name: String, email: String, returnTo: String): Future[Json] =
    post[Json]("/auth/dev-login", Json.obj("name" -> name.asJson, "email" -> email.asJson, "returnTo" -> returnTo.asJson))
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def users(q: String): Future[List[UserBrief]] = get[List[UserBrief]]("/api/users?q=" + enc(q))
  def updateMe(r: UpdateMeReq): Future[MeDto] = patch[MeDto]("/api/me", r.asJson)

  // ---------------------------------------------------------------- API トークン

  def tokens(): Future[List[ApiTokenDto]] = get[List[ApiTokenDto]]("/api/tokens")
  def createToken(r: ApiTokenReq): Future[ApiTokenCreated] = post[ApiTokenCreated]("/api/tokens", r.asJson)
  def revokeToken(id: UUID): Future[Unit] = delete(s"/api/tokens/$id")

  // ---------------------------------------------------------------- 会議室

  def rooms(): Future[List[RoomDto]] = get[List[RoomDto]]("/api/rooms")
  def room(code: String): Future[RoomDto] = get[RoomDto](s"/api/rooms/${enc(code)}")
  def createRoom(r: RoomReq): Future[RoomDto] = post[RoomDto]("/api/rooms", r.asJson)
  def updateRoom(code: String, r: RoomReq): Future[RoomDto] = put[RoomDto](s"/api/rooms/${enc(code)}", r.asJson)
  def deleteRoom(code: String): Future[Unit] = delete(s"/api/rooms/${enc(code)}")
  def forgetRoom(code: String): Future[Unit] = delete(s"/api/rooms/${enc(code)}/recent")
  /** 入る前の画面の情報（ログインしていなくても読める. 401 でもログインへは移らない）. */
  def entry(code: String): Future[RoomEntryDto] =
    request(dom.HttpMethod.GET, s"/api/rooms/${enc(code)}/entry", loginOn401 = false).map(as[RoomEntryDto])
  def join(code: String, guestName: Option[String]): Future[JoinDto] =
    request(dom.HttpMethod.POST, s"/api/rooms/${enc(code)}/join", Some(JoinReq(guestName).asJson), loginOn401 = false).map(as[JoinDto])

  // ---------------------------------------------------------------- 会議の履歴

  def meetings(room: Option[String] = None, before: Option[String] = None): Future[List[MeetingDto]] =
    get[List[MeetingDto]]("/api/meetings?limit=50" + room.fold("")(r => "&room=" + enc(r)) + before.fold("")(b => "&before=" + enc(b)))
  def meeting(id: UUID): Future[MeetingDetailDto] = get[MeetingDetailDto](s"/api/meetings/$id")

  // ---------------------------------------------------------------- skuaka（カレンダー）

  def skuaka(): Future[SkuakaLinkDto] = get[SkuakaLinkDto]("/api/skuaka")
  def linkSkuaka(token: String): Future[SkuakaLinkDto] = put[SkuakaLinkDto]("/api/skuaka", SkuakaLinkReq(token).asJson)
  def unlinkSkuaka(): Future[Unit] = delete("/api/skuaka")
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
  private def putBlob(url: String, data: dom.Blob, contentType: String): Future[Option[Json]] = {
    val i = init(dom.HttpMethod.PUT)
    i.headers = js.Dictionary[String]("X-Skaka" -> "1", "Content-Type" -> contentType)
    i.body = data
    dom.fetch(url, i).toFuture.recover { case e => throw ApiError(0, e.getMessage) }.flatMap { res =>
      if (res.ok) res.text().toFuture.map(t => if (t.isEmpty) None else parse(t).toOption)
      else res.text().toFuture.recover(_ => "").flatMap { t =>
        Future.failed(ApiError(res.status, parse(t).toOption.flatMap(_.hcursor.get[String]("error").toOption).getOrElse(s"${res.status}")))
      }
    }
  }

  /** 録画の断片を送る（本文はそのまま）. */
  def putChunk(id: UUID, seq: Int, data: dom.Blob): Future[Unit] =
    putBlob(s"/api/recordings/$id/chunks/$seq", data, "application/octet-stream").map(_ => ())

  /** アイコンを保存する（画像そのものを送る）・消す. */
  def uploadAvatar(image: dom.Blob): Future[MeDto] = putBlob("/api/me/avatar", image, image.`type`).map(as[MeDto])
  def deleteAvatar(): Future[MeDto] = request(dom.HttpMethod.DELETE, "/api/me/avatar").map(as[MeDto])
}
