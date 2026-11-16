package skaka.shared

import io.circe.*
import io.circe.derivation.ConfiguredCodec

import java.util.UUID

// バックエンド(JVM)と画面(Scala.js)の両方で使う API の型。
// 省略できる項目（既定値のある項目）は、JSON で省いてもよい（ConfiguredCodec）.
// 日時は ISO 8601 の UTC の文字列（2026-11-15T01:00:00Z）. 画面(Scala.js)で java.time を使わないため、文字列で持つ。


final case class ErrorDto(error: String) derives Encoder.AsObject, Decoder

/** ログイン・連携のあとに戻る先（同じサイトのパスだけ. 他のサイトへ飛ばされないよう、// や \・制御文字を含むものは断る）. */
object SafePath {
  def check(p: String): Option[String] =
    Option(p).filter(s => s.startsWith("/") && !s.startsWith("//") && !s.exists(c => c == '\\' || c.isControl))

  def orElse(p: Option[String], default: String = "/"): String = p.flatMap(check).getOrElse(default)
}

/** skuaka・skawau はそれぞれ連携する skuaka（カレンダー）・skawau（ファイル共有. 録画の保存先）の URL（連携しなければ None）. */
final case class ClientConfigDto(devLogin: Boolean, oidc: Boolean, version: String, skuaka: Option[String], skawau: Option[String] = None)
    derives ConfiguredCodec

final case class UserBrief(id: UUID, name: String, email: Option[String], avatarUrl: Option[String]) derives Encoder.AsObject, Decoder

final case class MeDto(id: UUID, name: String, email: Option[String], avatarUrl: Option[String], timezone: String) derives Encoder.AsObject, Decoder {
  def brief: UserBrief = UserBrief(id, name, email, avatarUrl)
}

final case class UpdateMeReq(name: Option[String], timezone: Option[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 会議室

/** 会議室のコード（abc-defg-hij）. URL は /r/{code}. */
object RoomCode {
  val Pattern = "[a-z]{3}-[a-z]{4}-[a-z]{3}"
  private val re = ("^" + Pattern + "$").r
  def valid(s: String): Boolean = re.matches(s)
  /** 入力された URL・コードからコードを取り出す（大文字・空白・ハイフンなしも受け付ける）. */
  def parse(s: String): Option[String] = {
    val t = s.trim.toLowerCase
    val tail = t.split("/r/").lastOption.getOrElse(t).takeWhile(c => c != '?' && c != '#' && c != '/')
    val letters = tail.filter(c => c >= 'a' && c <= 'z')
    if (valid(tail)) Some(tail)
    else Option.when(letters.length == 10)(s"${letters.take(3)}-${letters.slice(3, 7)}-${letters.drop(7)}")
  }
}

/** 会議室に結び付いた skuaka の予定（作ったとき・予定から会議室を作ったときの写し）. */
final case class RoomEventDto(eventId: UUID, title: String, start: String, end: String, allDay: Boolean) derives Encoder.AsObject, Decoder

/**
 * 会議室. personal は1人に1つの「自分の会議室」. guestAccess はログインしない人（ゲスト）も入れるか（主催者が許可してから入る）、
 * lobby はメンバーも主催者の許可を待つか. live は今入っている人数.
 */
final case class RoomDto(id: UUID, code: String, name: String, description: String, owner: UserBrief, personal: Boolean,
                         guestAccess: Boolean, lobby: Boolean, url: String, isOwner: Boolean, event: Option[RoomEventDto], live: Int,
                         lastUsedAt: Option[String], createdAt: String) derives Encoder.AsObject, Decoder

/** 会議室を作る・変える. event は結び付ける予定の写し（作るときだけ. skuaka が予定に会議室を付けるとき）. */
final case class RoomReq(name: String, description: String = "", guestAccess: Boolean = false, lobby: Boolean = false,
                         event: Option[RoomEventDto] = None) derives ConfiguredCodec

/** 入る前の画面の情報（ログインしていない人にも返す）. me はログインしていれば自分. */
final case class RoomEntryDto(code: String, name: String, owner: UserBrief, guestAccess: Boolean, live: Int, locked: Boolean,
                              me: Option[MeDto], event: Option[RoomEventDto]) derives Encoder.AsObject, Decoder

/** 入る. name はゲストの名前（ログインしている人は省く）. */
final case class JoinReq(name: Option[String]) derives Encoder.AsObject, Decoder

/** WebRTC の ICE サーバー（STUN・TURN）. */
final case class IceServer(urls: List[String], username: Option[String] = None, credential: Option[String] = None)
    derives ConfiguredCodec

/** 入るための切符（WebSocket /ws/{ticket} で使う. 1回だけ・60秒）. */
final case class JoinDto(ticket: String, iceServers: List[IceServer]) derives Encoder.AsObject, Decoder

/** いま入っている人（会議室の一覧・MCP 用）. */
final case class LiveDto(code: String, peers: List[PeerInfo], waiting: Int, locked: Boolean, startedAt: Option[String])
    derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ 会議の履歴

/** 開いた会議（会議室に人が入ってから全員が出るまで）. */
final case class MeetingDto(id: UUID, roomCode: String, roomName: String, startedAt: String, endedAt: Option[String], participants: Int,
                            messages: Int) derives Encoder.AsObject, Decoder

final case class ParticipantDto(name: String, userId: Option[UUID], guest: Boolean, joinedAt: String, leftAt: Option[String])
    derives Encoder.AsObject, Decoder

final case class MeetingDetailDto(meeting: MeetingDto, participants: List[ParticipantDto], chat: List[ChatMsg],
                                  recordings: List[RecordingDto] = Nil) derives ConfiguredCodec

// ------------------------------------------------------------------ 録画（保存先は skawau）

/**
 * 録画. status は recording（録画中）/ saving（skawau へ保存中）/ saved / failed. fileUrl は skawau で開く URL.
 * mine は自分が録画したもの（再試行・削除ができる）. shared は skawau で共有した参加者の数.
 */
final case class RecordingDto(id: UUID, meetingId: Option[UUID], roomCode: String, title: String, by: String, status: String,
                              startedAt: String, endedAt: Option[String], bytes: Long, fileUrl: Option[String], error: Option[String],
                              shared: Int, mine: Boolean) derives Encoder.AsObject, Decoder

object RecordingStatus {
  val Recording = "recording"
  val Saving = "saving"
  val Saved = "saved"
  val Failed = "failed"
  def label(s: String): String = s match {
    case Recording => "録画中"
    case Saving    => "skawau に保存中"
    case Saved     => "保存済み"
    case _         => "保存できませんでした"
  }
}

/** 録画を始める. mime は MediaRecorder の形式（video/webm など）. share は終わったら会議の参加者と skawau で共有するか. */
final case class RecordingStartReq(room: String, mime: String, share: Boolean = true) derives ConfiguredCodec

/** 録画を終える. durationMs は録画の長さ（WebM の長さの情報を補う）. */
final case class RecordingFinishReq(durationMs: Long) derives Encoder.AsObject, Decoder

/** skawau との連携の状態（OAuth で許可したか）. */
final case class SkawauLinkDto(available: Boolean, url: Option[String], linked: Boolean, since: Option[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ skuaka（カレンダー）との連携

/** 連携の状態. account は skuaka での名前、prefix はトークンの先頭（画面の表示用）. */
final case class SkuakaLinkDto(available: Boolean, url: Option[String], linked: Boolean, account: Option[String], prefix: Option[String],
                               canWrite: Boolean) derives Encoder.AsObject, Decoder

/** skuaka の API トークンを登録する. */
final case class SkuakaLinkReq(token: String) derives Encoder.AsObject, Decoder

/** skuaka の予定（1回分）. room はその予定の会議室（場所・説明に skaka の URL があれば）. */
final case class UpcomingDto(eventId: UUID, title: String, location: String, allDay: Boolean, start: String, end: String, recurring: Boolean,
                             canEdit: Boolean, myResponse: Option[String], room: Option[String]) derives Encoder.AsObject, Decoder

/** 会議の予定を立てる（skuaka に予定を作り、会議室を用意する）. start・end は UTC. attendees は招待する人（メールアドレス）. */
final case class ScheduleReq(title: String, start: String, end: String, description: String = "", attendees: List[String] = Nil,
                             guestAccess: Boolean = false, lobby: Boolean = false) derives ConfiguredCodec

final case class ScheduleDto(room: RoomDto, eventId: UUID, invited: List[String], notFound: List[String]) derives Encoder.AsObject, Decoder

// ------------------------------------------------------------------ API トークン

/** API トークン. scope は read（参照だけ）か write（変更も）. 日時は UTC の ISO 8601. */
object TokenScope {
  val Read = "read"
  val Write = "write"
  val all: List[String] = List(Read, Write)
  def label(s: String): String = if (s == Write) "読み書き" else "参照のみ"
}

final case class ApiTokenDto(id: UUID, name: String, prefix: String, scope: String, createdAt: String, expiresAt: Option[String],
                             lastUsedAt: Option[String]) derives Encoder.AsObject, Decoder

final case class ApiTokenReq(name: String, scope: String, expiresInDays: Option[Int]) derives Encoder.AsObject, Decoder

/** 作ったときだけ token（秘密の値）を返す. */
final case class ApiTokenCreated(info: ApiTokenDto, token: String) derives Encoder.AsObject, Decoder
