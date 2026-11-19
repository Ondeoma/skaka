package skaka.shared

import io.circe.*
import io.circe.derivation.ConfiguredCodec

import java.util.UUID

// 会議中のやり取り（WebSocket /ws/{ticket} の JSON. {"type": "Chat", ...} の形）.
// 映像・音声は参加者どうしが WebRTC で直接送り合い（メッシュ）、サーバーは SDP・ICE の中継と、参加者の状態・チャットを受け持つ.

/** マイク・カメラ・画面共有・挙手の状態. screen は画面共有の MediaStream の id（共有していなければ None）. */
final case class MediaState(
  audio: Boolean,
  video: Boolean,
  screen: Option[String] = None,
  hand: Boolean = false)
  derives ConfiguredCodec

/** 参加者. id は接続ごとの値（同じ人が2つの端末で入れば2人）. host は主催者（会議室の持ち主と、主催者にされた人）. */
final case class PeerInfo(
  id: String,
  userId: Option[UUID],
  name: String,
  avatarUrl: Option[String],
  host: Boolean,
  guest: Boolean,
  media: MediaState,
  joinedAt: String) derives Encoder.AsObject, Decoder

/** チャット. to は個別のメッセージの相手（参加者の id. 全員宛なら None）. */
final case class ChatMsg(
  id: String,
  from: String,
  name: String,
  userId: Option[UUID],
  text: String,
  at: String,
  to: Option[String] = None)
  derives ConfiguredCodec

/** 録画中であること（参加者全員に知らせる）. */
final case class RecordingNotice(
  id: UUID,
  by: String,
  startedAt: String) derives Encoder.AsObject, Decoder

/** 文字起こし中であること（主催者が始める. 参加者全員に知らせる）. */
final case class TranscribeNotice(
  by: String,
  startedAt: String) derives Encoder.AsObject, Decoder

/** 文字起こしの1行（話した人の端末が送った音声を、サーバーが Whisper で文字にしたもの）. at は話し始めた時刻. */
final case class TranscriptLine(
  id: String,
  peer: String,
  name: String,
  userId: Option[UUID],
  text: String,
  at: String)
  derives Encoder.AsObject, Decoder

object Reactions {
  val all: List[String] = List("👍", "👏", "😂", "🎉", "❤️", "😮", "🙏", "🤔")
}

/** 画面 → サーバー. */
enum ClientMsg derives ConfiguredCodec {

  /** WebRTC の SDP・ICE を相手に中継する（data はそのまま渡す）. */
  case Signal(
    to: String,
    data: Json)
  case Media(
    state: MediaState)
  case Chat(
    text: String,
    to: Option[String] = None)
  case React(
    emoji: String)
  // 主催者の操作
  /** target のマイクを切るよう頼む（None なら主催者以外の全員）. */
  case MuteRequest(
    target: Option[String])
  case LowerHand(
    target: String)
  case Remove(
    target: String)
  case Admit(
    target: String)
  case Deny(
    target: String)
  case SetHost(
    target: String,
    host: Boolean)
  case Lock(
    locked: Boolean)

  /** 文字起こしを始める・止める. */
  case Transcribe(
    on: Boolean)

  /** 全員を退出させて会議を終える. */
  case EndAll()
  case Ping()
}

/** サーバー → 画面. */
enum ServerMsg derives ConfiguredCodec {

  /** 入れた. peers は自分以外の参加者、chat はこれまでの全員宛のチャット. */
  case Welcome(
    self: PeerInfo,
    peers: List[PeerInfo],
    chat: List[ChatMsg],
    locked: Boolean,
    startedAt: String,
    recording: Option[RecordingNotice] = None,
    transcribing: Option[TranscribeNotice] = None)

  /** 待機室で主催者の許可を待っている. */
  case Waiting(
    hostPresent: Boolean)

  /** 入れなかった・退出させられた・会議が終わった（reason は表示の文言）. */
  case Closed(
    reason: String)
  case Joined(
    peer: PeerInfo)
  case Left(
    id: String)

  /** 参加者の情報が変わった（主催者にされた・名前など）. */
  case Updated(
    peer: PeerInfo)
  case Signal(
    from: String,
    data: Json)
  case Media(
    id: String,
    state: MediaState)
  case Chat(
    msg: ChatMsg)
  case React(
    from: String,
    emoji: String)

  /** 待機室の人（主催者、主催者がいなければメンバーに送る）. */
  case Lobby(
    waiting: List[PeerInfo])

  /** 主催者にマイクを切るよう頼まれた. */
  case MuteRequested(
    by: String)
  case HandLowered(
    by: String)
  case Locked(
    locked: Boolean,
    by: String)

  /** 録画が始まった（Some）・終わった（None）. */
  case Recording(
    info: Option[RecordingNotice])

  /** 文字起こしが始まった（Some）・終わった（None）. */
  case Transcribing(
    info: Option[TranscribeNotice])

  /** 文字起こしの1行（字幕）. */
  case Caption(
    line: TranscriptLine)
  case Pong()
  case Error(
    message: String)
}
