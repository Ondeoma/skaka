package skaka.core

import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 会議の状態の変化に伴って行うこと（送る・切る・DB に書く）. */
enum Effect:
  case Send(to: String, msg: ServerMsg)
  /** 送り終えてから接続を切る. */
  case Disconnect(peer: String)
  case StartMeeting(id: UUID, roomId: UUID, at: Instant)
  case EndMeeting(id: UUID, at: Instant)
  case Joined(rowId: UUID, meetingId: UUID, peer: PeerInfo, at: Instant)
  case LeftMeeting(rowId: UUID, at: Instant)
  case SaveChat(meetingId: UUID, msg: ChatMsg, at: Instant)
  case SaveTranscript(meetingId: UUID, line: TranscriptLine, at: Instant)

/**
 * 1つの会議室のいまの会議（メモリ上. 純粋な関数で変える）.
 * peers は入っている人、waiting は待機室の人. rows は参加者の履歴の行の id（参加者 → 行）. hostUsers は主催者にされた人
 * （出入りしても主催者のまま）. recording は録画中であること（録画そのものは録画する人の画面とサーバーの Recorder が行う）.
 * transcribing は文字起こし中であること（音声は話す人の端末が送り、サーバーの Transcriber が文字にする）.
 */
final case class Live(roomId: UUID, ownerId: UUID, lobbyAll: Boolean, meetingId: Option[UUID] = None, startedAt: Option[Instant] = None,
                      locked: Boolean = false, peers: Vector[PeerInfo] = Vector.empty, waiting: Vector[PeerInfo] = Vector.empty,
                      chat: Vector[ChatMsg] = Vector.empty, rows: Map[String, UUID] = Map.empty, hostUsers: Set[UUID] = Set.empty,
                      recording: Option[RecordingNotice] = None, transcribing: Option[TranscribeNotice] = None):
  def isEmpty: Boolean = peers.isEmpty && waiting.isEmpty
  def peer(id: String): Option[PeerInfo] = peers.find(_.id == id)
  def hostPresent: Boolean = peers.exists(_.host)

  /** 待機室の人を入れられる人: 主催者. 主催者がいなければ、ログインしているメンバー. */
  def admitters: Vector[PeerInfo] = if hostPresent then peers.filter(_.host) else peers.filterNot(_.guest)

  def canAdmit(id: String): Boolean = admitters.exists(_.id == id)

object Conference:
  val MaxChat     = 2000
  val ChatHistory = 300
  val MaxPeers    = 50

  private type R = (Live, List[Effect])

  private def to(ps: Iterable[PeerInfo], msg: ServerMsg): List[Effect] = ps.toList.map(p => Effect.Send(p.id, msg))
  private def others(l: Live, id: String, msg: ServerMsg): List[Effect] = to(l.peers.filterNot(_.id == id), msg)

  /** 待機室が変わったことを、入れられる人に知らせる. */
  private def lobbyNotice(l: Live): List[Effect] = to(l.admitters, ServerMsg.Lobby(l.waiting.toList))

  private def closeWith(id: String, reason: String): List[Effect] = List(Effect.Send(id, ServerMsg.Closed(reason)), Effect.Disconnect(id))

  /** 主催者か（会議室の持ち主と、この会議で主催者にされた人）. */
  def isHost(l: Live, userId: Option[UUID]): Boolean = userId.exists(u => u == l.ownerId || l.hostUsers(u))

  /**
   * 接続した. peer.host は呼ぶ側では決めず、ここで決める.
   * ゲストはいつも待機室へ. メンバーは lobbyAll のときだけ待機室へ（主催者を除く）. ロック中は主催者だけが入れる.
   */
  def arrive(l: Live, peer0: PeerInfo, now: Instant, fresh: () => UUID): R =
    val peer = peer0.copy(host = isHost(l, peer0.userId))
    if l.peers.size + l.waiting.size >= MaxPeers then (l, closeWith(peer.id, s"この会議室は満員です（$MaxPeers 人まで）"))
    else if l.locked && !peer.host then (l, closeWith(peer.id, "会議はロックされています。主催者にロックを外してもらってください"))
    else if peer.guest || (l.lobbyAll && !peer.host) then
      val l2 = l.copy(waiting = l.waiting :+ peer)
      (l2, Effect.Send(peer.id, ServerMsg.Waiting(l2.hostPresent)) :: lobbyNotice(l2))
    else admit(l, peer, now, fresh)

  private def admit(l: Live, peer: PeerInfo, now: Instant, fresh: () => UUID): R =
    val (mid, started, startFx) = l.meetingId match
      case Some(id) => (id, l.startedAt.getOrElse(now), Nil)
      case None     => val id = fresh(); (id, now, List(Effect.StartMeeting(id, l.roomId, now)))
    val row = fresh()
    val l2  = l.copy(meetingId = Some(mid), startedAt = Some(started), peers = l.peers :+ peer, waiting = l.waiting.filterNot(_.id == peer.id),
      rows = l.rows.updated(peer.id, row))
    val fx = startFx ++ List(
      Effect.Joined(row, mid, peer, now),
      Effect.Send(peer.id, ServerMsg.Welcome(peer, l.peers.toList, l.chat.toList, l2.locked, started.toString, l2.recording, l2.transcribing))
    ) ++ others(l2, peer.id, ServerMsg.Joined(peer))
    // 入れられる人が変わったかもしれない（主催者が来た・最初のメンバーが来た）
    (l2, fx ++ (if l2.waiting.nonEmpty then lobbyNotice(l2) else Nil))

  /** 接続が切れた. 最後の1人が出たら会議を終える. */
  def leave(l: Live, id: String, now: Instant): R =
    if l.waiting.exists(_.id == id) then
      val l2 = l.copy(waiting = l.waiting.filterNot(_.id == id))
      (l2, lobbyNotice(l2))
    else if l.peer(id).isEmpty then (l, Nil)
    else
      val l2  = l.copy(peers = l.peers.filterNot(_.id == id), rows = l.rows - id)
      val row = l.rows.get(id).map(Effect.LeftMeeting(_, now)).toList
      if l2.peers.isEmpty then
        val end = l.meetingId.map(Effect.EndMeeting(_, now)).toList
        // 待機室の人だけが残ったら、会議の続きはない（次に入った人から新しい会議）
        (l2.copy(meetingId = None, startedAt = None, locked = false, chat = Vector.empty, hostUsers = Set.empty, recording = None,
          transcribing = None),
          row ++ end ++ to(l2.waiting, ServerMsg.Waiting(false)))
      else (l2, row ++ to(l2.peers, ServerMsg.Left(id)) ++ (if l2.waiting.nonEmpty then lobbyNotice(l2) else Nil))

  /** 録画が始まった（Some）・終わった（None. id の録画のときだけ）. 全員に知らせる. */
  def recording(l: Live, notice: Option[RecordingNotice], id: UUID): R =
    if notice.isEmpty && !l.recording.exists(_.id == id) then (l, Nil)
    else (l.copy(recording = notice), to(l.peers, ServerMsg.Recording(notice)))

  /** 文字起こしの1行: 文字起こし中なら、全員に送って保存する. */
  def caption(l: Live, line: TranscriptLine, now: Instant): R =
    (l.transcribing, l.meetingId) match
      case (Some(_), Some(m)) if l.peer(line.peer).isDefined => (l, Effect.SaveTranscript(m, line, now) :: to(l.peers, ServerMsg.Caption(line)))
      case _                                                 => (l, Nil)

  /** 参加者からのメッセージ. */
  def handle(l: Live, from: String, msg: ClientMsg, now: Instant, fresh: () => UUID): R =
    l.peer(from) match
      case None =>
        // 待機室の人は、待つことしかできない
        msg match
          case ClientMsg.Ping() => (l, List(Effect.Send(from, ServerMsg.Pong())))
          case _                => (l, Nil)
      case Some(me) => fromPeer(l, me, msg, now, fresh)

  private def hostOnly(l: Live, me: PeerInfo)(f: => R): R =
    if me.host then f else (l, List(Effect.Send(me.id, ServerMsg.Error("主催者だけができる操作です"))))

  private def update(l: Live, p: PeerInfo): Live = l.copy(peers = l.peers.map(x => if x.id == p.id then p else x))

  private def fromPeer(l: Live, me: PeerInfo, msg: ClientMsg, now: Instant, fresh: () => UUID): R = msg match
    case ClientMsg.Ping() => (l, List(Effect.Send(me.id, ServerMsg.Pong())))

    case ClientMsg.Signal(target, data) =>
      (l, l.peer(target).map(t => Effect.Send(t.id, ServerMsg.Signal(me.id, data))).toList)

    case ClientMsg.Media(st) =>
      val p = me.copy(media = st)
      (update(l, p), others(l, me.id, ServerMsg.Media(me.id, st)))

    case ClientMsg.Chat(text0, target) =>
      val text = text0.trim.take(MaxChat)
      if text.isEmpty then (l, Nil)
      else
        val m = ChatMsg(fresh().toString, me.id, me.name, me.userId, text, now.toString, target)
        target match
          case Some(t) =>
            l.peer(t) match
              case Some(tp) => (l, List(Effect.Send(tp.id, ServerMsg.Chat(m)), Effect.Send(me.id, ServerMsg.Chat(m))))
              case None     => (l, List(Effect.Send(me.id, ServerMsg.Error("相手はもう退出しました"))))
          case None =>
            val save = l.meetingId.map(Effect.SaveChat(_, m, now)).toList
            (l.copy(chat = (l.chat :+ m).takeRight(ChatHistory)), save ++ to(l.peers, ServerMsg.Chat(m)))

    case ClientMsg.React(e) =>
      if Reactions.all.contains(e) then (l, to(l.peers, ServerMsg.React(me.id, e))) else (l, Nil)

    case ClientMsg.Admit(target) =>
      l.waiting.find(_.id == target) match
        case Some(w) if l.canAdmit(me.id) => admit(l, w, now, fresh)
        case Some(_)                      => (l, List(Effect.Send(me.id, ServerMsg.Error("待機室の人を入れられるのは主催者です"))))
        case None                         => (l, Nil)

    case ClientMsg.Deny(target) =>
      if !l.canAdmit(me.id) then (l, List(Effect.Send(me.id, ServerMsg.Error("待機室の人を断れるのは主催者です"))))
      else if !l.waiting.exists(_.id == target) then (l, Nil)
      else
        val l2 = l.copy(waiting = l.waiting.filterNot(_.id == target))
        (l2, closeWith(target, "参加が許可されませんでした") ++ lobbyNotice(l2))

    case ClientMsg.MuteRequest(target) => hostOnly(l, me) {
        val targets = target.fold(l.peers.filterNot(_.host))(t => l.peers.filter(_.id == t))
        (l, to(targets, ServerMsg.MuteRequested(me.name)))
      }

    case ClientMsg.LowerHand(target) =>
      if target != me.id && !me.host then (l, List(Effect.Send(me.id, ServerMsg.Error("主催者だけができる操作です"))))
      else
        l.peer(target) match
          case Some(p) if p.media.hand =>
            val st = p.media.copy(hand = false)
            val fx = to(l.peers.filterNot(_.id == target), ServerMsg.Media(target, st)) ++
              (if target != me.id then List(Effect.Send(target, ServerMsg.HandLowered(me.name))) else Nil)
            (update(l, p.copy(media = st)), fx)
          case _ => (l, Nil)

    case ClientMsg.Remove(target) => hostOnly(l, me) {
        l.peer(target) match
          case Some(t) if t.userId.contains(l.ownerId) => (l, List(Effect.Send(me.id, ServerMsg.Error("会議室の持ち主は退出させられません"))))
          case Some(t) if t.id != me.id =>
            val (l2, fx) = leave(l, target, now)
            (l2, closeWith(target, s"${me.name} さんにより会議から退出しました") ++ fx)
          case _ => (l, Nil)
      }

    case ClientMsg.SetHost(target, host) => hostOnly(l, me) {
        l.peer(target) match
          case Some(t) if t.guest => (l, List(Effect.Send(me.id, ServerMsg.Error("ゲストは主催者にできません"))))
          case Some(t) if !host && t.userId.contains(l.ownerId) => (l, List(Effect.Send(me.id, ServerMsg.Error("会議室の持ち主は主催者のままです"))))
          case Some(t) if t.host != host =>
            val users = t.userId.fold(l.hostUsers)(u => if host then l.hostUsers + u else l.hostUsers - u)
            // 同じ人のほかの端末も合わせる
            val changed = l.peers.filter(p => p.id == t.id || (p.userId.isDefined && p.userId == t.userId)).map(_.copy(host = host))
            val l2 = changed.foldLeft(l.copy(hostUsers = users))(update)
            (l2, changed.toList.flatMap(p => to(l2.peers, ServerMsg.Updated(p))) ++ (if l2.waiting.nonEmpty then lobbyNotice(l2) else Nil))
          case _ => (l, Nil)
      }

    case ClientMsg.Lock(locked) => hostOnly(l, me) {
        (l.copy(locked = locked), to(l.peers, ServerMsg.Locked(locked, me.name)))
      }

    case ClientMsg.Transcribe(on) => hostOnly(l, me) {
        if on == l.transcribing.isDefined then (l, Nil)
        else
          val n = Option.when(on)(TranscribeNotice(me.name, now.toString))
          (l.copy(transcribing = n), to(l.peers, ServerMsg.Transcribing(n)))
      }

    case ClientMsg.EndAll() => hostOnly(l, me) {
        val reason = s"${me.name} さんが会議を終了しました"
        val fx     = (l.peers ++ l.waiting).toList.flatMap(p => closeWith(p.id, reason))
        val rows   = l.rows.values.toList.map(Effect.LeftMeeting(_, now))
        (Live(l.roomId, l.ownerId, l.lobbyAll), fx ++ rows ++ l.meetingId.map(Effect.EndMeeting(_, now)).toList)
      }

/** skuaka の予定の場所・説明から、skaka の会議室のコードを探す. */
object RoomLinks:
  def find(publicUrl: String, texts: String*): Option[String] =
    val re = (java.util.regex.Pattern.quote(publicUrl.stripSuffix("/")) + "/r/(" + RoomCode.Pattern + ")").r
    texts.iterator.flatMap(t => re.findFirstMatchIn(t).map(_.group(1))).nextOption()
