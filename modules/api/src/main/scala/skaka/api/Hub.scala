package skaka.api

import skaka.core.ErrorCode
import cats.effect.*
import cats.effect.std.{AtomicCell, Queue}
import cats.syntax.all.*
import skaka.core.{Conference, Effect, Live}
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/** 入るための切符（POST /api/rooms/{code}/join で発行し、WebSocket の接続で1回だけ使う）. key は会議中に文字起こしの音声を送るときの鍵. */
final case class Ticket(room: Room, user: Option[User], guestName: Option[String], expires: Instant, key: String = "")

/**
 * 会議中の状態（メモリ上. このノードの会議室だけ）. 状態の変化は Conference の純粋な関数で行い、
 * 送る・切るは状態を変えたのと同じ順番で各接続のキューに入れる（AtomicCell で1つずつ）. DB への書き込みは裏で順に行う.
 */
final class Hub private (cell: AtomicCell[IO, Hub.State],
                         tickets: Ref[IO, Map[String, Ticket]],
                         dbq: Queue[IO, Effect],
                         meetings: MeetingStore,
                         notices: Notices,
                         transcription: Boolean) {
  import Hub.State

  // ---------------------------------------------------------------- 切符

  /** 切符を発行する: (切符, 音声を送るときの鍵). */
  def issue(t: Ticket): IO[(String, String)] = {
    val id = sbirds.auth.Crypto.randomToken(24)
    val key = sbirds.auth.Crypto.randomToken(24)
    IO.realTimeInstant.flatMap(now =>
      tickets.update(m => m.filter(_._2.expires.isAfter(now)).updated(id, t.copy(key = key)))
    ).as(id -> key)
  }

  private def take(id: String): IO[Option[Ticket]] =
    IO.realTimeInstant.flatMap(now => tickets.modify(m => (m - id, m.get(id).filter(_.expires.isAfter(now)))))

  // ---------------------------------------------------------------- 接続

  /** 切符で接続する. 接続ごとの id と、送るメッセージのキュー（None で切る）を返す. 切符が使えなければ None. */
  def connect(ticketId: String): IO[Option[(String, Queue[IO, Option[ServerMsg]])]] =
    take(ticketId).flatMap {
      case None => IO.pure(None)
      case Some(t) =>
        for {
          q <- Queue.unbounded[IO, Option[ServerMsg]]
          now <- IO.realTimeInstant
          id = sbirds.auth.Crypto.randomToken(9)
          peer = PeerInfo(
            id,
            t.user.map(_.id),
            t.user.fold(t.guestName.getOrElse("ゲスト"))(_.name),
            t.user.flatMap(_.avatarUrl),
            host = false,
            guest = t.user.isEmpty,
            MediaState(audio = false, video = false),
            now.toString
          )
          ba <- runWith { s =>
            val live = s.lives.getOrElse(t.room.id, Live(t.room.id, t.room.ownerId, t.room.lobby))
            val s1 = s.copy(
              conns = s.conns.updated(id, q),
              roomOf = s.roomOf.updated(id, t.room.id),
              seen = s.seen.updated(id, now),
              keys = s.keys.updated(t.key, id)
            )
            apply(s1, t.room.id, Conference.arrive(live, peer, now, () => UUID.randomUUID()))
          }
          (before, after) = ba
          _ <- arrived(t.room, id, before.lives.get(t.room.id), after.lives.get(t.room.id))
        } yield Some(id -> q)
    }

  /** 入ってきた後: 待機室に入った（持ち主がいなければプッシュ通知）・会議が始まった（招待された人にプッシュ通知）. */
  private def arrived(room: Room, peer: String, before: Option[Live], after: Option[Live]): IO[Unit] =
    after.traverse_ { l =>
      val knock = l.waiting.find(p => p.id == peer && !p.userId.contains(room.ownerId))
      val started = l.meetingId.isDefined && before.flatMap(_.meetingId) != l.meetingId
      knock.traverse_(p =>
        sbirds.observe.Metrics.count("skaka.lobby.knocks", "guest" -> p.guest.toString) *>
          IO.whenA(!l.peers.exists(_.userId.contains(room.ownerId)))(notices.knock(room, p.name))
      ) *>
        IO.whenA(started)(notices.started(room, l.peers.flatMap(_.userId).toSet).start.void)
    }

  def receive(peer: String, msg: ClientMsg): IO[Unit] = msg match {
    case ClientMsg.Transcribe(true) if !transcription =>
      run(s => (s, List(Effect.Send(peer, ServerMsg.Error("文字起こしは使えません（サーバーに Whisper が設定されていません）")))))
    case _ => handle(peer, msg)
  }

  private def handle(peer: String, msg: ClientMsg): IO[Unit] =
    IO.realTimeInstant.flatMap(now =>
      run { s0 =>
        val s = s0.copy(seen = if s0.conns.contains(peer) then s0.seen.updated(peer, now) else s0.seen)
        s.roomOf.get(peer).flatMap(r => s.lives.get(r).map(r -> _)) match {
          case Some((r, live)) => apply(s, r, Conference.handle(live, peer, msg, now, () => UUID.randomUUID()))
          case None => (s, Nil)
        }
      }
    )

  /** しばらく何も届かない接続を切る（画面は20秒ごとに Ping を送る. スリープ・回線断で TCP が残っても、参加者に残らないように）. */
  def reap(silent: FiniteDuration): IO[Unit] =
    IO.realTimeInstant.flatMap { now =>
      cell.get.map(_.seen.collect { case (p, t) if t.plusMillis(silent.toMillis).isBefore(now) => p }.toList)
    }.flatMap(_.traverse_(p =>
      run(s => (s, List(Effect.Send(p, ServerMsg.Closed("接続が切れました（応答がありません）")), Effect.Disconnect(p)))) *> disconnect(p)
    ))

  /** 接続が切れた（何度呼んでもよい）. */
  def disconnect(peer: String): IO[Unit] =
    IO.realTimeInstant.flatMap(now =>
      run { s =>
        val s1 = s.copy(
          conns = s.conns - peer,
          roomOf = s.roomOf - peer,
          seen = s.seen - peer,
          keys = s.keys.filterNot(_._2 == peer)
        )
        s.roomOf.get(peer).flatMap(r => s.lives.get(r).map(r -> _)) match {
          case Some((r, live)) => apply(s1, r, Conference.leave(live, peer, now))
          case None => (s1, Nil)
        }
      }
    )

  // ---------------------------------------------------------------- 会議室の変更

  /** 会議室の設定が変わった（待機室を使うか）. */
  def roomChanged(r: Room): IO[Unit] =
    run(s => (s.copy(lives = s.lives.updatedWith(r.id)(_.map(_.copy(lobbyAll = r.lobby)))), Nil))

  /** 録画が始まった・終わったことを、会議室の全員に知らせる. */
  def recording(roomId: UUID, notice: Option[RecordingNotice], id: UUID): IO[Unit] =
    run(s => s.lives.get(roomId).fold((s, Nil))(l => apply(s, roomId, Conference.recording(l, notice, id))))

  /** 文字起こしの1行を、会議室の全員に送って保存する（文字起こし中のときだけ）. */
  def caption(roomId: UUID, line: TranscriptLine): IO[Unit] =
    IO.realTimeInstant.flatMap(now =>
      run(s => s.lives.get(roomId).fold((s, Nil))(l => apply(s, roomId, Conference.caption(l, line, now))))
    )

  /** 音声を送ってきた参加者（key は切符と一緒に発行した鍵. 会議に入っている間だけ使える）: (会議室, いまの会議, 参加者). */
  def speaker(key: String): IO[Option[(UUID, Live, PeerInfo)]] = cell.get.map { s =>
    for {
      peer <- s.keys.get(key)
      room <- s.roomOf.get(peer)
      live <- s.lives.get(room)
      p <- live.peer(peer)
    } yield (room, live, p)
  }

  /** 会議室を消した: 全員を退出させる. */
  def close(roomId: UUID, reason: String): IO[Unit] = run { s =>
    s.lives.get(roomId) match {
      case None => (s, Nil)
      case Some(l) =>
        val fx = (l.peers ++ l.waiting).toList.flatMap(p =>
          List(Effect.Send(p.id, ServerMsg.Closed(reason)), Effect.Disconnect(p.id))
        )
        (s.copy(lives = s.lives - roomId), fx)
    }
  }

  // ---------------------------------------------------------------- 読む

  def live(roomId: UUID): IO[Option[Live]] = cell.get.map(_.lives.get(roomId))

  /** 会議室ごとの入っている人数. */
  def counts: IO[Map[UUID, Int]] = cell.get.map(_.lives.view.mapValues(_.peers.size).filter(_._2 > 0).toMap)

  /** いまの数（メトリクス）: 会議中の会議室・会議に入っている人・待機室の人. */
  def stats: IO[(Long, Long, Long)] = cell.get.map { s =>
    val ls = s.lives.values
    (ls.count(_.peers.nonEmpty).toLong, ls.map(_.peers.size.toLong).sum, ls.map(_.waiting.size.toLong).sum)
  }

  // ---------------------------------------------------------------- 内部

  private def apply(s: State, room: UUID, r: (Live, List[Effect])): (State, List[Effect]) = {
    val (l, fx) = r
    (s.copy(lives = if l.isEmpty then s.lives - room else s.lives.updated(room, l)), fx)
  }

  /** 状態を変え、同じ順番のまま送る. DB への書き込みは裏のキューへ. */
  private def run(f: State => (State, List[Effect])): IO[Unit] = runWith(f).void

  /** run と同じ. 変える前と後の状態を返す. */
  private def runWith(f: State => (State, List[Effect])): IO[(State, State)] =
    cell.evalModify { s =>
      val (s2, fx) = f(s)
      fx.traverse_ {
        case Effect.Send(to, m) => (s2.conns.get(to) orElse s.conns.get(to)).traverse_(_.offer(Some(m)))
        case Effect.Disconnect(to) => (s2.conns.get(to) orElse s.conns.get(to)).traverse_(_.offer(None))
        case db => dbq.offer(db)
      }.as(s2 -> (s, s2))
    }
}

object Hub {

  /**
   * lives: 会議室 → いまの会議. conns: 接続 → 送るキュー. roomOf: 接続 → 会議室. seen: 接続 → 最後に何か届いた時刻.
   * keys: 切符と一緒に発行した鍵 → 接続（文字起こしの音声を送るときに使う. 本人の画面しか知らない）.
   */
  final case class State(lives: Map[UUID, Live] = Map.empty,
                         conns: Map[String, Queue[IO, Option[ServerMsg]]] = Map.empty,
                         roomOf: Map[String, UUID] = Map.empty,
                         seen: Map[String, Instant] = Map.empty,
                         keys: Map[String, String] = Map.empty)

  /** Hub と、DB に履歴を書く裏の処理. transcription は文字起こし（Whisper）が使えるか. */
  def resource(rooms: RoomStore,
               meetings: MeetingStore,
               notices: Notices = Notices.none,
               transcription: Boolean = false): Resource[IO, Hub] =
    for {
      cell <- Resource.eval(AtomicCell[IO].of(State()))
      tickets <- Resource.eval(Ref.of[IO, Map[String, Ticket]](Map.empty))
      q <- Resource.eval(Queue.unbounded[IO, Effect])
      _ <- writer(q, rooms, meetings).foreverM.background
      hub = new Hub(cell, tickets, q, meetings, notices, transcription)
      _ <- (IO.sleep(30.seconds) *> hub.reap(Silent)).foreverM.background
    } yield hub

  /** DB に履歴を書く（会議の開始・終了・入室・退出・チャットの数もここで数える）. */
  private def writer(q: Queue[IO, Effect], rooms: RoomStore, meetings: MeetingStore): IO[Unit] = {
    import sbirds.observe.Metrics.count
    q.take.flatMap {
      case Effect.StartMeeting(id, room, at) => count("skaka.meetings.started") *> meetings.start(id, room, at)
      case Effect.EndMeeting(id, at) => count("skaka.meetings.ended") *> meetings.end(id, at)
      case Effect.Joined(row, m, p, at) =>
        count("skaka.participants.joined", "guest" -> p.guest.toString) *>
          meetings.joined(row, m, p.userId, p.name, p.guest, at)
      case Effect.LeftMeeting(row, at) => count("skaka.participants.left") *> meetings.left(row, at)
      case Effect.SaveChat(m, msg, at) => count("skaka.chat.messages") *> meetings.chat(m, msg, at)
      case Effect.SaveTranscript(m, line, _) => count("skaka.transcript.lines") *> meetings.transcript(m, line)
      case _ => IO.unit
    }.handleErrorWith(e => sbirds.observe.Log.report(ErrorCode.MeetingHistoryWriteFailed, e.toString, e))
  }

  val TicketTtl: FiniteDuration = 2.minutes

  /** これより長く何も届かない接続は切る（画面の Ping は20秒ごと）. */
  val Silent: FiniteDuration = 75.seconds
}
