package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 会議の履歴（開いた会議・参加者・チャット）. 書き込みは会議中の Hub から. */
final class MeetingStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  /** 会議を始める（同じ id で何度呼んでもよい）. */
  def start(
    id: UUID,
    roomId: UUID,
    at: Instant
  ): IO[Unit] = tx(
    sql"insert into meeting (id, room_id, started_at) values ($id, $roomId, $at) on conflict (id) do nothing".update.run.void
  )

  def end(
    id: UUID,
    at: Instant
  ): IO[Unit] = tx(
    sql"update meeting set ended_at = $at where id = $id and ended_at is null".update.run *>
      sql"update meeting_participant set left_at = $at where meeting_id = $id and left_at is null".update.run.void
  )

  /** 起動したとき: 前に動いていたときの、終わっていない会議を終える. */
  def endAll(
    at: Instant
  ): IO[Int] = tx(
    sql"update meeting_participant set left_at = $at where left_at is null".update.run *>
      sql"update meeting set ended_at = $at where ended_at is null".update.run
  )

  def joined(
    id: UUID,
    meetingId: UUID,
    userId: Option[UUID],
    name: String,
    guest: Boolean,
    at: Instant
  ): IO[Unit] = tx(
    sql"""insert into meeting_participant (id, meeting_id, user_id, name, guest, joined_at)
          values ($id, $meetingId, $userId, $name, $guest, $at)""".update.run.void
  )

  def left(
    id: UUID,
    at: Instant
  ): IO[Unit] = tx(sql"update meeting_participant set left_at = $at where id = $id".update.run.void)

  def chat(
    meetingId: UUID,
    m: ChatMsg,
    at: Instant
  ): IO[Unit] = tx(
    sql"""insert into chat_message (id, meeting_id, user_id, name, body, created_at)
          values (${UUID.fromString(m.id)}, $meetingId, ${m.userId}, ${m.name}, ${m.text}, $at)""".update.run.void
  )

  def transcript(
    meetingId: UUID,
    l: TranscriptLine
  ): IO[Unit] = tx(
    sql"""insert into transcript_line (id, meeting_id, user_id, name, body, spoken_at)
          values (${UUID.fromString(l.id)}, $meetingId, ${l.userId}, ${l.name}, ${l.text}, ${Instant.parse(
        l.at
      )})""".update.run.void
  )

  // ---------------------------------------------------------------- 読む

  private val meetingCols =
    fr"""m.id, r.code, r.name, m.started_at, m.ended_at,
         (select count(distinct coalesce(p.user_id::text, p.name)) from meeting_participant p where p.meeting_id = m.id),
         (select count(*) from chat_message c where c.meeting_id = m.id)"""

  private type MRow = (UUID, String, String, Instant, Option[Instant], Long, Long)
  private def meeting(
    r: MRow
  ): MeetingDto = MeetingDto(r._1, r._2, r._3, r._4.toString, r._5.map(_.toString), r._6.toInt, r._7.toInt)

  /** 見られる会議か（自分の会議室の会議と、自分が参加した会議）. */
  def canSee(
    userId: UUID,
    id: UUID
  ): IO[Boolean] = tx(
    (fr"select exists (select 1 from meeting m join room r on r.id = m.room_id where m.id = $id and" ++
      visible(userId) ++ fr")").query[Boolean].unique
  )

  /** 見られる会議: 自分の会議室の会議と、自分が参加した会議. */
  private def visible(
    userId: UUID
  ): Fragment =
    fr"(r.owner_id = $userId or exists (select 1 from meeting_participant p where p.meeting_id = m.id and p.user_id = $userId))"

  /** 会議の一覧（新しい順）. room で会議室を絞る. */
  def list(
    userId: UUID,
    room: Option[UUID],
    before: Option[Instant],
    limit: Int
  ): IO[List[MeetingDto]] = tx(
    (fr"select" ++ meetingCols ++ fr"from meeting m join room r on r.id = m.room_id where" ++ visible(userId) ++
      room.fold(Fragment.empty)(id => fr"and m.room_id = $id") ++
      before.fold(Fragment.empty)(b => fr"and m.started_at < $b") ++
      fr"order by m.started_at desc limit $limit").query[MRow].to[List].map(_.map(meeting))
  )

  def detail(
    userId: UUID,
    id: UUID
  ): IO[Option[MeetingDetailDto]] = tx(
    (fr"select" ++ meetingCols ++ fr"from meeting m join room r on r.id = m.room_id where m.id = $id and" ++
      visible(userId))
      .query[MRow].option.flatMap {
        case None => none[MeetingDetailDto].pure[ConnectionIO]
        case Some(row) =>
          for {
            ps <-
              sql"""select name, user_id, guest, joined_at, left_at from meeting_participant where meeting_id = $id order by joined_at"""
                .query[(String, Option[UUID], Boolean, Instant, Option[Instant])].to[List]
            cs <-
              sql"""select id, name, user_id, body, created_at from chat_message where meeting_id = $id order by created_at"""
                .query[(UUID, String, Option[UUID], String, Instant)].to[List]
            ts <-
              sql"""select id, name, user_id, body, spoken_at from transcript_line where meeting_id = $id order by spoken_at"""
                .query[(UUID, String, Option[UUID], String, Instant)].to[List]
          } yield Some(MeetingDetailDto(
            meeting(row),
            ps.map(p => ParticipantDto(p._1, p._2, p._3, p._4.toString, p._5.map(_.toString))),
            cs.map(c => ChatMsg(c._1.toString, "", c._2, c._3, c._4, c._5.toString)),
            transcript = ts.map(t => TranscriptLine(t._1.toString, "", t._2, t._3, t._4, t._5.toString))
          ))
      }
  )
}

/** 兄弟のサービス（skuaka・skakapo）の API トークンでの連携: 暗号化したトークン、先頭（表示用）、そのサービスでの名前、書き込めるか. */
final case class TokenLink(
  tokenEnc: Array[Byte],
  prefix: String,
  account: String,
  canWrite: Boolean)
