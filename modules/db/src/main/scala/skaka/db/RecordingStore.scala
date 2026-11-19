package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 録画の行. */
final case class Recording(id: UUID,
                           roomId: UUID,
                           meetingId: Option[UUID],
                           userId: UUID,
                           title: String,
                           mime: String,
                           share: Boolean,
                           status: String,
                           bytes: Long,
                           durationMs: Option[Long],
                           startedAt: Instant,
                           endedAt: Option[Instant])

/** 会議の録画（中身は skawau. ここには状態と skawau のファイルの場所）. */
final class RecordingStore(xa: Transactor[IO]) extends TxStore(xa) {

  private val cols =
    fr"id, room_id, meeting_id, user_id, title, mime, share, status, bytes, duration_ms, started_at, ended_at"

  def create(r: Recording): IO[Unit] = tx(
    sql"""insert into recording (id, room_id, meeting_id, user_id, title, mime, share, status, started_at)
          values (${r.id}, ${r.roomId}, ${r.meetingId}, ${r.userId}, ${r.title}, ${r.mime}, ${r.share}, ${r.status}, ${r.startedAt})""".update.run.void
  )

  def find(id: UUID): IO[Option[Recording]] =
    tx((fr"select" ++ cols ++ fr"from recording where id = $id").query[Recording].option)

  /** 録画中・保存中のもの（起動したときに続きを行う）. */
  def open: IO[List[Recording]] =
    tx((fr"select" ++ cols ++ fr"from recording where status in ('recording', 'saving') order by started_at").query[
      Recording
    ].to[List])

  /** 録り終えた（skawau へ送る前）. */
  def stopped(id: UUID, bytes: Long, durationMs: Option[Long], at: Instant): IO[Unit] = tx(
    sql"""update recording set status = 'saving', bytes = $bytes, duration_ms = coalesce($durationMs, duration_ms), ended_at = coalesce(ended_at, $at),
          error = null where id = $id""".update.run.void
  )

  def saved(id: UUID, fileId: UUID, url: String, shared: Int): IO[Unit] = tx(
    sql"""update recording set status = 'saved', file_id = $fileId, file_url = $url, shared = $shared, error = null where id = $id""".update.run.void
  )

  def failed(id: UUID, error: String): IO[Unit] = tx(
    sql"update recording set status = 'failed', error = ${error.take(500)} where id = $id".update.run.void
  )

  def delete(id: UUID): IO[Unit] = tx(sql"delete from recording where id = $id".update.run.void)

  /** 会議に参加した、ログインしているメンバーのメールアドレス（録画した人を除く. skawau で共有する相手）. */
  def participantEmails(meetingId: UUID, except: UUID): IO[List[String]] = tx(
    sql"""select distinct u.email from meeting_participant p join app_user u on u.id = p.user_id
          where p.meeting_id = $meetingId and u.id <> $except and u.email is not null and not u.disabled""".query[
      String
    ].to[List]
  )

  // ---------------------------------------------------------------- 画面に出す

  /** 見られる録画: 自分が録画したもの、自分の会議室のもの、自分が参加した会議のもの. */
  private def visible(me: UUID): Fragment =
    fr"""(x.user_id = $me or r.owner_id = $me or exists (select 1 from meeting_participant p where p.meeting_id = x.meeting_id and p.user_id = $me))"""

  private type Row = (
    UUID,
    Option[UUID],
    String,
    String,
    String,
    String,
    Instant,
    Option[Instant],
    Long,
    Option[String],
    Option[String],
    Int,
    UUID
  )

  private def query(me: UUID, where: Fragment, limit: Int): ConnectionIO[List[RecordingDto]] =
    (fr"""select x.id, x.meeting_id, r.code, x.title, u.name, x.status, x.started_at, x.ended_at, x.bytes, x.file_url, x.error, x.shared, x.user_id
          from recording x join room r on r.id = x.room_id join app_user u on u.id = x.user_id where""" ++
      visible(me) ++ fr"and" ++ where ++
      fr"order by x.started_at desc limit $limit").query[Row].map(r =>
      RecordingDto(
        r._1,
        r._2,
        r._3,
        r._4,
        r._5,
        r._6,
        r._7.toString,
        r._8.map(_.toString),
        r._9,
        r._10,
        r._11,
        r._12,
        r._13 == me
      )
    ).to[List]

  def dto(me: UUID, id: UUID): IO[Option[RecordingDto]] = tx(query(me, fr"x.id = $id", 1).map(_.headOption))

  def forMeeting(me: UUID, meetingId: UUID): IO[List[RecordingDto]] = tx(query(me, fr"x.meeting_id = $meetingId", 100))

  /** 会議室ごと（room）・全体の、新しい順. */
  def list(me: UUID, room: Option[UUID], limit: Int): IO[List[RecordingDto]] =
    tx(query(me, room.fold(fr"true")(id => fr"x.room_id = $id"), limit))
}
