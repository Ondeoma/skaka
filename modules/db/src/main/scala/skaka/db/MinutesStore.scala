package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 議事録・要約（skakapo の会話の返事を取り込んだもの）. */
final case class Minutes(id: UUID, meetingId: UUID, userId: UUID, sessionId: UUID, sessionUrl: String, title: String, status: String, body: String,
                         error: Option[String], fileId: Option[UUID], fileUrl: Option[String], fileError: Option[String],
                         createdAt: Instant, updatedAt: Instant):
  def dto(viewer: UUID, by: String): MinutesDto =
    MinutesDto(id, by, viewer == userId, status, body, error, sessionUrl, createdAt.toString, updatedAt.toString, fileUrl, fileError)

final class MinutesStore(xa: Transactor[IO]) extends TxStore(xa):

  private val names = List("id", "meeting_id", "user_id", "session_id", "session_url", "title", "status", "body", "error", "file_id", "file_url",
    "file_error", "created_at", "updated_at")
  private val cols  = Fragment.const(names.mkString(", "))

  def create(m: Minutes): IO[Unit] = tx(
    sql"""insert into minutes (id, meeting_id, user_id, session_id, session_url, title, status, body, error, created_at, updated_at)
          values (${m.id}, ${m.meetingId}, ${m.userId}, ${m.sessionId}, ${m.sessionUrl}, ${m.title}, ${m.status}, ${m.body}, ${m.error},
                  ${m.createdAt}, ${m.updatedAt})""".update.run.void
  )

  def find(id: UUID): IO[Option[Minutes]] = tx((fr"select" ++ cols ++ fr"from minutes where id = $id").query[Minutes].option)

  /** 取り込んだ・失敗した. */
  def update(id: UUID, status: String, body: Option[String], error: Option[String]): IO[Unit] = tx(
    sql"""update minutes set status = $status, body = coalesce($body, body), error = $error, updated_at = now() where id = $id""".update.run.void
  )

  /** skawau に置いた（fileId）・置けなかった（error）. */
  def filed(id: UUID, fileId: Option[UUID], url: Option[String], error: Option[String]): IO[Unit] = tx(
    sql"""update minutes set file_id = coalesce($fileId, file_id), file_url = coalesce($url, file_url), file_error = $error where id = $id""".update.run.void
  )

  def delete(id: UUID): IO[Unit] = tx(sql"delete from minutes where id = $id".update.run.void)

  /** 会議の議事録（新しい順）と、頼んだ人の名前. */
  def forMeeting(viewer: UUID, meetingId: UUID): IO[List[MinutesDto]] = tx(
    (fr"select" ++ Fragment.const(names.map("n." + _).mkString(", ")) ++
      fr", u.name from minutes n join app_user u on u.id = n.user_id where n.meeting_id = $meetingId order by n.created_at desc")
      .query[(Minutes, String)].to[List].map(_.map((m, by) => m.dto(viewer, by)))
  )

  /** 取り込み中のもの（起動したときに続きを見る）. */
  def running: IO[List[Minutes]] = tx((fr"select" ++ cols ++ fr"from minutes where status = 'running'").query[Minutes].to[List])
