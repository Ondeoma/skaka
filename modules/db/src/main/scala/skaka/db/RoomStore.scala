package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/** 会議室の行. */
final case class Room(
  id: UUID,
  code: String,
  ownerId: UUID,
  name: String,
  description: String,
  personal: Boolean,
  guestAccess: Boolean,
  lobby: Boolean,
  event: Option[RoomEventDto],
  createdAt: Instant,
  lastUsedAt: Option[Instant]) {
  def dto(
    owner: UserBrief,
    me: Option[UUID],
    baseUrl: String,
    live: Int
  ): RoomDto =
    RoomDto(
      id,
      code,
      name,
      description,
      owner,
      personal,
      guestAccess,
      lobby,
      s"$baseUrl/r/$code",
      me.contains(ownerId),
      event,
      live,
      lastUsedAt.map(_.toString),
      createdAt.toString
    )
}

/** 会議室・入ったことのある会議室. */
final class RoomStore(
  xa: Transactor[IO],
  users: UserStore) extends TxStore(xa) {

  private val names = List(
    "id",
    "code",
    "owner_id",
    "name",
    "description",
    "personal",
    "guest_access",
    "lobby",
    "event_id",
    "event_title",
    "event_start",
    "event_end",
    "event_all_day",
    "created_at",
    "last_used_at"
  )
  private val cols = Fragment.const(names.map("r." + _).mkString(", "))
  private val returning = Fragment.const("returning " + names.mkString(", "))

  private type Row = (
    UUID,
    String,
    UUID,
    String,
    String,
    Boolean,
    Boolean,
    Boolean,
    Option[UUID],
    Option[String],
    Option[String],
    Option[String],
    Option[Boolean],
    Instant,
    Option[Instant]
  )

  private def room(
    r: Row
  ): Room = {
    val ev = (r._9, r._10, r._11, r._12, r._13).mapN(RoomEventDto.apply)
    Room(r._1, r._2, r._3, r._4, r._5, r._6, r._7, r._8, ev, r._14, r._15)
  }

  private def select(
    where: Fragment
  ): ConnectionIO[List[Room]] = (fr"select" ++ cols ++ fr"from room r" ++ where).query[Row].to[List].map(_.map(room))

  /** 持ち主の情報を付ける. */
  private def withOwnersC(
    rs: List[Room]
  ): ConnectionIO[List[(Room, UserBrief)]] =
    users.briefs(rs.map(_.ownerId)).map(m => rs.flatMap(r => m.get(r.ownerId).map(r -> _)))

  def byId(
    id: UUID
  ): IO[Option[(Room, UserBrief)]] = tx(select(fr"where r.id = $id").flatMap(withOwnersC).map(_.headOption))

  def byCode(
    code: String
  ): IO[Option[(Room, UserBrief)]] = tx(select(fr"where r.code = $code").flatMap(withOwnersC).map(_.headOption))

  /** 自分の会議室と、最近入った会議室（新しい順）. */
  def mine(
    userId: UUID,
    limit: Int = 100
  ): IO[List[(Room, UserBrief)]] = tx(
    select(
      fr"""left join room_visit v on v.room_id = r.id and v.user_id = $userId
                where r.owner_id = $userId or v.user_id is not null
                order by r.personal desc, greatest(r.last_used_at, v.visited_at, r.created_at) desc nulls last limit $limit"""
    )
      .flatMap(withOwnersC)
  )

  /** 予定に結び付いた会議室のコード. */
  def forEvent(
    eventId: UUID
  ): IO[Option[String]] =
    tx(sql"select code from room where event_id = $eventId order by created_at limit 1".query[String].option)

  /** 新しい会議室（コードがぶつかったら作り直す）. */
  def create(
    owner: UUID,
    r: RoomReq,
    personal: Boolean = false,
    event: Option[RoomEventDto] = None
  ): IO[Room] = {
    def attempt(
      n: Int
    ): IO[Room] = {
      val code = Codes.room()
      tx(
        (fr"""insert into room (id, code, owner_id, name, description, personal, guest_access, lobby, event_id, event_title, event_start, event_end, event_all_day)
              values (${UUID.randomUUID()}, $code, $owner, ${r.name}, ${r.description}, $personal, ${r.guestAccess}, ${r.lobby},
                      ${event.map(_.eventId)}, ${event.map(_.title)}, ${event.map(_.start)}, ${event.map(
            _.end
          )}, ${event.map(_.allDay)})
              on conflict (code) do nothing""" ++ returning).query[Row].option
      ).flatMap {
        case Some(row) => IO.pure(room(row))
        case None if n < 5 => attempt(n + 1)
        case None => IO.raiseError(new IllegalStateException("会議室のコードを作れませんでした"))
      }
    }
    attempt(1)
  }

  /** 自分の会議室（なければ作る）. */
  def ensurePersonal(
    u: User
  ): IO[Room] =
    tx(select(fr"where r.owner_id = ${u.id} and r.personal").map(_.headOption)).flatMap {
      case Some(r) => IO.pure(r)
      case None => create(u.id, RoomReq(s"${u.name} の会議室"), personal = true).handleErrorWith(_ =>
          tx(select(fr"where r.owner_id = ${u.id} and r.personal").map(_.head))
        ) // 同時に作られたとき
    }

  def update(
    id: UUID,
    r: RoomReq
  ): IO[Unit] = tx(
    sql"update room set name = ${r.name}, description = ${r.description}, guest_access = ${r.guestAccess}, lobby = ${r.lobby} where id = $id"
      .update.run.void
  )

  /** 予定の写しを変える. rename があれば会議室の名前も変える. */
  def setEvent(
    id: UUID,
    ev: Option[RoomEventDto],
    rename: Option[String] = None
  ): IO[Unit] = tx(
    sql"""update room set event_id = ${ev.map(_.eventId)}, event_title = ${ev.map(_.title)}, event_start = ${ev.map(
        _.start
      )},
          event_end = ${ev.map(_.end)}, event_all_day = ${ev.map(
        _.allDay
      )}, name = coalesce($rename, name) where id = $id""".update.run.void
  )

  /** 自分の会議室（personal 以外）を消す. */
  def delete(
    id: UUID
  ): IO[Boolean] = tx(sql"delete from room where id = $id and not personal".update.run.map(_ > 0))

  /** 入った（最近の会議室に出す）. */
  def visited(
    userId: UUID,
    roomId: UUID
  ): IO[Unit] = tx(
    sql"""insert into room_visit (user_id, room_id) values ($userId, $roomId)
          on conflict (user_id, room_id) do update set visited_at = now()""".update.run *>
      sql"update room set last_used_at = now() where id = $roomId".update.run.void
  )

  /** 最近の会議室から外す（自分の会議室は外せない）. */
  def forget(
    userId: UUID,
    roomId: UUID
  ): IO[Unit] = tx(sql"delete from room_visit where user_id = $userId and room_id = $roomId".update.run.void)
}

/** 推測されにくいコード. */
object Codes {
  private val rnd = new java.security.SecureRandom()
  private val letters = "abcdefghijkmnopqrstuvwxyz" // l は 1・I と紛れるので使わない

  private def word(
    n: Int
  ): String = (1 to n).map(_ => letters.charAt(rnd.nextInt(letters.length))).mkString

  def room(): String = s"${word(3)}-${word(4)}-${word(3)}"
}
