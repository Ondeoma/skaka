package skaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.Method
import skaka.auth.SecretBox
import skaka.db.*
import skaka.push.{PushMessage, PushService}

import java.util.UUID

/**
 * 利用者へのプッシュ通知（PWA. 画面を閉じていても届く）. 送るのは裏で（会議の処理を待たせない）.
 *  - 待機室に人が入った: 会議室の持ち主に（会議に入っていなければ）
 *  - skuaka の予定に結び付いた会議室で会議が始まった: 予定に招待された人に（不参加と返事した人・会議に入っている人を除く）
 *  - 録画を skawau に保存した・保存できなかった: 録画した人に
 * push が無いとき（テストなど）は何もしない.
 */
final class Notices(push: Option[PushService], invitees: Room => IO[List[UUID]]):

  private def send(to: UUID, m: PushMessage): IO[Unit] = push.traverse_(_.send(to, m).start.void)

  def knock(room: Room, name: String): IO[Unit] =
    send(room.ownerId, PushMessage(s"$name さんが待機室で待っています", s"会議室「${room.name}」に入れるか決めてください", s"/r/${room.code}",
      Some(s"lobby:${room.id}")))

  def started(room: Room, present: Set[UUID]): IO[Unit] =
    IO.whenA(push.isDefined && room.event.isDefined) {
      val title = room.event.map(_.title).filter(_.trim.nonEmpty).getOrElse(room.name)
      invitees(room).flatMap(_.filterNot(present).distinct.traverse_(u =>
        send(u, PushMessage("会議が始まりました", s"「$title」の会議に参加できます", s"/r/${room.code}", Some(s"start:${room.id}")))))
    }.handleErrorWith(e => skaka.observe.Log.warn(s"会議の開始を知らせられませんでした: $e"))

  def recording(r: Recording, saved: Boolean): IO[Unit] =
    val m =
      if saved then PushMessage("録画を保存しました", s"「${r.title}」を skawau に保存しました", r.meetingId.fold("/meetings")(id => s"/meetings/$id"), Some(s"rec:${r.id}"))
      else PushMessage("録画を保存できませんでした", s"「${r.title}」は会議の履歴からもう一度保存できます", r.meetingId.fold("/meetings")(id => s"/meetings/$id"), Some(s"rec:${r.id}"))
    send(r.userId, m)

object Notices:
  val none: Notices = new Notices(None, _ => IO.pure(Nil))

  def apply(push: PushService, users: UserStore, links: LinkStore, skuaka: Option[SkuakaClient], box: SecretBox): Notices =
    new Notices(Some(push), room => invitees(room, users, links, skuaka, box))

  /**
   * 会議室に結び付いた skuaka の予定に招待された人（skaka でメールアドレスが一致する人）と、会議室の持ち主.
   * 予定は持ち主の skuaka の API トークンで読む（連携していなければ持ち主だけ）.
   */
  private def invitees(room: Room, users: UserStore, links: LinkStore, skuaka: Option[SkuakaClient], box: SecretBox): IO[List[UUID]] =
    val fromEvent = (skuaka, room.event).tupled.fold(IO.pure(List.empty[UUID])) { (c, ev) =>
      links.get(room.ownerId).flatMap(_.flatMap(l => box.open(l.tokenEnc)).fold(IO.pure(List.empty[UUID])) { token =>
        c.call(token, Method.GET, s"/api/events/${ev.eventId}").flatMap {
          case Left(_) => IO.pure(Nil)
          case Right(j) =>
            val emails = j.hcursor.downField("attendees").as[List[io.circe.Json]].getOrElse(Nil).map(_.hcursor)
              .filterNot(_.get[String]("response").contains("declined"))
              .flatMap(_.downField("user").get[String]("email").toOption)
            users.idsByEmail(emails)
        }
      })
    }
    fromEvent.map(room.ownerId :: _)
