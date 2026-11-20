package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import skaka.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

/** 画面全体で共有する状態（自分・設定・会議室の一覧）. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val rooms: RV[List[RoomDto]] = RV(Nil)

  /** 開いているダイアログ（会議室の作成・変更、会議の予定）. */
  val roomEditor: RV[Option[Option[RoomDto]]] = RV(None)
  val scheduler: RV[Option[ScheduleDraft]] = RV(None)

  def loadRooms(): Unit = if (me.v.isDefined) Api.rooms().foreach(rs => rooms := rs)

  def skuakaEnabled: Boolean = config.v.exists(_.skuaka.isDefined)
  def skawauEnabled: Boolean = config.v.exists(_.skawau.isDefined)
  def transcriptionEnabled: Boolean = config.v.exists(_.transcription)
  def skakapoEnabled: Boolean = config.v.exists(_.skakapo.isDefined)

  /** 実行中は送らない（二重送信を防ぐ）. 失敗はお知らせで知らせる. */
  def submit[A](act: ActionState)(f: => Future[A])(ok: A => Unit): Unit = act.run(f).onComplete {
    case Success(a) => ok(a)
    case Failure(ActionState.Busy) => ()
    case Failure(e) => Toasts.error(e)
  }
}

/** 会議の予定を立てる画面の入力. */
final case class ScheduleDraft(title: String = "",
                               date: String = Time.today(),
                               time: String = ScheduleDraft.nextSlot(),
                               minutes: Int = 30,
                               description: String = "",
                               attendees: List[UserBrief] = Nil,
                               guestAccess: Boolean = false,
                               lobby: Boolean = false)

object ScheduleDraft {

  /** 次の30分の区切り（14:10 → 14:30）. */
  def nextSlot(): String = {
    val d = new js.Date()
    val m = (d.getHours().toInt * 60 + d.getMinutes().toInt + 30) / 30 * 30
    f"${math.min(m, 23 * 60 + 30) / 60}%02d:${math.min(m, 23 * 60 + 30) % 60}%02d"
  }
}

/** この端末だけの設定（localStorage. 使えないときは既定値）. */
object Prefs extends sbirds.web.LocalPrefs("skaka")

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme extends sbirds.web.ThemeSwitch(Prefs, sbirds.web.ThemeSwitch.Plain)
