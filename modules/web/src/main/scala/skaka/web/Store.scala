package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skaka.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

final case class Toast(id: Int, text: String, error: Boolean)

/** 画面全体で共有する状態（自分・設定・会議室の一覧）と、トースト. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val rooms: RV[List[RoomDto]] = RV(Nil)
  val toasts: RV[List[Toast]] = RV(Nil)

  /** 開いているダイアログ（会議室の作成・変更、会議の予定）. */
  val roomEditor: RV[Option[Option[RoomDto]]] = RV(None)
  val scheduler: RV[Option[ScheduleDraft]] = RV(None)

  def loadRooms(): Unit = if (me.v.isDefined) Api.rooms().foreach(rs => rooms := rs)

  def skuakaEnabled: Boolean = config.v.exists(_.skuaka.isDefined)
  def skawauEnabled: Boolean = config.v.exists(_.skawau.isDefined)
  def transcriptionEnabled: Boolean = config.v.exists(_.transcription)
  def skakapoEnabled: Boolean = config.v.exists(_.skakapo.isDefined)

  // ---------------------------------------------------------------- トースト

  private var toastSeq = 0

  def toast(text: String, error: Boolean = false): Unit = {
    toastSeq += 1
    val t = Toast(toastSeq, text, error)
    toasts := (toasts.v :+ t).takeRight(4)
    dom.window.setTimeout(() => dismiss(t.id), if (error) 9000 else 5000)
  }

  def dismiss(id: Int): Unit = toasts := toasts.v.filterNot(_.id == id)

  def error(e: Throwable): Unit = e match {
    case ApiError(401, _) => ()
    case other => toast(other.getMessage, error = true)
  }

  /** busy の間は送らない（二重送信を防ぐ）. 終わったら busy を戻し、失敗はトーストで知らせる. */
  def submit[A](busy: RV[Boolean])(f: => Future[A])(ok: A => Unit): Unit = if (!busy.v) {
    busy := true
    val r = f
    r.onComplete(_ => busy := false)
    report(r)(ok)
  }

  /** Future の失敗をトーストで知らせる. */
  def report[A](f: Future[A])(ok: A => Unit): Unit = f.onComplete {
    case Success(a) => ok(a)
    case Failure(e) => error(e)
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

/** 画面内の確認のダイアログ（ブラウザ標準の confirm の代わり）. 表示は parts/DialogHost.st. */
final case class DialogReq(message: String, okLabel: String, danger: Boolean, onOk: () => Unit)

object Dialog {
  val current: RV[Option[DialogReq]] = RV(None)

  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    current := Some(DialogReq(message, ok, danger, () => f))

  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk()
  }
}

/** この端末だけの設定（localStorage. 使えないときは既定値）. */
object Prefs {
  def get(key: String, default: String): String =
    scala.util.Try(Option(dom.window.localStorage.getItem("skaka." + key))).toOption.flatten.getOrElse(default)

  def set(key: String, value: String): Unit = scala.util.Try(dom.window.localStorage.setItem("skaka." + key, value))
}

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme {
  val current: RV[String] = RV(Prefs.get("theme", "system"))

  def apply(): Unit = {
    val el = dom.document.documentElement
    if (current.v == "system") el.removeAttribute("data-theme") else el.setAttribute("data-theme", current.v)
  }

  def set(v: String): Unit = { current := v; Prefs.set("theme", v); apply() }

  val options: List[(String, String)] = List("system" -> "自動（OS に合わせる）", "light" -> "ライト", "dark" -> "ダーク")
}
