package skaka.web

import org.scalajs.dom

import scala.scalajs.js

/** DOM の補助. */
object Dom {
  def value(e: dom.Event): String = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
  def checked(e: dom.Event): Boolean = e.target.asInstanceOf[dom.HTMLInputElement].checked

  def copy(text: String, message: String = "コピーしました"): Unit = {
    dom.window.navigator.clipboard.writeText(text)
    Store.toast(message)
  }

  /** 入力欄の中で押されたキーか（ショートカットを無視するため）. */
  def typing(e: dom.KeyboardEvent): Boolean = {
    val t = Option(e.target.asInstanceOf[dom.Element]).map(_.tagName.toLowerCase).getOrElse("")
    Set("input", "textarea", "select").contains(t) ||
    e.target.asInstanceOf[js.Dynamic].isContentEditable.asInstanceOf[js.UndefOr[Boolean]].getOrElse(false)
  }
}
