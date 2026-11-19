package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*

import scala.scalajs.js
import scala.util.Try

/** 入退室・チャットの短い音（Web Audio で鳴らす. 設定で切れる）. */
object Sound {
  val enabled: RV[Boolean] = RV(Prefs.get("sound", "true") == "true")

  def set(
    v: Boolean
  ): Unit = { enabled := v; Prefs.set("sound", v.toString) }

  private lazy val ctx: Option[js.Dynamic] = Try(js.Dynamic.newInstance(js.Dynamic.global.AudioContext)()).toOption

  def play(
    kind: String
  ): Unit = if (enabled.v) ctx.foreach { c =>
    Try {
      if (c.state.asInstanceOf[String] == "suspended") c.resume()
      val notes = kind match {
        case "join" => List(660.0, 880.0)
        case "leave" => List(660.0, 440.0)
        case "knock" => List(520.0, 520.0)
        case _ => List(880.0)
      }
      notes.zipWithIndex.foreach {
        (
          f,
          i
        ) =>
          val t = c.currentTime.asInstanceOf[Double] + i * 0.12
          val osc = c.createOscillator()
          val g = c.createGain()
          osc.frequency.value = f
          g.gain.setValueAtTime(0.0001, t)
          g.gain.exponentialRampToValueAtTime(0.08, t + 0.02)
          g.gain.exponentialRampToValueAtTime(0.0001, t + 0.18)
          osc.connect(g)
          g.connect(c.destination)
          osc.start(t)
          osc.stop(t + 0.2)
      }
    }
  }
}
