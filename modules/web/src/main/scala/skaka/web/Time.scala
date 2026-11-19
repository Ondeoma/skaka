package skaka.web

import scala.scalajs.js

/**
 * 日付・時刻（画面のタイムゾーン = ブラウザのタイムゾーン）.
 * 日付は "yyyy-MM-dd" の文字列、日時は ISO 8601 の文字列で持つ（Scala.js で java.time を使わないため）.
 */
object Time {

  val WeekNames: Vector[String] = Vector("日", "月", "火", "水", "木", "金", "土")

  def iso(d: js.Date): String = f"${d.getFullYear().toInt}%04d-${d.getMonth().toInt + 1}%02d-${d.getDate().toInt}%02d"

  def today(): String = iso(new js.Date())

  def addDays(d: String, n: Int): String =
    iso(new js.Date(d.take(4).toInt, d.slice(5, 7).toInt - 1, d.slice(8, 10).toInt + n))

  /** 日時（ISO）のこの端末での日付. 終日の予定の日付（yyyy-MM-dd）はそのまま. */
  def dateOf(s: String): String = if (s.length == 10) s else iso(new js.Date(s))

  def hm(d: js.Date): String = f"${d.getHours().toInt}%02d:${d.getMinutes().toInt}%02d"

  /** 13:05. */
  def time(instant: String): String = hm(new js.Date(instant))

  /** 11月15日(日). */
  def label(d: String): String = {
    val x = new js.Date(d.take(4).toInt, d.slice(5, 7).toInt - 1, d.slice(8, 10).toInt)
    s"${x.getMonth().toInt + 1}月${x.getDate().toInt}日(${WeekNames(x.getDay().toInt)})"
  }

  /** 今日・明日・11月17日(火). */
  def dayLabel(d: String): String =
    if (d == today()) "今日" else if (d == addDays(today(), 1)) "明日" else label(d)

  /** 日時の表示（今年なら年を省く）: 11月15日(日) 13:05. */
  def dateTime(instant: String): String = label(dateOf(instant)) + " " + time(instant)

  /** 日付と時刻（HH:mm）→ 日時（ISO, UTC）. */
  def instant(d: String, hhmm: String): String = {
    val Array(h, m) = (hhmm.split(':') ++ Array("0", "0")).take(2).map(_.toIntOption.getOrElse(0))
    new js.Date(d.take(4).toInt, d.slice(5, 7).toInt - 1, d.slice(8, 10).toInt, h, m).toISOString()
  }

  def epoch(instant: String): Double = js.Date.parse(instant)

  /** 経過時間（1:02:03・12:34）. */
  def elapsed(ms: Double): String = {
    val t = math.max(0, (ms / 1000).toInt)
    if (t >= 3600) f"${t / 3600}%d:${t / 60 % 60}%02d:${t % 60}%02d" else f"${t / 60}%d:${t % 60}%02d"
  }

  /** 分を「1時間30分」に. */
  def minutes(m: Int): String = if (m < 60) s"${m}分" else if (m % 60 == 0) s"${m / 60}時間" else s"${m / 60}時間${m % 60}分"

  /** ブラウザのタイムゾーン（Asia/Tokyo）. */
  def timezone(): String =
    js.Dynamic.global.Intl.DateTimeFormat().resolvedOptions().timeZone.asInstanceOf[String]

  /** ブラウザが知っているタイムゾーンの一覧. */
  def timezones(): List[String] =
    scala.util.Try(
      js.Dynamic.global.Intl.supportedValuesOf("timeZone").asInstanceOf[js.Array[String]].toList
    ).getOrElse(List(timezone()))
}
