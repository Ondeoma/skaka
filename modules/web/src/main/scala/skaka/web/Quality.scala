package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*

/**
 * 送る映像・音声の品質（この端末の設定. localStorage）. 変えると会議中でもすぐに反映する（Meeting.applyQuality）.
 * メッシュなので送る量は人数に比例する. 映像の上限（maxBitrate）は解像度・人数・画質の設定から決める.
 */
object Quality {

  private def int(
    key: String,
    default: Int
  ): Int = Prefs.get(key, default.toString).toIntOption.getOrElse(default)
  private def bool(
    key: String,
    default: Boolean
  ): Boolean = Prefs.get(key, default.toString) == "true"

  /** カメラの解像度（縦の画素数）・フレームレート. */
  val resolution: RV[Int] = RV(int("q.res", 720))
  val fps: RV[Int] = RV(int("q.fps", 24))

  /** カメラの画質: saver（節約）/ auto / high. */
  val video: RV[String] = RV(Prefs.get("q.video", "auto"))

  /** 画面共有: detail（文字をくっきり）/ motion（動きをなめらかに）. */
  val screen: RV[String] = RV(Prefs.get("q.screen", "detail"))

  /** マイクの処理（エコー除去・ノイズ抑制・音量の自動調整）と、高音質（音楽向け. 128kbps）. */
  val echo: RV[Boolean] = RV(bool("q.echo", true))
  val noise: RV[Boolean] = RV(bool("q.noise", true))
  val gain: RV[Boolean] = RV(bool("q.gain", true))
  val hifi: RV[Boolean] = RV(bool("q.hifi", false))

  val resolutions: List[(Int, String)] =
    List(360 -> "360p（低）", 540 -> "540p", 720 -> "720p（HD）", 1080 -> "1080p（フル HD）")
  val frameRates: List[(Int, String)] = List(15 -> "15 fps", 24 -> "24 fps", 30 -> "30 fps")
  val videoModes: List[(String, String)] = List("saver" -> "節約（回線が細いとき）", "auto" -> "自動", "high" -> "高画質")
  val screenModes: List[(String, String)] = List("detail" -> "文字をくっきり（資料・コード）", "motion" -> "動きをなめらかに（動画）")

  /** 設定を変える. kind は反映するもの（video / audio / screen / bitrate）. */
  def setInt(
    rv: RV[Int],
    key: String,
    v: Int,
    kind: String
  ): Unit = { rv := v; Prefs.set(key, v.toString); Meeting.applyQuality(kind) }
  def setStr(
    rv: RV[String],
    key: String,
    v: String,
    kind: String
  ): Unit = { rv := v; Prefs.set(key, v); Meeting.applyQuality(kind) }
  def setBool(
    rv: RV[Boolean],
    key: String,
    v: Boolean,
    kind: String
  ): Unit = { rv := v; Prefs.set(key, v.toString); Meeting.applyQuality(kind) }

  /** カメラの映像の上限（bps）. 解像度ごとの基準に、相手の人数（送る本数）と画質の設定を掛ける. */
  def videoBitrate(
    peers: Int
  ): Int = {
    val base = resolution.v match {
      case r if r <= 360 => 500
      case r if r <= 540 => 900
      case r if r <= 720 => 1500
      case _ => 2800
    }
    val people = if (peers <= 1) 1.0 else if (peers <= 3) 0.55 else if (peers <= 6) 0.3 else 0.17
    val mode = video.v match { case "saver" => 0.5; case "high" => 1.6; case _ => 1.0 }
    (math.max(150.0, base * people * mode) * 1000 * fps.v.toDouble / 24.0).toInt.max(150000)
  }

  /** マイクの音の上限（bps. 高音質なら 128kbps、ふだんはブラウザの既定に任せる）. */
  def audioBitrate: Option[Int] = Option.when(hifi.v)(128000)
}
