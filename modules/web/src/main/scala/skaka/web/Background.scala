package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom

import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.Try

/**
 * カメラの背景をぼかす・画像に差し替える（この端末の設定）. 人物の切り抜きは MediaPipe の ImageSegmenter（frontend/segment.js. 使うときだけ読み込む）.
 * カメラの映像を小さくして切り抜き（人物らしさの 0〜1）、それを透明度にして、ぼかした映像・画像の上に人物を重ねた canvas を、送る映像にする.
 * 処理はすべてこの端末の中で行う（映像は外へ出さない）.
 */
object Background {

  /** none / blur（弱）/ blur-strong / 画像（あらかじめ用意したものの id か custom）. */
  val mode: RV[String] = RV(Prefs.get("bg", "none"))

  /** 自分で選んだ画像（1280×720 に縮めた JPEG の data URL. この端末に保存）. */
  val custom: RV[Option[String]] = RV(Option(Prefs.get("bg.custom", "")).filter(_.nonEmpty))

  /** 読み込み中（初回は切り抜きの WASM を読む）. */
  val loading: RV[Boolean] = RV(false)

  /** あらかじめ用意した背景（id, 名前, URL）. */
  val presets: List[(String, String, String)] = List(
    ("office", "部屋", "/backgrounds/office.svg"),
    ("sky", "青空", "/backgrounds/sky.svg"),
    ("gradient", "紺", "/backgrounds/gradient.svg"),
    ("skaka", "skaka", "/backgrounds/skaka.svg")
  )

  def active: Boolean = mode.v != "none"

  /** 背景を変える（会議中でもすぐ反映）. */
  def set(
    m: String
  ): Unit = { mode := m; Prefs.set("bg", m); Meeting.applyBackground() }

  /** 自分の画像を選んだ: 1280×720 に収まるよう切り取って JPEG にし、この端末に保存して使う. */
  def upload(
    file: dom.File
  ): Unit = {
    val url = dom.URL.createObjectURL(file)
    load(url).foreach { img =>
      dom.URL.revokeObjectURL(url)
      val c = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
      c.width = 1280
      c.height = 720
      cover(c.getContext("2d").asInstanceOf[js.Dynamic], img, 1280, 720)
      val data = c.toDataURL("image/jpeg", 0.85)
      Try(dom.window.localStorage.setItem("skaka.bg.custom", data)).fold(
        _ => Store.toast("画像が大きすぎて保存できません（この会議の間だけ使います）", error = true),
        _ => ()
      )
      custom := Some(data)
      images -= "custom"
      set("custom")
    }
  }

  // ---------------------------------------------------------------- 送る映像を作る

  private var segmenter: Option[js.Dynamic] = None
  private var stop: Option[() => Unit] = None
  private var output: Option[js.Dynamic] = None
  private var source: Option[dom.HTMLVideoElement] = None
  private var images: Map[String, dom.HTMLImageElement] = Map.empty

  private val Small = 256 // 切り抜きにかける映像の幅（小さいほど速い. 縁は拡大するときになめらかになる）

  /** カメラのトラックから送るトラックを作る. 背景なし・使えないときはカメラのまま. */
  def attach(
    camera: js.Dynamic
  ): Future[js.Dynamic] =
    if (!active) { detach(); Future.successful(camera) }
    else {
      loading := true
      val r = model().flatMap(seg => background().map(img => start(seg, camera, img))).recover { case e =>
        dom.console.warn(s"background: $e")
        giveUp()
        camera
      }
      r.onComplete(_ => loading := false)
      r
    }

  /** 作った映像を止める（カメラは止めない）. */
  def detach(): Unit = {
    stop.foreach(_())
    stop = None
    source.foreach(_.asInstanceOf[js.Dynamic].srcObject = null)
    source = None
    output.foreach(_.stop())
    output = None
  }

  /** 使えなかった: 知らせて、カメラのままに戻す. */
  private def giveUp(): Unit = {
    Store.toast("背景を変えられませんでした（このブラウザ・端末では使えないか、読み込みに失敗しました）", error = true)
    mode := "none"
    Prefs.set("bg", "none")
    detach()
  }

  /** 切り抜きは WebGL2 で映像を取り込む（CPU で処理するときも）. 使えなければ初めから断る. */
  private def webgl: Boolean =
    Try(dom.document.createElement("canvas").asInstanceOf[js.Dynamic].getContext("webgl2") != null).getOrElse(false)

  private def model(): Future[js.Dynamic] = segmenter match {
    case Some(s) => Future.successful(s)
    case None if !webgl => Future.failed(new Exception("WebGL2 が使えません"))
    case None =>
      js.Dynamic.global.skakaSegmenter().asInstanceOf[js.Promise[js.Dynamic]].toFuture.map { s =>
        segmenter = Some(s); s
      }
  }

  private def load(
    url: String
  ): Future[dom.HTMLImageElement] = {
    val p = Promise[dom.HTMLImageElement]()
    val img = dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
    img.onload = (_: dom.Event) => p.trySuccess(img)
    img.asInstanceOf[js.Dynamic].onerror = (_: dom.Event) => p.tryFailure(new Exception("画像を読み込めません"))
    img.src = url
    p.future
  }

  /** 背景の画像（ぼかしなら None）. */
  private def background(): Future[Option[dom.HTMLImageElement]] = {
    val m = mode.v
    val url = if (m == "custom") custom.v else presets.find(_._1 == m).map(_._3)
    url match {
      case None => Future.successful(None)
      case Some(u) =>
        images.get(m).fold(load(u).map { i => images += m -> i; Some(i) })(i => Future.successful(Some(i)))
    }
  }

  private def cover(
    g: js.Dynamic,
    image: dom.HTMLImageElement,
    w: Double,
    h: Double
  ): Unit = {
    val img = image.asInstanceOf[js.Dynamic]
    val (iw, ih) = (img.width.asInstanceOf[Double], img.height.asInstanceOf[Double])
    val k = math.max(w / iw, h / ih)
    g.drawImage(img, (w - iw * k) / 2, (h - ih * k) / 2, iw * k, ih * k)
  }

  private def canvas(): (dom.HTMLCanvasElement, js.Dynamic) = {
    val c = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    (c, c.getContext("2d").asInstanceOf[js.Dynamic])
  }

  private def start(
    seg: js.Dynamic,
    camera: js.Dynamic,
    img: Option[dom.HTMLImageElement]
  ): js.Dynamic = {
    detach()
    val video = dom.document.createElement("video").asInstanceOf[dom.HTMLVideoElement]
    video.muted = true
    video.asInstanceOf[js.Dynamic].playsInline = true
    video.asInstanceOf[js.Dynamic].srcObject = js.Dynamic.newInstance(js.Dynamic.global.MediaStream)(js.Array(camera))
    video.play()
    val (out, g) = canvas()
    val (small, sg) = canvas() // 切り抜きにかける小さな映像
    val (mask, mg) = canvas() // 人物らしさを透明度にしたもの
    val (person, pg) = canvas() // 人物だけ
    val (blur, bg) = canvas() // ぼかし用の縮小（canvas の filter が使えないブラウザ）
    val strong = mode.v == "blur-strong"
    val filter = !js.isUndefined(g.filter)
    val fps = Quality.fps.v
    // 最初の大きさ（カメラの映像が来るまで）
    out.width = 640
    out.height = 360
    val track = out.asInstanceOf[js.Dynamic].captureStream(fps).getVideoTracks().asInstanceOf[js.Array[js.Dynamic]](0)
    var data: Option[dom.ImageData] = None
    var fails = 0

    def frame(): Unit = if (video.readyState.asInstanceOf[Int] >= 2 && video.videoWidth > 0) Try {
      val (w, h) = (video.videoWidth, video.videoHeight)
      if (out.width != w || out.height != h) { out.width = w; out.height = h; person.width = w; person.height = h }
      val sw = Small
      val sh = math.max(1, (Small * h.toDouble / w).round.toInt)
      if (small.width != sw || small.height != sh) {
        small.width = sw; small.height = sh; mask.width = sw; mask.height = sh; data = None
      }
      sg.drawImage(video, 0, 0, sw, sh)
      // 切り抜き: 人物らしさ（0〜1）を透明度に（0.25〜0.75 をなめらかにつなぐ）
      val res = seg.segmentForVideo(small, dom.window.performance.now())
      Try {
        val m = res.confidenceMasks.asInstanceOf[js.Array[js.Dynamic]](0)
        val f = m.getAsFloat32Array().asInstanceOf[js.typedarray.Float32Array]
        val id = data.getOrElse { val d = mg.createImageData(sw, sh).asInstanceOf[dom.ImageData]; data = Some(d); d }
        val px = id.data.asInstanceOf[js.typedarray.Uint8ClampedArray]
        var i = 0
        val n = math.min(f.length, sw * sh)
        while (i < n) {
          val x = (f(i) - 0.25) / 0.5
          val a = if (x <= 0) 0.0 else if (x >= 1) 1.0 else x * x * (3 - 2 * x)
          px(i * 4 + 3) = (a * 255).toInt
          i += 1
        }
        mg.putImageData(id, 0, 0)
      }
      Try(res.close())
      // 背景: 画像か、ぼかした映像
      img match {
        case Some(i) => cover(g, i, w, h)
        case None if filter =>
          g.filter = s"blur(${if (strong) w / 40 else w / 90}px)"
          g.drawImage(video, 0, 0, w, h)
          g.filter = "none"
        case None =>
          val k = if (strong) 24 else 12
          blur.width = math.max(1, w / k)
          blur.height = math.max(1, h / k)
          bg.drawImage(video, 0, 0, blur.width, blur.height)
          g.imageSmoothingEnabled = true
          g.drawImage(blur, 0, 0, w, h)
      }
      // 人物: 映像を切り抜きの形で抜いて重ねる（切り抜きは拡大するときになめらかになる）
      pg.globalCompositeOperation = "source-over"
      pg.clearRect(0, 0, w, h)
      pg.drawImage(video, 0, 0, w, h)
      pg.globalCompositeOperation = "destination-in"
      pg.imageSmoothingEnabled = true
      pg.drawImage(mask, 0, 0, w, h)
      g.drawImage(person, 0, 0)
      fails = 0
    }.failed.foreach { e =>
      // 続けて失敗したら（GPU が使えなくなったなど）、カメラのままに戻す
      fails += 1
      if (fails == 1) dom.console.warn(s"background frame: $e")
      if (fails == 30) { giveUp(); Meeting.applyBackground() }
    }

    stop = Some(Ticker.every(1000 / fps)(() => frame()))
    output = Some(track)
    source = Some(video)
    track
  }
}
