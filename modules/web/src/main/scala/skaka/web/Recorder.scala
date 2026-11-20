package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skaka.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success, Try}

/**
 * 会議の録画（録画する人の画面で行う）. 映像は会議の枠を canvas に並べて描き（画面共有があれば大きく）、音は全員の音を Web Audio で1つにまとめ、
 * MediaRecorder で WebM（Safari は MP4）にする. 4秒ごとの断片を順にサーバーへ送り、録り終えたらサーバーが skawau に置いて参加者と共有する.
 * 画面を閉じても、それまでに送った分はサーバーが保存する.
 */
object Recorder {

  /** 録画中であること（誰が録画していても. サーバーから知らされる）. */
  val notice: RV[Option[RecordingNotice]] = RV(None)

  /** 自分が録画している. */
  val mine: RV[Boolean] = RV(false)

  /** 録り終えて、残りの断片を送っている. */
  val finishing: RV[Boolean] = RV(false)

  private val W = 1280
  private val H = 720
  private val Fps = 15
  private val SliceMs = 4000

  private var rec: Option[UUID] = None
  private var media: Option[js.Dynamic] = None
  private var canvas: Option[dom.HTMLCanvasElement] = None
  private var ticker: Option[() => Unit] = None
  private var audio: Option[js.Dynamic] = None
  private var dest: Option[js.Dynamic] = None
  private var sources: Map[String, js.Dynamic] = Map.empty // 音のトラックの id → source
  private var videos: Map[String, dom.HTMLVideoElement] = Map.empty // MediaStream の id → video
  private var startedAt = 0.0
  private var seq = 0
  private var sending: Future[Unit] = Future.unit
  private var failed = false
  private var frame = 0

  /** 録画できるブラウザか. */
  def supported: Boolean = !js.isUndefined(js.Dynamic.global.MediaRecorder) &&
    !js.isUndefined(dom.document.createElement("canvas").asInstanceOf[js.Dynamic].captureStream)

  /** 使える形式（WebM を優先. Safari は MP4）. */
  private def mimeType: Option[String] =
    List(
      "video/webm;codecs=vp8,opus",
      "video/webm;codecs=vp9,opus",
      "video/webm",
      "video/mp4;codecs=avc1,mp4a.40.2",
      "video/mp4"
    )
      .find(t => js.Dynamic.global.MediaRecorder.isTypeSupported(t).asInstanceOf[Boolean])

  // ---------------------------------------------------------------- 始める・終える

  /** 録画を始める（サーバーに知らせてから録り始める）. */
  def start(share: Boolean = true): Unit =
    if (!supported) Toasts.show("このブラウザは録画に対応していません（Chrome・Edge・Firefox・Safari の新しい版を使ってください）", error = true)
    else if (rec.isEmpty && !finishing.v) mimeType match {
      case None => Toasts.show("このブラウザで使える録画の形式がありません", error = true)
      case Some(mime) =>
        Api.startRecording(RecordingStartReq(Meeting.code.v, mime, share)).onComplete {
          case Failure(e) => Toasts.error(e)
          case Success(r) =>
            Try(begin(r.id, mime)) match {
              case Success(_) => Toasts.show("録画を始めました。参加者全員に「録画中」と表示されます")
              case Failure(e) =>
                release()
                Api.deleteRecording(r.id).failed.foreach(_ => ())
                Toasts.show(s"録画を始められませんでした: ${e.getMessage}", error = true)
            }
        }
    }

  /** 録画を終える（残りの断片を送ってから、サーバーに保存を頼む）. */
  def stop(): Unit = media.foreach { m =>
    if (m.state.asInstanceOf[String] != "inactive") m.stop() // 最後の断片（ondataavailable）のあとで onstop
  }

  private def begin(id: UUID, mime: String): Unit = {
    val c = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    c.width = W
    c.height = H
    canvas = Some(c)
    val ctx = js.Dynamic.newInstance(if (!js.isUndefined(js.Dynamic.global.AudioContext)) js.Dynamic.global.AudioContext
    else js.Dynamic.global.webkitAudioContext)()
    audio = Some(ctx)
    val d = ctx.createMediaStreamDestination()
    dest = Some(d)
    syncAudio()
    draw()
    val video = c.asInstanceOf[js.Dynamic].captureStream(Fps)
    val stream = js.Dynamic.newInstance(js.Dynamic.global.MediaStream)(
      video.getVideoTracks().asInstanceOf[js.Array[js.Any]].concat(
        d.stream.getAudioTracks().asInstanceOf[js.Array[js.Any]]
      )
    )
    val m = js.Dynamic.newInstance(js.Dynamic.global.MediaRecorder)(
      stream,
      js.Dynamic.literal(mimeType = mime, videoBitsPerSecond = 2500000, audioBitsPerSecond = 128000)
    )
    rec = Some(id)
    media = Some(m)
    seq = 0
    failed = false
    sending = Future.unit
    m.ondataavailable = (e: js.Dynamic) => {
      val blob = e.data.asInstanceOf[dom.Blob]
      if (blob.size > 0 && !failed) enqueue(id, blob)
    }
    m.onstop = (_: js.Any) => finish(id)
    m.onerror = (e: js.Dynamic) => { Toasts.show("録画が止まりました", error = true); stop() }
    m.start(SliceMs)
    startedAt = js.Date.now()
    mine := true
    // 背景のタブでも描き続ける
    ticker = Some(Ticker.every(1000 / Fps)(() => tick()))
    // 録画中に画面を閉じようとしたら確かめる
    unloadGuard =
      Some(useEventListener[dom.BeforeUnloadEvent](dom.window, "beforeunload")(e => if (mine.v) e.preventDefault()))
  }

  private var unloadGuard: Option[EffectHandle] = None

  /** 断片を順に送る（失敗したら少し待って送り直す. 録画が終わっている・上限を超えたなどは止める）. */
  private def enqueue(id: UUID, blob: dom.Blob): Unit = {
    val n = seq
    seq += 1
    def attempt(left: Int, wait: Int): Future[Unit] =
      Api.putChunk(id, n, blob).recoverWith {
        case ApiError(code, _) if (code == 0 || code >= 500) && left > 0 =>
          val p = scala.concurrent.Promise[Unit]()
          dom.window.setTimeout(() => p.completeWith(attempt(left - 1, wait * 2)), wait)
          p.future
      }
    sending = sending.flatMap(_ => if (failed) Future.unit else attempt(6, 1000)).recover { case e =>
      failed = true
      Toasts.show(s"録画を送れませんでした（${e.getMessage}）。録画を止めます", error = true)
      stop()
    }
  }

  private def finish(id: UUID): Unit = {
    val duration = (js.Date.now() - startedAt).toLong
    release()
    finishing := true
    sending.flatMap(_ => Api.finishRecording(id, duration)).onComplete { r =>
      finishing := false
      r match {
        case Success(_) => Toasts.show(s"録画を終えました。${AppNames.nameOf("skawau")} に保存しています（会議の記録から開けます）")
        case Failure(e) => Toasts.error(e)
      }
    }
  }

  /** 録画の道具を片付ける（送っている断片は送り終える）. */
  private def release(): Unit = {
    unloadGuard.foreach(_.stop())
    unloadGuard = None
    ticker.foreach(_())
    ticker = None
    media.foreach(m => Try(m.stream.getTracks().asInstanceOf[js.Array[js.Dynamic]].foreach(_.stop())))
    media = None
    sources.values.foreach(s => Try(s.disconnect()))
    sources = Map.empty
    audio.foreach(a => Try(a.close()))
    audio = None
    dest = None
    videos.values.foreach(v => v.asInstanceOf[js.Dynamic].srcObject = null)
    videos = Map.empty
    canvas = None
    rec = None
    mine := false
  }

  /** 会議を出た: 録画していれば終える. */
  def leave(): Unit = { stop(); notice := None }

  // ---------------------------------------------------------------- 描く

  private def tick(): Unit = {
    frame += 1
    draw()
    if (frame % Fps == 0) syncAudio() // 1秒ごとに、入ってきた人・出た人の音を合わせる
  }

  /** 全員の音（自分のマイク・相手・自分の画面共有の音）を1つにまとめる. トラックが変わったら付け替える. */
  private def syncAudio(): Unit = (audio, dest) match {
    case (Some(ctx), Some(d)) =>
      val tracks = (Meeting.local.toList ++ Meeting.screen.toList ++ Meeting.remoteStreams).flatMap(s =>
        s.getAudioTracks().asInstanceOf[js.Array[js.Dynamic]].toList
      ).filter(_.readyState.asInstanceOf[String] == "live")
      val ids = tracks.map(_.id.asInstanceOf[String]).toSet
      sources.filterNot((k, _) => ids(k)).foreach {
        (k, s) => Try(s.disconnect()); sources -= k
      }
      tracks.filterNot(t => sources.contains(t.id.asInstanceOf[String])).foreach { t =>
        Try {
          val src = ctx.createMediaStreamSource(js.Dynamic.newInstance(js.Dynamic.global.MediaStream)(js.Array(t)))
          src.connect(d)
          sources += t.id.asInstanceOf[String] -> src
        }
      }
    case _ => ()
  }

  /** 枠の映像（なければ None）. */
  private def streamOf(t: Tile): Option[js.Dynamic] =
    if (t.key == "self") Meeting.local
    else if (t.screen && t.peer == "self") Meeting.screen else Meeting.streamOf(t.peer, t.screen)

  private def videoOf(s: js.Dynamic): Option[dom.HTMLVideoElement] =
    if (s.getVideoTracks().asInstanceOf[js.Array[js.Dynamic]].forall(_.readyState.asInstanceOf[String] != "live")) None
    else {
      val id = s.id.asInstanceOf[String]
      val v = videos.getOrElse(
        id, {
          val el = dom.document.createElement("video").asInstanceOf[dom.HTMLVideoElement]
          el.muted = true
          el.autoplay = true
          el.asInstanceOf[js.Dynamic].playsInline = true
          el.asInstanceOf[js.Dynamic].srcObject = s
          el.play()
          videos += id -> el
          el
        }
      )
      Option.when(v.videoWidth > 0)(v)
    }

  private def label(t: Tile): String =
    if (t.peer == "self") Meeting.self.v.map(_.name).getOrElse("あなた") + (if (t.screen) " の画面" else "")
    else Meeting.peers.v.find(_.id == t.peer).map(_.name).getOrElse("") + (if (t.screen) " の画面" else "")

  private def draw(): Unit = canvas.foreach { c =>
    val g = c.getContext("2d").asInstanceOf[js.Dynamic]
    g.fillStyle = "#0b1220"
    g.fillRect(0, 0, W, H)
    val self = Meeting.self.v.map(_ => Tile("self", "self", screen = false)).toList
    val myShare = Meeting.screen.map(_ => Tile("self/screen", "self", screen = true)).toList
    val all = myShare ++ Meeting.tiles.v.filter(_.screen) ++ self ++
      Meeting.tiles.v.filterNot(t => t.screen || t.key == "self")
    all.find(_.screen) match {
      case Some(sh) =>
        // 画面共有を大きく、人は右に縦に並べる（話している人を先に）
        val people = all.filterNot(_.screen).sortBy(t =>
          if (Meeting.speaking.v.contains(if (t.key == "self") "self" else t.peer)) 0 else 1
        ).take(4)
        val side = if (people.isEmpty) 0 else 260
        cell(g, sh, 8, 8, W - side - 16, H - 16, contain = true)
        people.zipWithIndex.foreach {
          (t, i) =>
            val h = (H - 8 * (people.size + 1)) / people.size.max(1) min 170
            cell(g, t, W - side, 8 + i * (h + 8), side - 8, h, contain = false)
        }
      case None =>
        val n = all.size.max(1)
        val cols = if (n == 1) 1 else if (n <= 4) 2 else if (n <= 9) 3 else 4
        val rows = (n + cols - 1) / cols
        // 枠は 16:9 に保ち、全体を真ん中に置く
        val cw0 = (W - 8 * (cols + 1)) / cols
        val ch0 = (H - 8 * (rows + 1)) / rows
        val cw = math.min(cw0, ch0 * 16 / 9)
        val ch = cw * 9 / 16
        val x0 = (W - cols * cw - (cols - 1) * 8) / 2
        val y0 = (H - rows * ch - (rows - 1) * 8) / 2
        all.take(16).zipWithIndex.foreach {
          (t, i) => cell(g, t, x0 + (i % cols) * (cw + 8), y0 + (i / cols) * (ch + 8), cw, ch, contain = false)
        }
    }
  }

  /** 1つの枠: 映像（なければ名前の頭文字）と名前. 話している人は枠を光らせる. */
  private def cell(g: js.Dynamic, t: Tile, x: Int, y: Int, w: Int, h: Int, contain: Boolean): Unit = {
    g.fillStyle = "#1e293b"
    g.fillRect(x, y, w, h)
    streamOf(t).flatMap(videoOf) match {
      case Some(v) =>
        val (vw, vh) = (v.videoWidth.toDouble, v.videoHeight.toDouble)
        if (contain) {
          val k = math.min(w / vw, h / vh)
          g.drawImage(v, x + (w - vw * k) / 2, y + (h - vh * k) / 2, vw * k, vh * k)
        } else {
          // 枠いっぱいに（はみ出す分を切る）
          val k = math.max(w / vw, h / vh)
          val (sw, sh) = (w / k, h / k)
          g.drawImage(v, (vw - sw) / 2, (vh - sh) / 2, sw, sh, x, y, w, h)
        }
      case None =>
        val name = label(t)
        g.fillStyle = "#334155"
        g.beginPath()
        g.arc(x + w / 2, y + h / 2, math.min(w, h) / 5, 0, 2 * math.Pi)
        g.fill()
        g.fillStyle = "#e5e7eb"
        g.font = s"${math.min(w, h) / 6}px sans-serif"
        g.textAlign = "center"
        g.textBaseline = "middle"
        g.fillText(name.take(1), x + w / 2, y + h / 2)
    }
    if (!t.screen && Meeting.speaking.v.contains(if (t.key == "self") "self" else t.peer)) {
      g.strokeStyle = "#22c55e"
      g.lineWidth = 4
      g.strokeRect(x + 2, y + 2, w - 4, h - 4)
    }
    val name = label(t)
    if (name.nonEmpty) {
      g.font = "16px sans-serif"
      g.textAlign = "left"
      g.textBaseline = "alphabetic"
      val tw = g.measureText(name).width.asInstanceOf[Double]
      g.fillStyle = "rgba(0,0,0,0.55)"
      g.fillRect(x + 6, y + h - 30, tw + 14, 24)
      g.fillStyle = "#ffffff"
      g.fillText(name, x + 13, y + h - 12)
    }
  }
}
