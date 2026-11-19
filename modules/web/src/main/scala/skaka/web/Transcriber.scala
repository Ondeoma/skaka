package skaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skaka.shared.*

import scala.collection.mutable
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.typedarray.{DataView, Float32Array, Int16Array}
import scala.util.Try

/**
 * 文字起こし（主催者が始める）. 文字起こし中は、自分のマイクの音を話の切れ目ごとに切り出し、16kHz の WAV にしてサーバーへ送る.
 * サーバーが Whisper で文字にし、全員に字幕として配る. 音声は人ごとに分かれている（メッシュ）ので、送った人がそのまま話した人になる.
 *
 * 話の切れ目は音の大きさで見る（周りの雑音の大きさを覚えて、それより十分大きければ話している）. 話し始めの直前も少し含め、
 * 0.9秒黙るか20秒続いたら区切る. 短すぎるもの（咳など）は送らない.
 */
object Transcriber {

  val notice: RV[Option[TranscribeNotice]] = RV(None)

  /** 届いた字幕（新しいもの. 話し始めた順）. */
  val captions: RV[List[TranscriptLine]] = RV(Nil)

  /** 字幕を画面に出すか（端末ごとに保存）. */
  val show: RV[Boolean] = RV(Prefs.get("captions", "true") == "true")

  private var arrived: Map[String, Double] = Map.empty

  /** 画面に出す字幕（届いてから12秒の間. 最後の3行）. */
  val visible: CRV[List[TranscriptLine]] = computed {
    val t = Meeting.now.v
    captions.v.filter(l => arrived.getOrElse(l.id, 0.0) > t - 12000).takeRight(3)
  }

  def supported: Boolean = !js.isUndefined(js.Dynamic.global.AudioContext) ||
    !js.isUndefined(js.Dynamic.global.webkitAudioContext)

  def toggleShow(): Unit = { show := !show.v; Prefs.set("captions", show.v.toString) }

  /** 文字起こしを始める・止める（主催者）. */
  def toggle(): Unit = Meeting.send(ClientMsg.Transcribe(notice.v.isEmpty))

  def set(n: Option[TranscribeNotice]): Unit = {
    notice := n
    if (n.isDefined) start() else stop()
  }

  def caption(line: TranscriptLine): Unit = {
    arrived = (arrived + (line.id -> js.Date.now())).filter(_._2 > js.Date.now() - 60000)
    captions := (captions.v :+ line).sortBy(_.at).takeRight(50)
  }

  def leave(): Unit = {
    notice := None
    captions := Nil
    arrived = Map.empty
    stop()
  }

  // ---------------------------------------------------------------- 音を拾う

  private val Rate = 16000
  private val SilenceMs = 900.0
  private val MaxMs = 20000.0
  private val MinMs = 400.0
  private val PreFrames = 4

  private var ctx: Option[js.Dynamic] = None
  private var nodes: List[js.Dynamic] = Nil
  private var trackId = ""
  private var timer: Option[Int] = None

  // 話の切れ目の状態
  private var floor = 0.005
  private var speaking = false
  private var loudFrames = 0
  private var silence = 0.0
  private var total = 0.0
  private var startedAt = 0.0
  private val pre = mutable.Queue.empty[(Float32Array, Double)]
  private val chunks = mutable.ArrayBuffer.empty[Float32Array]
  private var lastError = 0.0

  private def start(): Unit = if (timer.isEmpty && supported) {
    timer = Some(dom.window.setInterval(() => follow(), 1000))
    follow()
  }

  private def stop(): Unit = {
    timer.foreach(dom.window.clearInterval)
    timer = None
    if (speaking) finish()
    detach()
    ctx.foreach(c => Try(c.close()))
    ctx = None
  }

  /** マイクの機器を変えたらつなぎ直す. */
  private def follow(): Unit = {
    val t = Meeting.audioTrack
    val id = t.map(_.id.asInstanceOf[String]).getOrElse("")
    if (id != trackId) {
      if (speaking) finish()
      detach()
      trackId = id
      t.foreach(attach)
    }
  }

  private def attach(track: js.Dynamic): Unit = Try {
    val c = ctx.getOrElse {
      val C = if (!js.isUndefined(js.Dynamic.global.AudioContext)) js.Dynamic.global.AudioContext
      else js.Dynamic.global.webkitAudioContext
      val x = js.Dynamic.newInstance(C)()
      ctx = Some(x)
      x
    }
    if (c.state.asInstanceOf[String] == "suspended") c.resume()
    val src = c.createMediaStreamSource(js.Dynamic.newInstance(js.Dynamic.global.MediaStream)(js.Array(track)))
    val proc = c.createScriptProcessor(4096, 1, 1)
    // 音は出さない（ScriptProcessor は出力先につながっていないと動かないブラウザがある）
    val mute = c.createGain()
    mute.gain.value = 0
    val rate = c.sampleRate.asInstanceOf[Double]
    proc.onaudioprocess = (
      (e: js.Dynamic) => onAudio(e.inputBuffer.getChannelData(0).asInstanceOf[Float32Array], rate)): js.Function1[
      js.Dynamic,
      Unit
    ]
    src.connect(proc)
    proc.connect(mute)
    mute.connect(c.destination)
    nodes = List(src, proc, mute)
  }

  private def detach(): Unit = {
    nodes.foreach(n => Try(n.disconnect()))
    nodes = Nil
    trackId = ""
    pre.clear()
  }

  private def onAudio(buf: Float32Array, rate: Double): Unit = {
    val ms = buf.length / rate * 1000
    var sum = 0.0
    var i = 0
    while (i < buf.length) { val x = buf(i).toDouble; sum += x * x; i += 1 }
    val rms = math.sqrt(sum / buf.length)
    val on = Meeting.mic.v
    val loud = on && rms > math.max(0.015, floor * 3)
    val down = downsample(buf, rate)
    if (speaking) {
      chunks += down
      total += ms
      silence = if (loud) 0 else silence + ms
      if (!on || silence >= SilenceMs || total >= MaxMs) finish()
    } else {
      if (!loud) floor = math.min(0.05, floor * 0.95 + rms * 0.05)
      pre.enqueue(down -> ms)
      while (pre.size > PreFrames) pre.dequeue()
      loudFrames = if (loud) loudFrames + 1 else 0
      if (loudFrames >= 2) {
        speaking = true
        chunks.clear()
        chunks ++= pre.map(_._1)
        total = pre.map(_._2).sum
        startedAt = js.Date.now() - total
        silence = 0
        pre.clear()
      }
    }
  }

  /** 区切った音を送る（話した時間が短すぎれば捨てる）. */
  private def finish(): Unit = {
    speaking = false
    loudFrames = 0
    val voiced = total - silence
    val parts = chunks.toList
    chunks.clear()
    if (voiced >= MinMs && Meeting.speechKey.nonEmpty) {
      val ago = (js.Date.now() - startedAt).toLong
      Api.speech(Meeting.speechKey, ago, wav(parts)).failed.foreach {
        case ApiError(503, m) if js.Date.now() - lastError > 30000 =>
          lastError = js.Date.now(); Store.toast(m, error = true)
        case _ => ()
      }
    }
  }

  /** rate の音を 16kHz にする（区間の平均）. */
  private def downsample(buf: Float32Array, rate: Double): Float32Array = {
    val ratio = rate / Rate
    if (ratio <= 1.0) new Float32Array(buf)
    else {
      val n = (buf.length / ratio).toInt
      val out = new Float32Array(n)
      var i = 0
      while (i < n) {
        val a = (i * ratio).toInt
        val b = math.min(buf.length, ((i + 1) * ratio).toInt).max(a + 1)
        var s = 0.0
        var j = a
        while (j < b) { s += buf(j); j += 1 }
        out(i) = (s / (b - a)).toFloat
        i += 1
      }
      out
    }
  }

  /** 16kHz・16bit・モノラルの WAV. */
  private def wav(parts: List[Float32Array]): dom.Blob = {
    val n = parts.map(_.length).sum
    val data = new Int16Array(n)
    var k = 0
    parts.foreach { p =>
      var i = 0
      while (i < p.length) { data(k) = (math.max(-1.0, math.min(1.0, p(i).toDouble)) * 32767).toShort; k += 1; i += 1 }
    }
    val head = new DataView(new js.typedarray.ArrayBuffer(44))
    def text(at: Int, s: String): Unit = s.zipWithIndex.foreach((c, i) => head.setUint8(at + i, c.toShort))
    text(0, "RIFF"); head.setUint32(4, 36 + n * 2, true); text(8, "WAVE"); text(12, "fmt ")
    head.setUint32(16, 16, true); head.setUint16(20, 1, true); head.setUint16(22, 1, true)
    head.setUint32(24, Rate, true); head.setUint32(28, Rate * 2, true); head.setUint16(32, 2, true);
    head.setUint16(34, 16, true)
    text(36, "data"); head.setUint32(40, n * 2, true)
    new dom.Blob(
      js.Array[js.Any](head.buffer, data.buffer).asInstanceOf[js.Array[dom.BlobPart]],
      new dom.BlobPropertyBag { `type` = "audio/wav" }
    )
  }
}
