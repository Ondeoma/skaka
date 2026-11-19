package skaka.web

import org.scalajs.dom

import scala.scalajs.js

/**
 * 一定の間隔で呼ぶ時計. タブが裏にあると setInterval・requestAnimationFrame は遅くなる（止まる）ため、Worker の時計を使う.
 * 録画（Recorder）と背景（Background）が映像を描き続けるのに使う. 返す関数で止める.
 */
object Ticker {
  private lazy val src = dom.URL.createObjectURL(new dom.Blob(
    js.Array(
      "let t; onmessage = e => { clearInterval(t); if (e.data > 0) t = setInterval(() => postMessage(0), e.data); };"
    ),
    new dom.BlobPropertyBag { `type` = "text/javascript" }
  ))

  def every(
    ms: Int
  )(
    f: () => Unit
  ): () => Unit = {
    val w = new dom.Worker(src)
    w.onmessage = (_: dom.MessageEvent) => f()
    w.postMessage(ms)
    () => { w.postMessage(0); w.terminate() }
  }
}
