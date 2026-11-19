package skaka.api

import cats.effect.*
import org.http4s.*
import org.typelevel.ci.CIString
import sbirds.http.Errors
import sbirds.shared.AppError
import skaka.core.ErrorCode

/**
 * 応答の安全のための共通の処理: リクエストの本文の大きさの上限と、セキュリティのヘッダー.
 * 本文は録画の断片・文字起こしの音声だけ大きくてよく、ほかは JSON・フォームなので 1MB まで（メモリを使い切らせない）.
 */
object Guard {
  val MaxBody: Long = 1024 * 1024

  private def limitOf(
    req: Request[IO]
  ): Long = req.pathInfo.segments.map(_.decoded()).toList match {
    case "api" :: "recordings" :: _ :: "chunks" :: _ :: Nil => Recorder.MaxChunk.toLong
    case "api" :: "speech" :: _ :: Nil => Transcriber.MaxBytes.toLong
    case _ => MaxBody
  }

  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")

  /**
   * 画面（index.html）の CSP. MediaPipe の WASM（wasm-unsafe-eval）、Worker の時計（blob:）、背景の画像（data:）、IdP のアバター（https:）を許す.
   * skorimako のブラウザの観測（SKORIMAKO_RUM_*）を設定したときは、その rum.js の読み込みと送信も許す.
   */
  val PageCsp: String = {
    val rum = sbirds.observe.RumRoutes.origin.fold("")(" " + _)
    List(
      "default-src 'self'",
      s"script-src 'self' 'wasm-unsafe-eval'$rum",
      "worker-src 'self' blob:",
      "style-src 'self' 'unsafe-inline'",
      s"img-src 'self' data: blob: https:$idpImg",
      "media-src 'self' blob:",
      s"connect-src 'self'$rum",
      "font-src 'self'",
      "object-src 'none'",
      "base-uri 'self'",
      "form-action 'self'",
      "frame-ancestors 'none'"
    ).mkString("; ")
  }

  def apply(
    routes: HttpRoutes[IO]
  ): HttpRoutes[IO] = cats.data.Kleisli { (req: Request[IO]) =>
    val limit = limitOf(req)
    if req.contentLength.exists(_ > limit) then
      cats.data.OptionT.liftF(Errors.respond(ErrorCode.RequestTooLarge))
    else {
      // Content-Length がなく（chunked で）送られても、上限を超えたところで止める
      val body = req.body.chunks.evalMapAccumulate(0L) {
        (
          n,
          c
        ) =>
          {
            val m = n + c.size
            if m > limit then IO.raiseError(AppError(ErrorCode.RequestTooLarge)) else IO.pure(m -> c)
          }
      }.flatMap(
        (
          _,
          c
        ) => fs2.Stream.chunk(c)
      )
      routes(req.withBodyStream(body)).map(secure)
    }
  }

  /** どの応答にも付けるヘッダー（HTML には CSP も. 個別に付けたものはそのまま）. */
  def secure(
    r: Response[IO]
  ): Response[IO] = {
    val html = r.contentType.exists(_.mediaType == MediaType.text.html)
    val base = r.putHeaders("X-Content-Type-Options" -> "nosniff", "Referrer-Policy" -> "same-origin")
    if !html then base
    else {
      val csp = if r.headers.get(CIString("Content-Security-Policy")).isDefined then Nil
      else List[Header.ToRaw]("Content-Security-Policy" -> PageCsp)
      base.putHeaders(("X-Frame-Options" -> "DENY": Header.ToRaw) :: csp*)
    }
  }
}
