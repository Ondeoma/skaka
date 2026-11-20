package skaka.api

import cats.effect.*
import org.http4s.*
import sbirds.http.{BodyLimit, SecurityHeaders}
import sbirds.shared.AppError
import skaka.core.ErrorCode

/**
 * 応答の安全のための共通の処理: リクエストの本文の大きさの上限と、セキュリティのヘッダー.
 * 本文は録画の断片・文字起こしの音声だけ大きくてよく、ほかは JSON・フォームなので 1MB まで（メモリを使い切らせない）.
 */
object Guard {
  val MaxBody: Long = 1024 * 1024

  private def limitOf(req: Request[IO]): Long = req.pathInfo.segments.map(_.decoded()).toList match {
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

  /** どの応答にも付けるヘッダー（HTML には X-Frame-Options と CSP も. 応答が自分で付けた CSP はそのまま）. */
  val headers: SecurityHeaders.Policy = SecurityHeaders.Policy(
    referrer = Some("same-origin"),
    csp = Some(PageCsp),
    cspHtmlOnly = true,
    frameOptionsHtmlOnly = true
  )

  /** 本文が上限を超えた要求は 413 で断り（Content-Length がなく chunked で送られても、超えたところで止める）、応答にヘッダーを付ける. */
  def apply(routes: HttpRoutes[IO]): HttpRoutes[IO] =
    BodyLimit(
      req => Some(limitOf(req)),
      (_, _) => AppError(ErrorCode.RequestTooLarge)
    )(SecurityHeaders(headers)(routes))
}
