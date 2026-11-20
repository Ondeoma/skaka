package skaka.api

import cats.effect.*
import org.http4s.*
import org.http4s.headers.`WWW-Authenticate`
import sbirds.http.{Csrf, Errors, RouteHelpers}
import sbirds.shared.AppError
import skaka.core.ErrorCode
import skaka.db.*

/**
 * *Routes に共通の補助（エラー応答・認証）. 本文・クエリ・パスの UUID・302 は sbirds.http.RouteHelpers.
 */
trait RouteSupport extends RouteHelpers {

  protected def bodyInvalid: sbirds.shared.ErrorCode = ErrorCode.RequestBodyInvalid

  // 本文の読み取りの途中のエラー（大きすぎる本文を含む）は、これまでどおり RequestBodyInvalid にする
  override protected def bodyKeepsAppError: Boolean = false

  protected def sessions: SessionAuth

  /** エラーの応答（コード付きでログに出る）. detail を省くとコードの既定の文言. */
  protected def err(code: ErrorCode, detail: String | Null = null): IO[Response[IO]] =
    Errors.respond(AppError(code, Option(detail)))

  protected def fail(e: AppError): IO[Response[IO]] = Errors.respond(e)

  /** 画面からの更新の要求に付ける見出し. 他のサイトのフォームからは付けられない（CORS の事前確認が要る）ので CSRF を防げる. */
  private val csrf = Csrf.Policy.headerOnly("X-Skaka")

  private def readOnly(req: Request[IO]): Boolean = Set(Method.GET, Method.HEAD).contains(req.method)

  /**
   * ログインしていなければ 401.
   * Cookie のときは、GET 以外に X-Skaka の見出しが必要. API トークン（Bearer）はブラウザが自動では付けないので CSRF の確認は要らないが、
   * 参照のみのトークンでは GET しかできない.
   */
  protected def authed(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p =>
      if p.canWrite || readOnly(req) then f(p.user) else err(ErrorCode.ApiTokenReadOnly)
    )

  /** 画面（Cookie のセッション）からだけ受け付ける操作（API トークンの管理など）. */
  protected def interactive(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p => if p.token.isEmpty then f(p.user) else err(ErrorCode.ApiTokenNotAllowed))

  /** ログインしていればその人（していなければ None. API トークンが正しくなければ 401）. */
  protected def optional(req: Request[IO])(f: Option[User] => IO[Response[IO]]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None if sessions.bearer(req).isDefined => principal(req)(p => f(Some(p.user)))
      case Some(p) => as(p.user)(f(Some(p.user)))
      case None => f(None)
    }

  /** 画面からの更新の要求か（CSRF の確認. ログインしていない人の要求にも使う）. */
  protected def fromScreen(req: Request[IO]): Boolean = Csrf.fromOwnScreen(req, csrf)

  private def principal(req: Request[IO])(f: Principal => IO[Response[IO]]): IO[Response[IO]] = {
    val bearer = sessions.bearer(req).isDefined
    if !bearer && Csrf.foreign(req, csrf) then
      err(ErrorCode.CrossSiteRequest)
    else
      sessions.fromRequest(req).flatMap {
        case Some(p) => as(p.user)(f(p))
        case None if bearer =>
          err(ErrorCode.ApiTokenInvalid).map(_.putHeaders(`WWW-Authenticate`(Challenge("Bearer", "skaka"))))
        case None => err(ErrorCode.NotLoggedIn)
      }
  }

  /** 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる. */
  private def as[A](u: User)(fa: IO[A]): IO[A] = sbirds.observe.Telemetry.withUser(u.id.toString, u.name)(fa)

  /** 一覧の件数（limit. 1〜200、省くと 50）. */
  protected def limitParam(req: Request[IO]): Int =
    param(req, "limit").flatMap(_.toIntOption).map(_.max(1).min(200)).getOrElse(50)

  /** 結果が Right なら f、Left なら 400. */
  protected def valid[A](e: Either[String, A])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    e.fold(m => err(ErrorCode.InvalidInput, m), f)
}
