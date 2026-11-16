package skaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.typelevel.ci.CIString
import org.http4s.headers.{`WWW-Authenticate`, Authorization}
import skaka.auth.{ApiTokens, Crypto}
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(
    cookieSecure: Boolean,
    publicUrl: String,
    sessionTtl: FiniteDuration = 14.days,
    cookieName: String = "skaka_session",
    devLogin: Boolean = false,
    version: String = "0.1.0",
    ice: IceConfig = IceConfig(),
    oauthClients: List[OAuthClient] = Nil
)

/**
 * WebRTC の ICE サーバー. servers は STUN（と固定の TURN）. turnUrls と turnSecret があれば、
 * coturn の use-auth-secret（TURN REST API）の一時的な認証情報（ttl の間有効）を参加者ごとに作る.
 */
final case class IceConfig(servers: List[IceServer] = List(IceServer(List("stun:stun.l.google.com:19302"))), turnUrls: List[String] = Nil,
                           turnSecret: Option[String] = None, ttl: FiniteDuration = 12.hours):
  def forPeer(name: String, now: Instant): List[IceServer] =
    servers ++ turnSecret.filter(_ => turnUrls.nonEmpty).toList.map { secret =>
      val user = s"${now.getEpochSecond + ttl.toSeconds}:$name"
      IceServer(turnUrls, Some(user), Some(Crypto.hmacSha1Base64(secret, user)))
    }

/** 誰の要求か. token は API トークンで呼ばれたときの (トークンID, スコープ). */
final case class Principal(user: User, token: Option[(UUID, String)]):
  def canWrite: Boolean = token.forall(_._2 == TokenScope.Write)

/** 認証: Cookie のセッション、または `Authorization: Bearer <API トークン>`（どちらも DB にはハッシュだけ保存する）. */
final class SessionAuth(users: UserStore, cfg: ApiConfig):

  /** Bearer の値（API トークンの形のものだけ）. */
  def bearer(req: Request[IO]): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, t)) => t.trim }

  def fromRequest(req: Request[IO]): IO[Option[Principal]] =
    bearer(req) match
      case Some(t) if ApiTokens.looksLike(t) =>
        users.findToken(Crypto.sha256(t), Instant.now()).map(_.map((u, id, scope) => Principal(u, Some(id -> scope))))
      case Some(_) => IO.pure(None)
      case None =>
        req.cookies.find(_.name == cfg.cookieName).map(_.content).filter(_.nonEmpty) match
          case None        => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map(Principal(_, None)))

  def issue(user: User, idToken: Option[String]): IO[ResponseCookie] =
    val token = Crypto.randomToken()
    users.createSession(Crypto.sha256(token), user.id, idToken, Instant.now().plusSeconds(cfg.sessionTtl.toSeconds))
      .as(cookie(token, cfg.sessionTtl.toSeconds))

  /** セッションを消す. IdP のログアウトに渡す ID トークンと、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] =
    req.cookies.find(_.name == cfg.cookieName) match
      case Some(c) => users.deleteSession(Crypto.sha256(c.content)).map(_ -> cookie("", 0))
      case None    => IO.pure(None -> cookie("", 0))

  private def cookie(value: String, maxAge: Long): ResponseCookie =
    ResponseCookie(cfg.cookieName, value, maxAge = Some(maxAge), path = Some("/"), httpOnly = true, secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax))

/** 文字列の応答（JSON 以外. circe の EntityEncoder に取られないよう明示する）. */
object TextResponse:
  def apply(status: Status, body: String, media: MediaType = MediaType.text.plain): Response[IO] =
    Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder).withContentType(headers.`Content-Type`(media, Charset.`UTF-8`))

/** *Routes に共通の補助（エラー応答・認証・リクエスト本文）. */
trait RouteSupport extends Http4sDsl[IO]:
  protected def sessions: SessionAuth

  protected def err(status: Status, msg: String): IO[Response[IO]] =
    Response[IO](status).withEntity(ErrorDto(msg)).pure[IO]

  /** 画面からの更新の要求に付ける見出し. 他のサイトのフォームからは付けられない（CORS の事前確認が要る）ので CSRF を防げる. */
  private val csrfHeader = CIString("X-Skaka")

  private def readOnly(req: Request[IO]): Boolean = Set(Method.GET, Method.HEAD).contains(req.method)

  /**
   * ログインしていなければ 401.
   * Cookie のときは、GET 以外に X-Skaka の見出しが必要. API トークン（Bearer）はブラウザが自動では付けないので CSRF の確認は要らないが、
   * 参照のみのトークンでは GET しかできない.
   */
  protected def authed(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p => if p.canWrite || readOnly(req) then f(p.user) else err(Status.Forbidden, "参照のみの API トークンでは変更できません"))

  /** 画面（Cookie のセッション）からだけ受け付ける操作（API トークンの管理など）. */
  protected def interactive(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p => if p.token.isEmpty then f(p.user) else err(Status.Forbidden, "この操作は API トークンではできません"))

  /** ログインしていればその人（していなければ None. API トークンが正しくなければ 401）. */
  protected def optional(req: Request[IO])(f: Option[User] => IO[Response[IO]]): IO[Response[IO]] =
    sessions.fromRequest(req).flatMap {
      case None if sessions.bearer(req).isDefined => principal(req)(p => f(Some(p.user)))
      case p                                      => f(p.map(_.user))
    }

  /** 画面からの更新の要求か（CSRF の確認. ログインしていない人の要求にも使う）. */
  protected def fromScreen(req: Request[IO]): Boolean = req.headers.get(csrfHeader).isDefined

  private def principal(req: Request[IO])(f: Principal => IO[Response[IO]]): IO[Response[IO]] =
    val bearer = sessions.bearer(req).isDefined
    if !bearer && !readOnly(req) && req.headers.get(csrfHeader).isEmpty then err(Status.Forbidden, "画面以外からの更新の要求は受け付けません")
    else
      sessions.fromRequest(req).flatMap {
        case Some(p) => f(p)
        case None if bearer =>
          err(Status.Unauthorized, "API トークンが正しくないか、期限が切れています").map(_.putHeaders(`WWW-Authenticate`(Challenge("Bearer", "skaka"))))
        case None => err(Status.Unauthorized, "ログインしてください")
      }

  protected def body[A: Decoder](req: Request[IO])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    req.as[A].attempt.flatMap {
      case Left(e)  => err(Status.BadRequest, s"リクエストの内容が正しくありません: ${e.getMessage.take(200)}")
      case Right(b) => f(b)
    }

  protected def param(req: Request[IO], k: String): Option[String] = req.params.get(k).map(_.trim).filter(_.nonEmpty)

  protected object UuidVar:
    def unapply(s: String): Option[UUID] = scala.util.Try(UUID.fromString(s)).toOption

  /** 結果が Right なら f、Left なら 400. */
  protected def valid[A](e: Either[String, A])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    e.fold(err(Status.BadRequest, _), f)

/** 入力の確認. */
object Check:
  def text(v: String, what: String, max: Int): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")

  def name(v: String, what: String, max: Int): Either[String, String] =
    val t = v.trim
    if t.isEmpty then Left(s"$what を入れてください") else text(t, what, max)

/** アプリのルートをまとめる（REST API・会議の WebSocket・MCP）. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes:
  /** recorder は skawau（録画の保存先）を設定したときだけ. */
  final case class Deps(oidc: Option[skaka.auth.OidcClient], users: UserStore, rooms: RoomStore, meetings: MeetingStore, links: LinkStore,
                        hub: Hub, skuaka: Option[SkuakaClient], box: skaka.auth.SecretBox, sessions: SessionAuth, cfg: ApiConfig,
                        recordings: RecordingStore, skawauLinks: SkawauStore, oauth: OAuthStore, recorder: Option[Recorder] = None)

  /** REST API と MCP（WebSocket は SignalRoutes）. */
  def apply(d: Deps): HttpRoutes[IO] =
    val rooms = new RoomRoutes(d)
    val rest = new AuthRoutes(d).routes <+> rooms.routes <+> new SkuakaRoutes(d, rooms).routes <+> new SkawauRoutes(d).routes <+>
      new OAuthRoutes(d.oauth, d.cfg.oauthClients, d.sessions, d.cfg).routes
    rest <+> new McpRoutes(rest, d.sessions, d.cfg.publicUrl, d.cfg.version).routes
