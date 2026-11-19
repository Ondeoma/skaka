package skaka.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.staticcontent.*
import skaka.api.*
import skaka.auth.{Crypto, OidcClient, OidcConfig, SecretBox}
import skaka.db.*
import skaka.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import skaka.shared.{ErrorDto, IceServer}

import scala.concurrent.duration.*

final case class AppConfig(port: Int, db: DbConfig, oidc: Option[OidcConfig], api: ApiConfig, staticDir: Option[String],
                           skuakaUrl: Option[String], secretKey: Option[String], skawau: Option[SkawauConfig],
                           whisper: Option[WhisperConfig] = None, skakapo: Option[SkakapoConfig] = None)

/** 設定は環境変数から読む. */
object Config:
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int]    = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean    = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  private def list(name: String): List[String] = opt(name).toList.flatMap(_.split(Array(',', ' ')).map(_.trim).filter(_.nonEmpty))

  /** OAUTH_CLIENTS: [{"id":"skuaka","secret":"…（16文字以上）","redirectUri":"https://…/api/integrations/skaka/callback","name":"skuaka","scopes":["read","write"]}] */
  private def oauthClients: Either[String, List[OAuthClient]] =
    opt("OAUTH_CLIENTS").fold(Right(Nil)) { s =>
      io.circe.parser.parse(s).flatMap(_.as[List[io.circe.Json]]).left.map(_ => "OAUTH_CLIENTS は JSON の配列で指定してください").flatMap(_.traverse { j =>
        val c = j.hcursor
        (c.get[String]("id"), c.get[String]("secret"), c.get[String]("redirectUri")).tupled.toOption
          .filter((id, sec, uri) => id.nonEmpty && sec.length >= 16 && (uri.startsWith("https://") || local(uri)))
          .map((id, sec, uri) => OAuthClient(id, sec, uri, c.get[String]("name").getOrElse(id), c.get[List[String]]("scopes").getOrElse(List("read"))))
          .filter(c => c.scopes.nonEmpty && c.scopes.forall(Set("read", "write")))
          .toRight("OAUTH_CLIENTS の各要素には id・secret（16文字以上）・redirectUri（https://）を指定してください（scopes は read・write）")
      })
    }

  /** 手元の URL か（http://localhost・127.0.0.1・*.localhost）. */
  private def local(url: String): Boolean =
    org.http4s.Uri.fromString(url).toOption.exists(u => u.host.map(_.value.toLowerCase).exists(h =>
      h == "localhost" || h == "127.0.0.1" || h == "[::1]" || h.endsWith(".localhost")))

  def load: IO[AppConfig] = IO.fromEither(
    (for
      dbUrl  <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub    <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
      // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）の URL でしか使わせない
      _ <- Either.cond(!devLogin || local(pub), (), "DEV_LOGIN は開発用です。PUBLIC_URL が http://localhost など手元の URL のときだけ使えます")
      ice <- opt("ICE_SERVERS").fold(Right(IceConfig().servers))(j =>
               io.circe.parser.decode[List[IceServer]](j).left.map(e => s"ICE_SERVERS を読めません（JSON の配列 [{\"urls\": [...]}]）: ${e.getMessage}"))
      clients <- oauthClients
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match
                case (Some(i), Some(c), Some(s)) =>
                  Right(Some(OidcConfig(i, c, s, s"$pub/auth/callback", opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
                    Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")), list("OIDC_ALLOWED_GROUPS").toSet)))
                case (None, None, None) if devLogin => Right(None)
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      skawau <- (opt("SKAWAU_URL"), opt("SKAWAU_CLIENT_ID"), opt("SKAWAU_CLIENT_SECRET")) match
                  case (Some(u), Some(id), Some(sec)) =>
                    Right(Some(SkawauConfig(u.stripSuffix("/"), opt("SKAWAU_INTERNAL_URL").getOrElse(u).stripSuffix("/"), id, sec,
                      opt("RECORDING_DIR").getOrElse(java.nio.file.Paths.get(sys.props("java.io.tmpdir"), "skaka-recordings").toString),
                      int("RECORDING_MAX_MB").getOrElse(8192).toLong * 1024 * 1024)))
                  case (None, None, None) => Right(None)
                  case _ => Left("skawau と連携するときは SKAWAU_URL・SKAWAU_CLIENT_ID・SKAWAU_CLIENT_SECRET を設定してください")
    yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
      oidc = oidc,
      api = ApiConfig(cookieSecure = pub.startsWith("https://"), publicUrl = pub, sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin, ice = IceConfig(ice, list("TURN_URLS"), opt("TURN_SECRET"), int("TURN_TTL_HOURS").getOrElse(12).hours),
        oauthClients = clients),
      staticDir = opt("STATIC_DIR"),
      skuakaUrl = opt("SKUAKA_URL").map(_.stripSuffix("/")),
      secretKey = opt("SECRET_KEY"),
      skawau = skawau,
      whisper = opt("WHISPER_URL").map(u => WhisperConfig(u.stripSuffix("/"), opt("WHISPER_MODEL").getOrElse("whisper-1"),
        opt("WHISPER_LANGUAGE").getOrElse("ja"), opt("WHISPER_API_KEY"), int("WHISPER_CONCURRENCY").getOrElse(2).max(1).min(32), opt("WHISPER_PROMPT"))),
      skakapo = opt("SKAKAPO_URL").map(u => SkakapoConfig(u.stripSuffix("/"), opt("SKAKAPO_INTERNAL_URL").getOrElse(u).stripSuffix("/"),
        opt("SKAKAPO_REQUEST_TYPE").getOrElse("other")))
    )).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version = Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- skaka.observe.Log.println(s"[skaka] database migrated ($n migration(s) applied)")
      _ <- (for
        _    <- Telemetry.resource("skaka", version, runtime)
        xa   <- Database.transactor(cfg.db)
        _    <- gauges(xa)
        // 外への呼び出しもトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        // 録画を skawau へ送る（大きいので、時間で切らない）
        upload <- EmberClientBuilder.default[IO].withTimeout(Duration.Inf).withIdleTimeInPool(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users    = new UserStore(xa)
        rooms    = new RoomStore(xa, users)
        meetings = new MeetingStore(xa)
        sess     = new SessionAuth(users, cfg.api)
        _   <- Resource.eval(IO.whenA(oidc.isEmpty)(skaka.observe.Log.println("[skaka] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        box <- Resource.eval(cfg.secretKey match
                 case Some(k) => IO.pure(new SecretBox(k))
                 case None => skaka.observe.Log.println("[skaka] WARNING: SECRET_KEY が未設定です。skuaka・skawau・skakapo のトークンは再起動すると読めなくなります") *>
                     IO(new SecretBox(Crypto.randomToken(32))))
        ended <- Resource.eval(IO.realTimeInstant.flatMap(meetings.endAll))
        _     <- Resource.eval(IO.whenA(ended > 0)(skaka.observe.Log.println(s"[skaka] closed $ended meeting(s) left open by the previous run")))
        skuaka = cfg.skuakaUrl.map(SkuakaClient(_, http))
        links  = new LinkStore(xa)
        pushStore = new skaka.push.PushStore(xa)
        push  <- Resource.eval(skaka.push.PushService.create(pushStore, http, cfg.api.publicUrl, "skaka"))
        notices = Notices(push, users, links, skuaka, box)
        hub   <- Hub.resource(rooms, meetings, notices, transcription = cfg.whisper.isDefined)
        // 文字起こし（自前の Whisper）. 音声を送ってから文字が返るまで待つ（長い音声でも切れないよう長めに）
        // 使い回す接続は短めに捨てる（Whisper のサーバーが先に切っていると送れないため）
        sttHttp <- EmberClientBuilder.default[IO].withTimeout(2.minutes).withIdleTimeInPool(3.seconds).build.map(HttpClientTelemetry(_))
        stt   <- Resource.eval(cfg.whisper.traverse(WhisperClient.create(_, sttHttp)))
        _     <- Resource.eval(cfg.whisper.traverse_(w => skaka.observe.Log.println(s"[skaka] whisper: ${w.url} (model ${w.model}, ${w.language})")))
        _     <- hubGauges(hub)
        recs   = new RecordingStore(xa)
        sklinks = new SkawauStore(xa)
        // skawau にファイルを置く（録画・議事録）
        skFiles  = cfg.skawau.map(c => new SkawauFiles(new SkawauClient(c, http, upload), sklinks, box, recs.participantEmails))
        recorder <- skFiles.traverse(f => Recorder.resource(recs, sklinks, f, hub, notices))
        // 議事録（skakapo）. 利用者の skakapo の API トークンで依頼を作る
        minutes <- cfg.skakapo.traverse { c =>
                     val api = new TokenApi("skakapo", "SKAKAPO_URL", c.url, c.internalUrl, "skakapo_", http)
                     MinutesService.resource(new MinutesStore(xa), meetings, new TokenLinks(Some(api), new LinkStore(xa, "skakapo_link"), box, sess, "skakapo"), c,
                       skFiles, cfg.api.publicUrl)
                   }
        _ <- Resource.eval(cfg.skawau.traverse_(c => skaka.observe.Log.println(s"[skaka] skawau: ${c.url} (recordings in ${c.dir})")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
               case Right(m) => skaka.observe.Log.println(s"[skaka] OIDC: ${m.issuer}")
               case Left(e)  => skaka.observe.Log.println(s"[skaka] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します")
             }))
        _ <- purgeLoop(users).background
        app = healthRoutes(xa) <+> ApiRoutes(ApiRoutes.Deps(oidc, users, rooms, meetings, links, hub, skuaka, box, sess, cfg.api,
                recs, sklinks, new OAuthStore(xa), recorder, stt, minutes)) <+> RumRoutes.routes <+> new PushRoutes(pushStore, push, sess).routes
        all = Guard(cfg.staticDir.fold(app)(d => app <+> spa(d)))
        // 会議の WebSocket（/ws）は接続の数（skaka.ws.connections）で見るので、スパンにはしない
        traced <- HttpTelemetry.middleware(List("/api", "/auth", "/oauth", "/mcp"))
        _ <- EmberServerBuilder.default[IO]
               .withHost(ipv4"0.0.0.0")
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               // 会議のやり取りの1つのメッセージの上限（大きなフレームでメモリを使い切らせない）
               .withMaxWebSocketMessageSize(SignalRoutes.MaxFrame)
               .withHttpWebSocketApp(ws => traced(ErrorHandling.Custom.recoverWith((SignalRoutes(hub, ws) <+> all).orNotFound)(handler)))
               .build
        _ <- Resource.eval(skaka.observe.Log.println(s"[skaka] listening on :${cfg.port} (${cfg.api.publicUrl})"))
      yield ()).useForever
    yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else skaka.observe.Log.println(s"[skaka] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] =
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    List(
      Metrics.gauge("skaka.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge("skaka.users.active_24h", "24時間以内にログインしたユーザーの数")(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge("skaka.sessions.active", "有効なセッションの数")(q(sql"select count(*) from session where expires_at > now()")),
      Metrics.gauge("skaka.rooms.total", "会議室の数")(q(sql"select count(*) from room")),
      Metrics.gauge("skaka.meetings.last_24h", "24時間以内に開かれた会議の数")(q(sql"select count(*) from meeting where started_at > now() - interval '24 hours'")),
      Metrics.gauge("skaka.recordings.pending", "録画中・保存中の録画の数")(q(sql"select count(*) from recording where status in ('recording', 'saving')")),
      Metrics.gauge("skaka.recordings.failed", "保存できなかった録画の数")(q(sql"select count(*) from recording where status = 'failed'"))
    ).sequence_

  /** 会議中の数（このノードのメモリ上. 15秒ごと）: 会議中の会議室・会議に入っている人・待機室の人. */
  private def hubGauges(hub: Hub): Resource[IO, Unit] =
    List(
      Metrics.gauge("skaka.rooms.open", "会議中の会議室の数", 15.seconds)(hub.stats.map(_._1)),
      Metrics.gauge("skaka.participants.current", "会議に入っている人の数", 15.seconds)(hub.stats.map(_._2)),
      Metrics.gauge("skaka.lobby.waiting", "待機室で待っている人の数", 15.seconds)(hub.stats.map(_._3))
    ).sequence_

  private def healthRoutes(xa: doobie.Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select 1".query[Int].unique.transact(xa).timeout(3.seconds).attempt.flatMap {
        case Right(_) => IO.pure(TextResponse(Status.Ok, "ok"))
        case Left(e)  => skaka.observe.Log.println(s"[skaka] healthz: $e").as(TextResponse(Status.ServiceUnavailable, "database unavailable"))
      }
  }

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = {
    case _: BodyTooLarge => PayloadTooLarge(ErrorDto("リクエストが大きすぎます"))
    case e =>
    IO(org.slf4j.LoggerFactory.getLogger("skaka").error("unhandled error", e)) *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** 期限の切れたセッション・ログイン途中の状態を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore): IO[Unit] =
    (IO.sleep(1.minute) *> (IO.realTimeInstant.flatMap(users.purgeExpired).handleErrorWith(e => skaka.observe.Log.println(s"[skaka] purge failed: $e")) *>
      IO.sleep(1.hour)).foreverM).void

  /** ビルド済みの画面の配信. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): HttpRoutes[IO] =
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> path if !path.segments.headOption.map(_.decoded()).exists(Set("api", "auth", "oauth", "ws", "mcp", "healthz")) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc))
    }
