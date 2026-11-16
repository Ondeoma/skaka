package skaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skaka.auth.Crypto
import skaka.db.*
import skaka.shared.*

import java.time.Instant
import java.util.UUID

/**
 * skawau（ファイル共有）との連携と録画.
 * 連携: 各ユーザーが OAuth 2.0（認可コード + PKCE）で skawau を許可し（スコープ read write）、skaka は本人の権限で録画を置く.
 * 取り決めは skawau の README の「skea・skiwi・skaka との連携」.
 */
final class SkawauRoutes(d: ApiRoutes.Deps) extends RouteSupport:
  import d.{box, cfg, recordings}
  val sessions: SessionAuth = d.sessions
  private val links = d.skawauLinks

  private def callback: String = s"${cfg.publicUrl}/api/integrations/skawau/callback"

  private def redirect(to: String): IO[Response[IO]] = IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to))))

  /** 設定の画面へ戻る（結果を skawau=… に付ける）. 戻り先は同じサイトのパスだけ. */
  private def done(returnTo: String, result: String): IO[Response[IO]] =
    val back = SafePath.orElse(Option(returnTo), "/settings")
    redirect(back + (if back.contains("?") then "&" else "?") + s"skawau=$result")

  private def withClient(f: SkawauClient => IO[Response[IO]]): IO[Response[IO]] =
    d.recorder.fold(err(Status.ServiceUnavailable, "skawau との連携が設定されていません（SKAWAU_URL）"))(r => f(r.client))

  private def withRecorder(f: Recorder => IO[Response[IO]]): IO[Response[IO]] =
    d.recorder.fold(err(Status.ServiceUnavailable, "録画の保存先（skawau）が設定されていません"))(f)

  private def linkDto(u: User): IO[SkawauLinkDto] =
    links.token(u.id).map(t => SkawauLinkDto(d.recorder.isDefined, d.recorder.map(_.client.cfg.url), t.isDefined, t.map(_._2.toString)))

  private def answer[A](r: Either[(Status, String), A])(ok: A => IO[Response[IO]]): IO[Response[IO]] = r.fold((s, m) => err(s, m), ok)

  private def recording(u: User, id: UUID)(f: RecordingDto => IO[Response[IO]]): IO[Response[IO]] =
    recordings.dto(u.id, id).flatMap(_.fold(err(Status.NotFound, "録画が見つかりません"))(f))

  private def roomOf(code: String): IO[Option[(Room, UserBrief)]] = RoomCode.parse(code).fold(IO.pure(None))(d.rooms.byCode)

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // ------------------------------------------------------------ 連携

    case req @ GET -> Root / "api" / "skawau" => authed(req)(u => linkDto(u).flatMap(Ok(_)))

    /** 連携を始める（skawau の許可の画面へ）. */
    case req @ GET -> Root / "api" / "integrations" / "skawau" / "connect" =>
      interactive(req)(u => withClient { c =>
        val state    = Crypto.randomToken(24)
        val verifier = Crypto.randomToken(48)
        links.saveState(state, u.id, verifier, SafePath.orElse(param(req, "returnTo"), "/settings"), Instant.now().plusSeconds(600)) *>
          redirect(Uri.unsafeFromString(s"${c.cfg.url}/oauth/authorize").withQueryParams(Map(
            "response_type" -> "code", "client_id" -> c.cfg.clientId, "redirect_uri" -> callback, "scope" -> "read write", "state" -> state,
            "code_challenge" -> Crypto.pkceChallenge(verifier), "code_challenge_method" -> "S256")).renderString)
      })

    /** 許可の画面から戻ったとき: コードをトークンに替えて保存する. */
    case req @ GET -> Root / "api" / "integrations" / "skawau" / "callback" =>
      interactive(req)(u => withClient { c =>
        param(req, "state").fold(IO.pure(None))(links.takeState).flatMap {
          case None                                     => done("/settings", "error")
          case Some((uid, _, back)) if uid != u.id      => done(back, "error")
          case Some((_, _, back)) if param(req, "error").isDefined =>
            done(back, if param(req, "error").contains("access_denied") then "denied" else "error")
          case Some((_, verifier, back)) =>
            param(req, "code").fold(done(back, "error")) { code =>
              c.exchange(code, callback, verifier).flatMap {
                case Right((token, scopes)) if scopes.contains("write") => links.saveToken(u.id, box.seal(token)) *> done(back, "connected")
                // 書き込みを許されていない（skawau の OAUTH_CLIENTS の scopes に write がない）
                case Right((token, _)) => c.revoke(token) *> done(back, "readonly")
                case Left(e)           => IO.println(s"[skaka] skawau token exchange failed: $e") *> done(back, "error")
              }
            }
        }
      })

    /** 連携をやめる（skawau のトークンも失効させる）. */
    case req @ DELETE -> Root / "api" / "integrations" / "skawau" / "connection" =>
      interactive(req)(u => withClient { c =>
        links.token(u.id).flatMap(_.flatMap(t => box.open(t._1)).traverse_(c.revoke)) *> links.deleteToken(u.id) *> NoContent()
      })

    // ------------------------------------------------------------ 録画（画面から）

    /** 録画を始める（会議中の主催者. 録画は画面が行い、断片を順に送る）. */
    case req @ POST -> Root / "api" / "recordings" =>
      interactive(req)(u => withRecorder(rec => body[RecordingStartReq](req) { r =>
        roomOf(r.room).flatMap {
          case None => err(Status.NotFound, "会議室が見つかりません")
          case Some((room, _)) =>
            rec.start(u, room, r.mime, r.share).flatMap(answer(_)(x => recording(u, x.id)(Created(_))))
        }
      }))

    /** 録画の断片（本文がそのまま中身. seq は 0 から順に）. */
    case req @ PUT -> Root / "api" / "recordings" / UuidVar(id) / "chunks" / IntVar(seq) =>
      interactive(req)(u => withRecorder(rec =>
        req.body.take(Recorder.MaxChunk.toLong + 1).compile.to(Array).flatMap { data =>
          if data.length > Recorder.MaxChunk then err(Status.PayloadTooLarge, "断片が大きすぎます")
          else rec.append(u, id, seq, data).flatMap(answer(_)(n => Ok(io.circe.Json.obj("bytes" -> io.circe.Json.fromLong(n)))))
        }))

    /** 録り終えた（skawau に置くのは裏で行う）. */
    case req @ POST -> Root / "api" / "recordings" / UuidVar(id) / "finish" =>
      interactive(req)(u => withRecorder(rec => body[RecordingFinishReq](req)(r =>
        rec.finish(u, id, Some(r.durationMs)).flatMap(answer(_)(_ => recording(u, id)(Accepted(_)))))))

    // ------------------------------------------------------------ 録画（一覧・再試行・削除. API トークンでも）

    case req @ GET -> Root / "api" / "recordings" =>
      authed(req) { u =>
        val limit = param(req, "limit").flatMap(_.toIntOption).map(_.max(1).min(200)).getOrElse(50)
        (param(req, "meeting"), param(req, "room")) match
          case (Some(m), _) => scala.util.Try(UUID.fromString(m)).toOption.fold(err(Status.BadRequest, "meeting は会議の id です"))(id =>
              recordings.forMeeting(u.id, id).flatMap(Ok(_)))
          case (_, Some(c)) => roomOf(c).flatMap(_.fold(err(Status.NotFound, "会議室が見つかりません"))((r, _) =>
              recordings.list(u.id, Some(r.id), limit).flatMap(Ok(_))))
          case _ => recordings.list(u.id, None, limit).flatMap(Ok(_))
      }

    case req @ GET -> Root / "api" / "recordings" / UuidVar(id) => authed(req)(u => recording(u, id)(Ok(_)))

    case req @ POST -> Root / "api" / "recordings" / UuidVar(id) / "retry" =>
      authed(req)(u => withRecorder(rec => rec.retry(u, id).flatMap(answer(_)(_ => recording(u, id)(Accepted(_))))))

    /** 録画を消す（録画した人・会議室の持ち主. skawau に置いたファイルは消さない）. */
    case req @ DELETE -> Root / "api" / "recordings" / UuidVar(id) =>
      authed(req)(u => recording(u, id) { _ =>
        recordings.find(id).flatMap {
          case None => err(Status.NotFound, "録画が見つかりません")
          case Some(r) =>
            d.rooms.byId(r.roomId).flatMap { room =>
              if r.userId != u.id && !room.exists(_._1.ownerId == u.id) then err(Status.Forbidden, "録画した人か会議室の持ち主だけが消せます")
              else d.recorder.fold(recordings.delete(id))(_.discard(r)) *> NoContent()
            }
        }
      })
  }
