package skaka.api

import cats.data.EitherT
import cats.effect.*
import cats.effect.std.{Mutex, Supervisor}
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import fs2.io.file.{Files, Flags, Path}
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{`Content-Length`, `Content-Type`, Authorization}
import skaka.auth.SecretBox
import skaka.core.Webm
import skaka.db.*
import skaka.shared.*

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.duration.*
import scala.util.Try

/**
 * skawau（ファイル共有）の設定. url は画面から開く URL（許可の画面・録画のリンク）、internalUrl はこのサーバーから呼ぶ URL.
 * dir は録画中の一時ファイルを置くところ、maxBytes は1つの録画の上限.
 */
final case class SkawauConfig(url: String, internalUrl: String, clientId: String, clientSecret: String, dir: String, maxBytes: Long)

/** skawau の API. 失敗は (HTTP のステータス, 文言). */
final class SkawauClient(val cfg: SkawauConfig, http: Client[IO], upload: Client[IO]):

  private def uri(path: String): Uri = Uri.unsafeFromString(cfg.internalUrl + path)
  private def basic = Authorization(BasicCredentials(cfg.clientId, cfg.clientSecret))
  private def bearer(t: String) = RemoteJson.bearer(t)

  private def send(c: Client[IO], req: Request[IO]): IO[Either[(Int, String), Json]] = RemoteJson.run(c, req, "skawau")

  /** 認可コードをトークンに替える. (トークン, スコープ). */
  def exchange(code: String, redirect: String, verifier: String): IO[Either[String, (String, List[String])]] =
    val form = UrlForm("grant_type" -> "authorization_code", "code" -> code, "redirect_uri" -> redirect, "code_verifier" -> verifier)
    send(http, Request[IO](Method.POST, uri("/oauth/token")).withEntity(form).putHeaders(basic)).map {
      case Left((_, m)) => Left(m)
      case Right(j) =>
        j.hcursor.get[String]("access_token").toOption.toRight("skawau の応答を読めません")
          .map(_ -> j.hcursor.get[String]("scope").getOrElse("read").split(' ').toList)
    }

  def revoke(token: String): IO[Unit] =
    send(http, Request[IO](Method.POST, uri("/oauth/revoke")).withEntity(UrlForm("token" -> token)).putHeaders(basic)).void

  def call(token: String, method: Method, path: String, body: Option[Json] = None): IO[Either[(Int, String), Json]] =
    val req = Request[IO](method, uri(path)).putHeaders(bearer(token))
    send(http, body.fold(req)(req.withEntity(_)))

  /** ファイルを置く（folder の下の path のフォルダに. 同じ名前があれば名前を変えて）. 応答は skawau の NodeDto. */
  def put(token: String, folder: UUID, path: String, name: String, body: Stream[IO, Byte], length: Long, mime: String): IO[Either[(Int, String), Json]] =
    val u = uri(s"/api/files/$folder/upload").withQueryParams(Map("path" -> path, "name" -> name, "conflict" -> "rename"))
    val ct = MediaType.parse(mime).toOption.getOrElse(MediaType.application.`octet-stream`)
    send(upload, Request[IO](Method.PUT, u).withBodyStream(body).putHeaders(bearer(token), `Content-Length`(length), `Content-Type`(ct)))

/**
 * 録画. 録画する人の画面が会議の映像と音を1つにまとめて MediaRecorder で録り、数秒ごとの断片を順に送ってくる.
 * サーバーは一時ファイルに追記し、録り終えたら（画面が終えた・しばらく届かない）、録画した人の skawau に置いて参加者と共有する.
 * skawau のアップロードは1回で全体を送るため、録画中は一時ファイルにためる（途中で切れても、それまでの分は残る）.
 */
final class Recorder private (store: RecordingStore, links: SkawauStore, val client: SkawauClient, box: SecretBox, hub: Hub, sup: Supervisor[IO],
                              active: Ref[IO, Map[UUID, Recorder.Active]], notices: Notices):
  import Recorder.*

  private def file(id: UUID): Path = Path(client.cfg.dir) / s"$id.part"

  private def stamp(at: Instant, tz: String): String =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH-mm").withZone(Try(ZoneId.of(tz)).getOrElse(ZoneId.of("Asia/Tokyo"))).format(at)

  /** 録画を始める（会議に主催者として入っている人だけ. 1つの会議室で同時に1つ）. */
  def start(u: User, room: Room, mime: String, share: Boolean): IO[Either[(Status, String), Recording]] =
    (hub.live(room.id), links.token(u.id)).tupled.flatMap {
      case (_, None) => IO.pure(Left(Status.Conflict -> "skawau と連携していません。設定の画面で「skawau と連携する」を押してください"))
      case (None, _) => IO.pure(Left(Status.Conflict -> "会議が開かれていません"))
      case (Some(l), _) if !l.peers.exists(p => p.userId.contains(u.id) && p.host) => IO.pure(Left(Status.Forbidden -> "録画できるのは会議中の主催者です"))
      case (Some(l), _) if l.recording.isDefined => IO.pure(Left(Status.Conflict -> s"${l.recording.get.by} さんが録画しています"))
      // 中身の種類は skawau に置くときの Content-Type になる. 動画（WebM・MP4）だけを受け付ける
      case _ if !Mimes.contains(baseMime(mime)) => IO.pure(Left(Status.BadRequest -> "録画の形式は WebM か MP4 にしてください"))
      case (Some(l), _) =>
        active.get.map(_.values.exists(_.rec.userId == u.id)).ifM(
          IO.pure(Left(Status.Conflict -> "別の会議室で録画しています。止めてから録画してください")),
          begin(u, room, l, mime, share))
    }

  private def begin(u: User, room: Room, l: skaka.core.Live, mime: String, share: Boolean): IO[Either[(Status, String), Recording]] =
    for
      now <- IO.realTimeInstant
      rec  = Recording(UUID.randomUUID(), room.id, l.meetingId, u.id, s"${room.name} ${stamp(now, u.timezone)}".take(200),
               baseMime(mime), share, RecordingStatus.Recording, 0, None, now, None)
      _   <- Files[IO].createDirectories(Path(client.cfg.dir))
      _   <- Files[IO].createFile(file(rec.id))
      _   <- store.create(rec)
      mx  <- Mutex[IO]
      _   <- active.update(_.updated(rec.id, Active(rec, 0, 0, now, mx)))
      _   <- hub.recording(room.id, Some(RecordingNotice(rec.id, u.name, now.toString)), rec.id)
      _   <- skaka.observe.Metrics.count("skaka.recordings.started", "mime" -> rec.mime)
    yield Right(rec)

  /** 断片を足す（seq は 0 から順に. 送り直しで同じものが来たら受け流す）. 返すのはここまでの大きさ. */
  def append(u: User, id: UUID, seq: Int, data: Array[Byte]): IO[Either[(Status, String), Long]] =
    active.get.map(_.get(id)).flatMap {
      case None                         => IO.pure(Left(Status.Conflict -> "この録画は終わっています"))
      case Some(a) if a.rec.userId != u.id => IO.pure(Left(Status.Forbidden -> "録画した人だけが送れます"))
      case Some(a) =>
        a.lock.lock.surround(active.get.map(_.get(id)).flatMap {
          case None                       => IO.pure(Left(Status.Conflict -> "この録画は終わっています"))
          case Some(cur) if seq < cur.seq => IO.pure(Right(cur.bytes))
          case Some(cur) if seq > cur.seq => IO.pure(Left(Status.Conflict -> s"断片が抜けています（${cur.seq} 番目から送ってください）"))
          case Some(cur) if cur.bytes + data.length > client.cfg.maxBytes =>
            IO.pure(Left(Status.PayloadTooLarge -> s"録画の大きさの上限（${client.cfg.maxBytes / 1024 / 1024} MB）を超えました"))
          case Some(cur) =>
            for
              _   <- Stream.chunk(Chunk.array(data)).through(Files[IO].writeAll(file(id), Flags.Append)).compile.drain
              now <- IO.realTimeInstant
              next = cur.copy(seq = seq + 1, bytes = cur.bytes + data.length, last = now)
              _   <- active.update(_.updated(id, next))
            yield Right(next.bytes)
        })
    }

  /** 録り終えた: skawau に置く（裏で. 画面は待たない）. */
  def finish(u: User, id: UUID, durationMs: Option[Long]): IO[Either[(Status, String), Unit]] =
    active.get.map(_.get(id)).flatMap {
      case Some(a) if a.rec.userId != u.id => IO.pure(Left(Status.Forbidden -> "録画した人だけが終えられます"))
      case Some(a)                         => a.lock.lock.surround(stop(id, durationMs)).as(Right(()))
      case None =>
        store.find(id).map {
          case Some(r) if r.userId != u.id => Left(Status.Forbidden -> "録画した人だけが終えられます")
          case Some(_)                     => Right(()) // もう終えている（送り直し）
          case None                        => Left(Status.NotFound -> "録画が見つかりません")
        }
    }

  /** 保存できなかった録画を、もう一度 skawau に置く. */
  def retry(u: User, id: UUID): IO[Either[(Status, String), Unit]] =
    store.find(id).flatMap {
      case Some(r) if r.userId != u.id => IO.pure(Left(Status.Forbidden -> "録画した人だけができます"))
      case Some(r) if r.status != RecordingStatus.Failed => IO.pure(Left(Status.Conflict -> "保存できなかった録画ではありません"))
      case Some(r) =>
        Files[IO].exists(file(id)).flatMap {
          case false => IO.pure(Left(Status.Gone -> "録画の中身がサーバーに残っていません"))
          case true  => store.stopped(id, r.bytes, None, r.endedAt.getOrElse(r.startedAt)) *> sup.supervise(save(id)).as(Right(()))
        }
      case None => IO.pure(Left(Status.NotFound -> "録画が見つかりません"))
    }

  /** 録画を消す（録画中なら止めて捨てる. skawau に置いたファイルは skawau で消す）. */
  def discard(r: Recording): IO[Unit] =
    val remove = active.modify(m => (m - r.id, m.get(r.id))).flatMap(_.traverse_(_ => hub.recording(r.roomId, None, r.id))) *>
      store.delete(r.id) *> Files[IO].deleteIfExists(file(r.id)).void
    // 録画中なら、断片の書き込みと重ならないように
    active.get.map(_.get(r.id)).flatMap(_.fold(remove)(_.lock.lock.surround(remove)))

  /** 録画中のまま断片がしばらく届かない（画面を閉じた・回線が切れた）ものを終える. */
  def sweep(silent: FiniteDuration): IO[Unit] =
    IO.realTimeInstant.flatMap { now =>
      active.get.flatMap(_.values.toList.filter(_.last.plusMillis(silent.toMillis).isBefore(now)).traverse_ { a =>
        a.lock.lock.surround(stop(a.rec.id, Some(now.toEpochMilli - a.rec.startedAt.toEpochMilli)))
      })
    }

  /** 起動したとき: 前に動いていたときの録画中・保存中のものを保存する（中身が残っていれば）. */
  def recover: IO[Int] =
    store.open.flatMap(_.traverse { r =>
      Files[IO].exists(file(r.id)).flatMap {
        case false => store.failed(r.id, "サーバーが止まったため、録画の中身が残っていません")
        case true =>
          Files[IO].size(file(r.id)).flatMap(n => store.stopped(r.id, n, None, r.endedAt.getOrElse(Instant.now()))) *> sup.supervise(save(r.id)).void
      }
    }).map(_.size)

  private def stop(id: UUID, durationMs: Option[Long]): IO[Unit] =
    active.modify(m => (m - id, m.get(id))).flatMap {
      case None => IO.unit
      case Some(a) =>
        IO.realTimeInstant.flatMap(now => store.stopped(id, a.bytes, durationMs.filter(_ > 0), now)) *>
          hub.recording(a.rec.roomId, None, id) *> sup.supervise(save(id)).void
    }

  private type Res[A] = EitherT[IO, String, A]

  private def sk(token: String, userId: UUID)(r: IO[Either[(Int, String), Json]]): Res[Json] =
    EitherT(r.flatMap {
      case Left((401, _)) => links.deleteToken(userId).as(Left("skawau との連携が切れています。設定の画面で連携し直してから、もう一度保存してください"))
      case Left((_, m))   => IO.pure(Left(s"skawau: $m"))
      case Right(j)       => IO.pure(Right(j))
    })

  /** skawau に置き、参加者と共有する. 失敗したら一時ファイルを残す（もう一度保存できる）. 終わったら録画した人にプッシュ通知. */
  private def save(id: UUID): IO[Unit] = skaka.observe.Telemetry.span("recording.save", "recording.id" -> id.toString)(saveImpl(id))

  private def saveImpl(id: UUID): IO[Unit] =
    store.find(id).flatMap {
      case None => Files[IO].deleteIfExists(file(id)).void
      case Some(r) =>
        val path = file(id)
        val go: Res[Unit] =
          for
            enc   <- EitherT.fromOptionF(links.token(r.userId), "skawau と連携していません。設定の画面で連携してから、もう一度保存してください")
            token <- EitherT.fromOption[IO](box.open(enc._1), "skawau のトークンを読めません（SECRET_KEY が変わりました）。連携し直してください")
            size  <- EitherT.liftF(Files[IO].size(path))
            head  <- EitherT.liftF(Files[IO].readRange(path, 64 * 1024, 0, HeadBytes.min(size)).compile.to(Array))
            fixed  = r.durationMs.filter(_ => r.mime.contains("webm")).flatMap(ms => Webm.withDuration(head, ms.toDouble)).getOrElse(head)
            body   = Stream.chunk(Chunk.array(fixed)) ++ Files[IO].readRange(path, 256 * 1024, head.length, size)
            root  <- sk(token, r.userId)(client.call(token, Method.GET, "/api/files/root"))
            folder <- EitherT.fromOption[IO](root.hcursor.downField("folder").get[UUID]("id").toOption, "skawau の応答を読めません")
            month  = DateTimeFormatter.ofPattern("yyyy-MM").withZone(ZoneId.of("Asia/Tokyo")).format(r.startedAt)
            node  <- sk(token, r.userId)(client.put(token, folder, s"$Folder/$month", fileName(r.title) + ext(r.mime), body,
                       fixed.length + size - head.length, r.mime))
            fileId <- EitherT.fromOption[IO](node.hcursor.get[UUID]("id").toOption, "skawau の応答を読めません")
            shared <- EitherT.liftF(if r.share then r.meetingId.fold(IO.pure(0))(share(token, r.userId, fileId, _)) else IO.pure(0))
            _      <- EitherT.liftF(store.saved(id, fileId, s"${client.cfg.url}/view/$fileId", shared))
            _      <- EitherT.liftF(Files[IO].deleteIfExists(path))
          yield ()
        val result = go.value.flatMap {
          case Right(_) => IO.pure(true)
          case Left(m)  => store.failed(id, m).as(false)
        }.handleErrorWith(e => store.failed(id, s"保存できませんでした: ${e.getMessage}").as(false))
        result.flatMap(ok => skaka.observe.Metrics.count("skaka.recordings.saved", "result" -> (if ok then "ok" else "failed")) *> notices.recording(r, ok))
    }

  /** 会議に参加したメンバーと共有する（閲覧のみ. skawau でメールアドレスが一致する人だけ）. 共有できた人数. */
  private def share(token: String, owner: UUID, fileId: UUID, meetingId: UUID): IO[Int] =
    store.participantEmails(meetingId, owner).flatMap {
      case Nil => IO.pure(0)
      case emails =>
        client.call(token, Method.POST, "/api/users/lookup", Some(Json.obj("emails" -> emails.take(100).asJson))).flatMap {
          case Left(_) => IO.pure(0)
          case Right(j) =>
            j.hcursor.downField("users").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[UUID]("id").toOption).distinct
              .traverse(uid => client.call(token, Method.POST, s"/api/files/$fileId/shares",
                Some(Json.obj("kind" -> "user".asJson, "userId" -> uid.asJson, "permission" -> "view".asJson))).map(_.isRight))
              .map(_.count(identity))
        }
    }

object Recorder:
  /** 録画中のもの（seq は次に受け取る断片の番号、last は最後に届いた時刻）. */
  final case class Active(rec: Recording, seq: Int, bytes: Long, last: Instant, lock: Mutex[IO])

  /** skawau の自分のルートの下に作るフォルダ（その下に月ごと）. */
  val Folder = "skaka 録画"

  /** 1つの断片の上限（画面は数秒ごとに送る. 数 MB まで）. */
  val MaxChunk: Int = 32 * 1024 * 1024

  /** これより長く断片が届かなければ、録り終えたものとして保存する（画面は4秒ごとに送る）. */
  val Silent: FiniteDuration = 90.seconds

  /** WebM の長さを直すときに読む先頭（Info はこの中にある）. */
  private val HeadBytes = 256L * 1024

  /** 受け付ける録画の形式（MediaRecorder の mimeType の ; より前）. */
  val Mimes: Set[String] = Set("video/webm", "video/mp4")

  def baseMime(mime: String): String = mime.takeWhile(_ != ';').trim.toLowerCase

  /** skawau に置くファイルの名前（会議室の名前から. パスの区切り・制御文字を除く）. */
  def fileName(title: String): String =
    title.map(c => if c == '/' then '／' else if c == '\\' then '＼' else c).filterNot(_.isControl).trim.stripPrefix(".")

  def ext(mime: String): String = if mime.contains("mp4") then ".mp4" else if mime.contains("ogg") then ".ogg" else ".webm"

  def resource(store: RecordingStore, links: SkawauStore, client: SkawauClient, box: SecretBox, hub: Hub,
               notices: Notices = Notices.none): Resource[IO, Recorder] =
    for
      sup    <- Supervisor[IO](await = false)
      active <- Resource.eval(Ref.of[IO, Map[UUID, Active]](Map.empty))
      rec     = new Recorder(store, links, client, box, hub, sup, active, notices)
      n      <- Resource.eval(rec.recover)
      _      <- Resource.eval(IO.whenA(n > 0)(skaka.observe.Log.println(s"[skaka] saving $n recording(s) left by the previous run")))
      _      <- (IO.sleep(15.seconds) *> rec.sweep(Silent).handleErrorWith(e => skaka.observe.Log.println(s"[skaka] recording sweep failed: $e"))).foreverM.background
    yield rec
