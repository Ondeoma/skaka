package skaka.api

import skaka.core.ErrorCode
import sbirds.shared.AppError
import cats.data.EitherT
import cats.effect.*
import cats.effect.std.Supervisor
import cats.syntax.all.*
import fs2.{Chunk, Stream}
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.headers.`Content-Type`
import skaka.db.*
import skaka.shared.*

import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.concurrent.duration.*
import scala.util.Try

/**
 * 議事録・要約を skakapo に頼む. 頼んだ人の skakapo の API トークンで、会議の記録（参加者・文字起こし・チャット）を Markdown のファイルとして添付した
 * 依頼（会話）を作り、エージェントの返事を待って取り込む. 返事に満足できなければ、skakapo の会話で直してもらい「取り込み直す」.
 * 取り込んだら、頼んだ人の skawau に Markdown のファイルとして置き、会議に参加したメンバーと共有する（files. skawau を設定したときだけ）.
 * 取り込み直したら、同じファイルの中身を書き換える（skawau に版が残る）.
 */
final class MinutesService private (store: MinutesStore,
                                    meetings: MeetingStore,
                                    val link: TokenLinks,
                                    cfg: SkakapoConfig,
                                    sup: Supervisor[IO],
                                    interval: FiniteDuration,
                                    limit: FiniteDuration,
                                    files: Option[SkawauFiles],
                                    publicUrl: String) {
  import MinutesService.*
  import link.{Res, reject}

  private def zone(u: User): ZoneId = Try(ZoneId.of(u.timezone)).getOrElse(ZoneId.of("Asia/Tokyo"))

  /** 議事録を頼む（会議を見られる人なら誰でも. 頼んだ人の skakapo の会話になる）. */
  def start(u: User, meetingId: UUID): IO[Either[AppError, MinutesDto]] =
    (for {
      d <- EitherT.fromOptionF(meetings.detail(u.id, meetingId), AppError(ErrorCode.MeetingNotFound))
      _ <- if d.transcript.isEmpty && d.chat.isEmpty then reject[Unit](AppError(ErrorCode.MinutesNoContent))
      else EitherT.rightT[IO, AppError](())
      ln <- link.linked(u)
      (c, token, l) = ln
      _ <- if l.canWrite then EitherT.rightT[IO, AppError](())
      else reject[Unit](AppError(ErrorCode.MinutesTokenReadOnly))
      fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm").withZone(zone(u))
      hm = DateTimeFormatter.ofPattern("HH:mm").withZone(zone(u))
      time = (s: String) => Try(hm.format(Instant.parse(s))).getOrElse(s)
      title =
        s"${d.meeting.roomName}（${Try(fmt.format(Instant.parse(d.meeting.startedAt))).getOrElse(d.meeting.startedAt)}〜${d.meeting.endedAt.fold("開催中")(time)}）"
      text = MeetingText.markdown(d, title, time)
      att <- upload(c, token, text)
      ses <- link.call(
        c,
        token,
        Method.POST,
        "/api/sessions",
        Some(Json.obj(
          "requestType" -> cfg.requestType.asJson,
          "values" -> Json.obj("title" -> s"議事録: $title".take(200).asJson, "description" -> instructions(title).asJson),
          "attachments" -> List(att).asJson
        ))
      )
      sid <- EitherT.fromOption[IO](ses.hcursor.get[UUID]("id").toOption, AppError(ErrorCode.SkakapoBadResponse))
      now <- EitherT.liftF(IO.realTimeInstant)
      m = Minutes(
        UUID.randomUUID(),
        meetingId,
        u.id,
        sid,
        s"${c.base}/sessions/$sid",
        title.take(300),
        MinutesStatus.Running,
        "",
        None,
        None,
        None,
        None,
        now,
        now
      )
      _ <- EitherT.liftF(store.create(m) *> sup.supervise(follow(m)).void *>
        sbirds.observe.Metrics.count("skaka.minutes.requested"))
    } yield m.dto(u.id, u.name)).value

  /** 会議の記録を skakapo に添付する（ID を返す）. */
  private def upload(c: TokenApi, token: String, text: String): Res[UUID] =
    EitherT.fromEither[IO](c.uri("/api/attachments?name=" +
      java.net.URLEncoder.encode(AttachmentName, "UTF-8")).left.map(AppError(ErrorCode.MinutesAttachmentUrlInvalid, _)))
      .flatMap { u =>
        val req = Request[IO](Method.POST, u).withEntity(text)(using EntityEncoder.stringEncoder)
          .withContentType(`Content-Type`(MediaType.text.markdown, Charset.`UTF-8`))
        link.result(c.send(token, req))
      }
      .flatMap(j =>
        EitherT.fromOption[IO](j.hcursor.get[UUID]("id").toOption, AppError(ErrorCode.SkakapoBadResponse))
      )

  /** skakapo の返事を、もう一度取り込む（skakapo の会話で直してもらったあと. 頼んだ人だけ）. */
  def refresh(u: User, id: UUID): IO[Either[AppError, Unit]] =
    store.find(id).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.MinutesNotFound)))
      case Some(m) if m.userId != u.id => IO.pure(Left(AppError(ErrorCode.MinutesRefreshRequesterOnly)))
      case Some(m) =>
        read(m, 0L, 0).value.flatMap {
          case Left((_, e)) => IO.pure(Left(AppError(ErrorCode.MinutesSkakapoFailed, e)))
          case Right(Progress(_, _, None, Some(err))) =>
            store.update(id, MinutesStatus.Failed, None, Some(err)).as(Right(()))
          case Right(Progress(st, _, text, _)) =>
            if st == Busy then
              store.update(id, MinutesStatus.Running, None, None) *> sup.supervise(follow(m)).as(Right(()))
            else
              store.update(
                id,
                if text.isDefined then MinutesStatus.Done else MinutesStatus.Failed,
                text,
                Option.when(text.isEmpty)("skakapo の返事がありません")
              ) *> IO.whenA(text.isDefined)(file(id)).as(Right(()))
        }
    }

  def delete(u: User, id: UUID): IO[Either[AppError, Unit]] =
    store.find(id).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.MinutesNotFound)))
      case Some(m) if m.userId != u.id => IO.pure(Left(AppError(ErrorCode.MinutesDeleteRequesterOnly)))
      case Some(m) => store.delete(m.id).as(Right(()))
    }

  /** 進み具合: 会話の状態（Busy・Idle・Failed）、読んだ最後の出来事、いちばん新しい返事、失敗の文言. */
  private final case class Progress(state: Int, after: Long, text: Option[String], error: Option[String])

  /** skakapo の会話の出来事を読む（wait 秒まで、新しい出来事を待つ）. */
  private def read(m: Minutes, after: Long, wait: Int): EitherT[IO, (Int, String), Progress] =
    EitherT(link.linkedFor(m.userId).value.map(_.left.map(e => 0 -> e.message))).flatMap {
      (c, token, _) =>
        EitherT(c.call(token, Method.GET, s"/api/sessions/${m.sessionId}/events?after=$after&wait=$wait")).map { j =>
          val c0 = j.hcursor
          val status = c0.downField("session").get[String]("status").getOrElse("")
          val events = c0.downField("events").as[List[Json]].getOrElse(Nil).map(_.hcursor)
          val last = events.flatMap(_.get[Long]("id").toOption).maxOption.getOrElse(after)
          def texts(kind: String) = events.filter(
            _.get[String]("kind").toOption.contains(kind)
          ).flatMap(_.downField("data").get[String]("text").toOption)
          val state = status match {
            case "idle" => Idle
            case "failed" => Failed
            case _ => Busy
          }
          Progress(
            state,
            last,
            texts("assistant").lastOption.map(_.trim).filter(_.nonEmpty),
            Option.when(state == Failed)(texts("error").lastOption.getOrElse("skakapo の作業が失敗しました"))
          )
        }
    }

  /** 返事が出るまで待って取り込む（limit を過ぎたら、取り込み直しを促して終える）. */
  private def follow(m: Minutes): IO[Unit] = {
    def loop(after: Long, text: Option[String], deadline: Long): IO[Unit] =
      read(m, after, interval.toSeconds.toInt.min(25)).value.flatMap {
        case Left((401, _)) =>
          store.update(m.id, MinutesStatus.Failed, None, Some("skakapo の API トークンが使えません。設定の画面で登録し直してください"))
        case Left((_, e)) =>
          IO.monotonic.flatMap(t =>
            if t.toMillis > deadline then store.update(m.id, MinutesStatus.Failed, None, Some(e))
            else IO.sleep(interval) *> loop(after, text, deadline)
          )
        case Right(p) =>
          val latest = p.text.orElse(text)
          if p.state == Failed then store.update(m.id, MinutesStatus.Failed, latest, p.error)
          else if p.state == Idle && latest.isDefined then
            store.update(m.id, MinutesStatus.Done, latest, None) *>
              sbirds.observe.Metrics.count("skaka.minutes.done") *>
              file(m.id)
          else
            IO.monotonic.flatMap { t =>
              if t.toMillis > deadline then
                store.update(
                  m.id,
                  MinutesStatus.Failed,
                  latest,
                  Some("skakapo の返事を待ちきれませんでした。skakapo の会話を確かめて「取り込み直す」を押してください")
                )
              else (if p.after == after then IO.sleep(interval) else IO.unit) *> loop(p.after, latest, deadline)
            }
      }
    IO.monotonic.flatMap(t => loop(0L, None, t.toMillis + limit.toMillis))
      .handleErrorWith(e => store.update(m.id, MinutesStatus.Failed, None, Some(s"取り込めませんでした: ${e.getMessage}")))
  }

  /** skawau に置く（もう一度置くときも. 頼んだ人だけ）. */
  def saveFile(u: User, id: UUID): IO[Either[AppError, Unit]] =
    store.find(id).flatMap {
      case None => IO.pure(Left(AppError(ErrorCode.MinutesNotFound)))
      case Some(m) if m.userId != u.id => IO.pure(Left(AppError(ErrorCode.MinutesSaveRequesterOnly)))
      case Some(_) if files.isEmpty => IO.pure(Left(AppError(ErrorCode.SkawauNotConfigured)))
      case Some(m) if m.body.isEmpty => IO.pure(Left(AppError(ErrorCode.MinutesNotReady)))
      case Some(m) => file(m.id).as(Right(()))
    }

  /** skawau に置くファイルの中身（議事録と、会議の記録・skakapo の会話への案内）. */
  private def document(m: Minutes): String =
    s"""# 議事録: ${m.title}
       |
       |${m.body.trim}
       |
       |---
       |- skaka の会議の記録: $publicUrl/meetings/${m.meetingId}
       |- skakapo の会話: ${m.sessionUrl}
       |""".stripMargin

  /**
   * 取り込んだ議事録を skawau に置く. 置いたことがあれば中身を書き換え（消されていたら置き直す）、初めて置いたときは会議に参加したメンバーと共有する.
   * 置けなかったら理由を残す（「skawau に保存」でもう一度置ける）.
   */
  private def file(id: UUID): IO[Unit] = files.fold(IO.unit) { f =>
    store.find(id).flatMap {
      case Some(m) if m.body.nonEmpty =>
        val data = document(m).getBytes(java.nio.charset.StandardCharsets.UTF_8)
        val month = DateTimeFormatter.ofPattern("yyyy-MM").withZone(ZoneId.of("Asia/Tokyo")).format(m.createdAt)
        val go = for {
          token <- f.token(m.userId)
          before <-
            m.fileId.fold(EitherT.rightT[IO, String](Option.empty[UUID]))(f.write(token, m.userId, _, data, Mime))
          fileId <- before.fold(
            f.put(
              token,
              m.userId,
              s"$Folder/$month",
              Recorder.fileName(s"議事録 ${m.title}") + ".md",
              Stream.chunk(Chunk.array(data)),
              data.length.toLong,
              Mime
            ).semiflatTap(fid => f.share(token, m.userId, fid, m.meetingId))
          )(fid => EitherT.rightT[IO, String](fid))
        } yield fileId
        go.value.flatMap {
          case Right(fid) => store.filed(id, Some(fid), Some(f.url(fid)), None) *>
              sbirds.observe.Metrics.count("skaka.minutes.filed")
          case Left(e) => store.filed(id, None, None, Some(e))
        }.handleErrorWith(e => store.filed(id, None, None, Some(s"skawau に保存できませんでした: ${e.getMessage}")))
      case _ => IO.unit
    }
  }

  /** skakapo の画面の URL. */
  def url: String = cfg.url

  def forMeeting(viewer: UUID, meetingId: UUID): IO[List[MinutesDto]] = store.forMeeting(viewer, meetingId)

  /** 起動したとき: 前に動いていたときに待っていたものの続きを見る. */
  def recover: IO[Int] = store.running.flatMap(_.traverse(m => sup.supervise(follow(m)))).map(_.size)
}

object MinutesService {
  private val Busy = 0
  private val Idle = 1
  private val Failed = 2

  val AttachmentName = "会議の記録.md"

  /** skawau の自分のルートの下に作るフォルダ（その下に月ごと）と、置くファイルの種類. */
  val Folder = "skaka 議事録"
  val Mime = "text/markdown"

  /** skakapo のエージェントへの頼み方（依頼の「内容」）. */
  def instructions(title: String): String =
    s"""添付の「$AttachmentName」は、Web 会議（skaka）「$title」の記録です（参加者・音声の文字起こし・チャット）。
       |これをもとに、日本語で議事録と要約を作ってください。
       |
       |- 文字起こしは音声認識の結果なので、誤変換や言い間違いがあります。文脈から補って読みやすくし、確かでないところには（？）を付けてください。
       |- 話した人の名前は、文字起こしの「名前:」のとおりです（人ごとのマイクから文字にしているので、取り違えはありません）。
       |- ほかのサービスへの書き込みや、確認の質問はしないでください。議事録の本文だけを Markdown で返してください（前置き・後書きは不要です）。
       |
       |形式:
       |## 要約
       |（3〜5行で、会議の目的と結論）
       |## 決まったこと
       |## 宿題（担当・期限）
       |## 話し合ったこと
       |（話題ごとに、主な意見と経緯）
       |## 次回までに確かめること""".stripMargin

  /** files は skawau に置くとき（skawau を設定したときだけ）、publicUrl は skaka の URL（ファイルに会議の記録への案内を書く）. */
  def resource(store: MinutesStore,
               meetings: MeetingStore,
               link: TokenLinks,
               cfg: SkakapoConfig,
               files: Option[SkawauFiles],
               publicUrl: String,
               interval: FiniteDuration = 3.seconds,
               limit: FiniteDuration = 60.minutes): Resource[IO, MinutesService] =
    for {
      sup <- Supervisor[IO](await = false)
      svc = new MinutesService(store, meetings, link, cfg, sup, interval, limit, files, publicUrl)
      n <- Resource.eval(svc.recover)
      _ <- Resource.eval(
        IO.whenA(n > 0)(sbirds.observe.Log.println(s"[skaka] following $n minutes request(s) left by the previous run"))
      )
    } yield svc
}
