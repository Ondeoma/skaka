package skaka.api

import skaka.core.ErrorCode
import cats.effect.*
import cats.syntax.all.*
import org.http4s.*

/**
 * 文字起こしの音声を受け取る: `POST /api/speech/{key}?ago=…`（本文は音声. key は切符と一緒に発行した鍵、ago は話し始めてからの ms）.
 * 話した人の画面が、自分のマイクの音を話の切れ目ごとに送る（メッシュなので、音声は最初から人ごとに分かれている. 話者を推定しなくてよい）.
 * 文字にできたら、会議室の全員に字幕として送り、会議の記録に残す. ゲストも送れるよう、ログインではなく鍵で確かめる.
 */
final class SpeechRoutes(
  hub: Hub,
  stt: SpeechToText) extends org.http4s.dsl.Http4sDsl[IO] {
  import org.http4s.circe.CirceEntityCodec.*
  import skaka.shared.*

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "api" / "speech" / key => postSpeech(req, key)
  }

  private def postSpeech(
    req: Request[IO],
    key: String
  ): IO[Response[IO]] = {
    val mime = req.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}".toLowerCase).getOrElse("")
    val ago = req.params.get("ago").flatMap(_.toLongOption).getOrElse(0L).max(0L).min(120000L)
    hub.speaker(key).flatMap {
      case None => err(ErrorCode.NotInMeeting)
      case Some((_, live, _)) if live.transcribing.isEmpty => err(ErrorCode.TranscriptionStopped)
      case Some(_) if !Transcriber.Mimes(mime) => err(ErrorCode.AudioTypeUnsupported)
      case Some((room, _, peer)) =>
        for {
          start <- IO.realTimeInstant.map(_.minusMillis(ago))
          data <- req.body.take(Transcriber.MaxBytes + 1L).compile.to(Array)
          res <-
            if data.length > Transcriber.MaxBytes then err(ErrorCode.AudioTooLong)
            else if !peer.media.audio then NoContent() // マイクを切った直後に届いたもの
            else
              sbirds.observe.Telemetry.span(
                "speech.transcribe",
                "bytes" -> data.length.toString
              )(stt.transcribe(data, mime)).flatMap {
                case Left(m) => sbirds.observe.Metrics.count("skaka.transcript.failed") *>
                    err(ErrorCode.TranscriptionFailed, s"文字にできませんでした: $m")
                case Right(text) =>
                  Transcriber.clean(text) match {
                    case None => NoContent()
                    case Some(t) =>
                      val line = TranscriptLine(
                        java.util.UUID.randomUUID().toString,
                        peer.id,
                        peer.name,
                        peer.userId,
                        t,
                        start.toString
                      )
                      hub.caption(room, line) *> Ok(line)
                  }
              }
        } yield res
    }
  }

  private def err(
    code: ErrorCode,
    detail: String | Null = null
  ): IO[Response[IO]] = sbirds.http.Errors.respond(sbirds.shared.AppError(code, Option(detail)))
}
