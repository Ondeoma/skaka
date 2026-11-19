package skaka.api

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.http4s.multipart.{Multiparts, Part}

/**
 * 自前の Whisper のサーバー（OpenAI 互換の `POST {url}/audio/transcriptions`. speaches・faster-whisper-server・LocalAI・
 * whisper.cpp の server（`--inference-path /v1/audio/transcriptions`）など）.
 * url は `http://whisper:8000/v1` のように /v1 まで. model はそのサーバーのモデルの名前. concurrency は同時に送る数.
 */
final case class WhisperConfig(url: String, model: String, language: String = "ja", apiKey: Option[String] = None, concurrency: Int = 2,
                               prompt: Option[String] = None)

/** 音声を文字にする（会議の文字起こし）. */
trait SpeechToText:
  def transcribe(audio: Array[Byte], mime: String): IO[Either[String, String]]

final class WhisperClient(cfg: WhisperConfig, http: Client[IO], slots: Semaphore[IO], waiting: Ref[IO, Int]) extends SpeechToText:

  private val uri = Uri.unsafeFromString(cfg.url.stripSuffix("/") + "/audio/transcriptions")

  private def ext(mime: String): String =
    if mime.contains("wav") then "wav" else if mime.contains("ogg") then "ogg" else if mime.contains("mp4") then "m4a" else "webm"

  /** 混んでいる（待ちが多い）ときは断る（会議の字幕は遅れて届いても役に立たないため）. */
  def transcribe(audio: Array[Byte], mime: String): IO[Either[String, String]] =
    waiting.modify(n => if n >= cfg.concurrency * Transcriber.Backlog then (n, false) else (n + 1, true)).flatMap {
      case false => IO.pure(Left("文字起こしが混んでいます"))
      // 接続できなかったとき（使い回した接続が切られていたなど）は、1回だけ送り直す
      case true  => slots.permit.surround(send(audio, mime).flatMap {
                      case Left((502, _)) => send(audio, mime)
                      case r              => IO.pure(r)
                    }).map(_.left.map(_._2)).guarantee(waiting.update(_ - 1))
    }

  private def send(audio: Array[Byte], mime: String): IO[Either[(Int, String), String]] =
    val ct = MediaType.parse(mime).toOption.getOrElse(MediaType.application.`octet-stream`)
    val fields = Vector[Part[IO]](Part.formData("model", cfg.model), Part.formData("language", cfg.language),
      Part.formData("response_format", "json"), Part.formData("temperature", "0")) ++
      cfg.prompt.map(Part.formData("prompt", _)).toVector
    Multiparts.forSync[IO].flatMap(_.multipart(fields :+ Part.fileData[IO]("file", s"speech.${ext(mime)}", Entity.strict(scodec.bits.ByteVector(audio)), `Content-Type`(ct))))
      .flatMap { mp =>
        // 大きさ（Content-Length）を付けて送る（chunked の本文を読めないサーバーがある. 音声は 2MB までなので、まとめてよい）
        val r0 = Request[IO](Method.POST, uri).withEntity(mp)
        // 見出しは Content-Type（boundary）だけを使う（マルチパートの見出しには chunked の指定も入っている）
        r0.body.compile.to(Array).map { bytes =>
          val req = Request[IO](Method.POST, uri).withEntity(bytes)(using EntityEncoder.byteArrayEncoder)
          mp.headers.get[`Content-Type`].fold(req)(req.withContentType)
        }
      }
      .flatMap { req =>
        RemoteJson.run(http, cfg.apiKey.fold(req)(k => req.putHeaders(RemoteJson.bearer(k))), "Whisper").map {
          case Left(e)  => Left(e)
          case Right(j) => Right(j.hcursor.get[String]("text").getOrElse(j.asString.getOrElse("")))
        }
      }

object WhisperClient:
  def create(cfg: WhisperConfig, http: Client[IO]): IO[WhisperClient] =
    (Semaphore[IO](cfg.concurrency.max(1).toLong), Ref.of[IO, Int](0)).mapN(new WhisperClient(cfg, http, _, _))

/** 文字起こしの決まり（受け付ける音声・Whisper の空振りの文の除去）. */
object Transcriber:
  /** 1回に送れる音声の上限（画面は話の切れ目か20秒ごとに、16kHz・16bit の WAV で送る. 20秒で約 640KB）. */
  val MaxBytes: Int = 2 * 1024 * 1024

  /** 同時に送る数のこの倍まで待たせる. それより多ければ断る. */
  val Backlog = 4

  val Mimes: Set[String] = Set("audio/wav", "audio/x-wav", "audio/wave", "audio/webm", "audio/ogg", "audio/mp4")

  /**
   * Whisper が無音や雑音から作りがちな文（動画の締めの決まり文句など）. 文字起こしの結果がこれだけなら捨てる.
   * 比べるときは空白・句読点を除く.
   */
  private val Hallucinations: Set[String] = Set(
    "ご視聴ありがとうございました", "ご視聴ありがとうございます", "チャンネル登録よろしくお願いします", "チャンネル登録お願いします",
    "字幕は自動生成されています", "最後までご視聴いただきありがとうございました", "次回もお楽しみに",
    "thankyouforwatching", "thanksforwatching"
  ).map(norm)

  private def norm(s: String): String = s.toLowerCase.filterNot(c => c.isWhitespace || "、。，．,.!！?？・…「」『』()（）♪〜~ー-".contains(c))

  /** 文字起こしの結果を整える（空・空振りなら None）. */
  def clean(text: String): Option[String] =
    val t = text.replaceAll("[\\s\\u3000]+", " ").trim
    val n = norm(t)
    Option.when(n.nonEmpty && !Hallucinations(n) && !n.forall(c => c == n.head))(t.take(2000))

/**
 * 文字起こしの音声を受け取る: `POST /api/speech/{key}?ago=…`（本文は音声. key は切符と一緒に発行した鍵、ago は話し始めてからの ms）.
 * 話した人の画面が、自分のマイクの音を話の切れ目ごとに送る（メッシュなので、音声は最初から人ごとに分かれている. 話者を推定しなくてよい）.
 * 文字にできたら、会議室の全員に字幕として送り、会議の記録に残す. ゲストも送れるよう、ログインではなく鍵で確かめる.
 */
final class SpeechRoutes(hub: Hub, stt: SpeechToText) extends org.http4s.dsl.Http4sDsl[IO]:
  import org.http4s.circe.CirceEntityCodec.*
  import skaka.shared.*

  private def err(s: Status, m: String): IO[Response[IO]] = IO.pure(Response[IO](s).withEntity(ErrorDto(m)))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "api" / "speech" / key =>
      val mime = req.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}".toLowerCase).getOrElse("")
      val ago  = req.params.get("ago").flatMap(_.toLongOption).getOrElse(0L).max(0L).min(120000L)
      hub.speaker(key).flatMap {
        case None                                   => err(Status.Forbidden, "会議に入っていません")
        case Some((_, live, _)) if live.transcribing.isEmpty => err(Status.Conflict, "文字起こしは止まっています")
        case Some(_) if !Transcriber.Mimes(mime)    => err(Status.UnsupportedMediaType, "音声は WAV・WebM・Ogg・MP4 で送ってください")
        case Some((room, _, peer)) =>
          for
            start <- IO.realTimeInstant.map(_.minusMillis(ago))
            data  <- req.body.take(Transcriber.MaxBytes + 1L).compile.to(Array)
            res <-
              if data.length > Transcriber.MaxBytes then err(Status.PayloadTooLarge, "音声が長すぎます")
              else if !peer.media.audio then NoContent() // マイクを切った直後に届いたもの
              else
                skaka.observe.Telemetry.span("speech.transcribe", "bytes" -> data.length.toString)(stt.transcribe(data, mime)).flatMap {
                  case Left(m) => skaka.observe.Metrics.count("skaka.transcript.failed") *> err(Status.ServiceUnavailable, s"文字にできませんでした: $m")
                  case Right(text) =>
                    Transcriber.clean(text) match
                      case None => NoContent()
                      case Some(t) =>
                        val line = TranscriptLine(java.util.UUID.randomUUID().toString, peer.id, peer.name, peer.userId, t, start.toString)
                        hub.caption(room, line) *> Ok(line)
                }
          yield res
      }
  }
