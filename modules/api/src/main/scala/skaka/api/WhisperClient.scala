package skaka.api

import cats.effect.*
import cats.effect.std.Semaphore
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.`Content-Type`
import org.http4s.multipart.{Multiparts, Part}

final class WhisperClient(cfg: WhisperConfig, http: Client[IO], slots: Semaphore[IO], waiting: Ref[IO, Int])
  extends SpeechToText {

  private val uri = Uri.unsafeFromString(cfg.url.stripSuffix("/") + "/audio/transcriptions")

  private def ext(mime: String): String =
    if mime.contains("wav") then "wav"
    else if mime.contains("ogg") then "ogg" else if mime.contains("mp4") then "m4a" else "webm"

  /** 混んでいる（待ちが多い）ときは断る（会議の字幕は遅れて届いても役に立たないため）. */
  def transcribe(audio: Array[Byte], mime: String): IO[Either[String, String]] =
    waiting.modify(n => if n >= cfg.concurrency * Transcriber.Backlog then (n, false) else (n + 1, true)).flatMap {
      case false => IO.pure(Left("文字起こしが混んでいます"))
      // 接続できなかったとき（使い回した接続が切られていたなど）は、1回だけ送り直す
      case true => slots.permit.surround(send(audio, mime).flatMap {
          case Left((502, _)) => send(audio, mime)
          case r => IO.pure(r)
        }).map(_.left.map(_._2)).guarantee(waiting.update(_ - 1))
    }

  private def send(audio: Array[Byte], mime: String): IO[Either[(Int, String), String]] = {
    val ct = MediaType.parse(mime).toOption.getOrElse(MediaType.application.`octet-stream`)
    val fields = Vector[Part[IO]](
      Part.formData("model", cfg.model),
      Part.formData("language", cfg.language),
      Part.formData("response_format", "json"),
      Part.formData("temperature", "0")
    ) ++
      cfg.prompt.map(Part.formData("prompt", _)).toVector
    Multiparts.forSync[IO].flatMap(_.multipart(fields :+ Part.fileData[IO](
      "file",
      s"speech.${ext(mime)}",
      Entity.strict(scodec.bits.ByteVector(audio)),
      `Content-Type`(ct)
    )))
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
          case Left(e) => Left(e)
          case Right(j) => Right(j.hcursor.get[String]("text").getOrElse(j.asString.getOrElse("")))
        }
      }
  }
}

object WhisperClient {
  def create(cfg: WhisperConfig, http: Client[IO]): IO[WhisperClient] =
    (Semaphore[IO](cfg.concurrency.max(1).toLong), Ref.of[IO, Int](0)).mapN(new WhisperClient(cfg, http, _, _))
}
