package skaka.api

class TranscriberSpec extends munit.FunSuite {

  test("Whisper の結果を整える: 空白をまとめる・空振りの決まり文句と同じ音の繰り返しは捨てる") {
    assertEquals(Transcriber.clean("  では　始めます。\n よろしく "), Some("では 始めます。 よろしく"))
    assertEquals(Transcriber.clean("ありがとうございました"), Some("ありがとうございました"))
    for t <- List("", "   ", "ご視聴ありがとうございました。", "チャンネル登録よろしくお願いします！", "Thank you for watching.", "ああああ", "…") do
      assertEquals(Transcriber.clean(t), None, t)
  }
}

class WhisperClientSpec extends munit.CatsEffectSuite {
  import cats.effect.*
  import io.circe.Json
  import org.http4s.*
  import org.http4s.circe.CirceEntityCodec.*
  import org.http4s.client.Client
  import org.http4s.dsl.io.*
  import org.http4s.multipart.Multipart

  test("Whisper（OpenAI 互換）に、モデル・言語・音声をマルチパートで、大きさを付けて送る") {
    val audio = Array.tabulate[Byte](5000)(_.toByte)
    val app = HttpRoutes.of[IO] {
      case req @ POST -> Root / "v1" / "audio" / "transcriptions" =>
        val len = req.contentLength
        req.as[Multipart[IO]].flatMap { mp =>
          def field(
            n: String
          ) = mp.parts.find(_.name.contains(n)).fold(IO.pure(""))(_.bodyText.compile.string)
          for {
            model <- field("model")
            lang <- field("language")
            file <- mp.parts.find(_.name.contains("file")).fold(IO.pure(Array.empty[Byte]))(_.body.compile.to(Array))
            auth = req.headers.get[headers.Authorization].map(_.credentials.toString)
            res <- Ok(Json.obj("text" -> Json.fromString(
              s"$model/$lang/${file.length}/${file.sameElements(audio)}/${len.isDefined}/${auth.getOrElse("")}"
            )))
          } yield res
        }
    }.orNotFound
    val cfg = WhisperConfig("http://whisper.test/v1/", "kotoba-whisper", apiKey = Some("k1"))
    WhisperClient.create(cfg, Client.fromHttpApp(app)).flatMap(_.transcribe(audio, "audio/wav")).map { r =>
      assertEquals(r, Right("kotoba-whisper/ja/5000/true/true/Bearer k1"))
    }
  }
}
