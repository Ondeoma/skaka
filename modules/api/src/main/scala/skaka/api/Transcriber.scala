package skaka.api

import cats.effect.*

/**
 * 自前の Whisper のサーバー（OpenAI 互換の `POST {url}/audio/transcriptions`. speaches・faster-whisper-server・LocalAI・
 * whisper.cpp の server（`--inference-path /v1/audio/transcriptions`）など）.
 * url は `http://whisper:8000/v1` のように /v1 まで. model はそのサーバーのモデルの名前. concurrency は同時に送る数.
 */
final case class WhisperConfig(url: String,
                               model: String,
                               language: String = "ja",
                               apiKey: Option[String] = None,
                               concurrency: Int = 2,
                               prompt: Option[String] = None)

/** 音声を文字にする（会議の文字起こし）. */
trait SpeechToText {
  def transcribe(audio: Array[Byte], mime: String): IO[Either[String, String]]
}

/** 文字起こしの決まり（受け付ける音声・Whisper の空振りの文の除去）. */
object Transcriber {

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
    "ご視聴ありがとうございました",
    "ご視聴ありがとうございます",
    "チャンネル登録よろしくお願いします",
    "チャンネル登録お願いします",
    "字幕は自動生成されています",
    "最後までご視聴いただきありがとうございました",
    "次回もお楽しみに",
    "thankyouforwatching",
    "thanksforwatching"
  ).map(norm)

  private def norm(s: String): String =
    s.toLowerCase.filterNot(c => c.isWhitespace || "、。，．,.!！?？・…「」『』()（）♪〜~ー-".contains(c))

  /** 文字起こしの結果を整える（空・空振りなら None）. */
  def clean(text: String): Option[String] = {
    val t = text.replaceAll("[\\s\\u3000]+", " ").trim
    val n = norm(t)
    Option.when(n.nonEmpty && !Hallucinations(n) && !n.forall(c => c == n.head))(t.take(2000))
  }
}
