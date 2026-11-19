package skaka.api

import cats.effect.*
import skaka.shared.*

/**
 * 1つの接続から受け付けるメッセージの数の上限（10秒ごと. 超えた分は捨てる）. 全員に送られるチャット・リアクションは少なめに.
 * 中継（SDP・ICE）は、大人数で入った直後にまとめて届くので多めにしてある.
 */
final class Flood private (
  ref: Ref[IO, (Long, Int, Int)]) {
  import Flood.*

  def allow(
    m: ClientMsg
  ): IO[Boolean] = IO.monotonic.flatMap { t =>
    val now = t.toMillis
    val broad = m match {
      case _: ClientMsg.Chat | _: ClientMsg.React => true
      case _ => false
    }
    ref.modify {
      (
        start,
        all,
        chat
      ) =>
        {
          val (s, a, c) = if now - start >= WindowMillis then (now, 0, 0) else (start, all, chat)
          val ok = a < MaxMessages && (!broad || c < MaxBroadcasts)
          ((s, a + 1, if broad then c + 1 else c), ok)
        }
    }
  }
}

object Flood {
  val WindowMillis = 10000L
  val MaxMessages = 3000
  val MaxBroadcasts = 40

  def create: IO[Flood] = IO.monotonic.flatMap(t => Ref.of[IO, (Long, Int, Int)]((t.toMillis, 0, 0))).map(new Flood(_))
}
