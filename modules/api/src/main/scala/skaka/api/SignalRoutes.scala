package skaka.api

import skaka.core.ErrorCode
import cats.effect.*
import fs2.Stream
import io.circe.parser.decode
import io.circe.syntax.*
import org.http4s.*
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import skaka.shared.*

/** 会議中のやり取り（WebSocket）. 切符は POST /api/rooms/{code}/join で発行する. */
object SignalRoutes {

  /** 1つのメッセージの上限（SDP は数 KB. それより大きいものは読まない）. */
  val MaxFrame = 64 * 1024

  def apply(
    hub: Hub,
    ws: WebSocketBuilder[IO]
  ): HttpRoutes[IO] = {
    val dsl = org.http4s.dsl.io
    import dsl.*
    HttpRoutes.of[IO] {
      case GET -> Root / "ws" / ticket =>
        hub.connect(ticket).flatMap {
          case None => sbirds.http.Errors.respond(ErrorCode.JoinTicketExpired)
          case Some((peer, q)) =>
            Flood.create.flatMap { flood =>
              // 接続中の数（メトリクス）は、送る側のストリームが続いている間
              val conn = Stream.bracket(sbirds.observe.Metrics.upDown("skaka.ws.connections", 1))(_ =>
                sbirds.observe.Metrics.upDown("skaka.ws.connections", -1)
              )
              val send = conn >>
                (Stream.fromQueueNoneTerminated(q).map(m => WebSocketFrame.Text(m.asJson.noSpaces)) ++
                  Stream.emit(WebSocketFrame.Close()))
              val receive: fs2.Pipe[IO, WebSocketFrame, Unit] =
                _.collect { case WebSocketFrame.Text(t, _) if t.length <= MaxFrame => t }
                  .evalMap(t =>
                    decode[ClientMsg](t).fold(_ => IO.unit, m => flood.allow(m).ifM(hub.receive(peer, m), IO.unit))
                  )
                  .onFinalize(hub.disconnect(peer))
              ws.withOnClose(hub.disconnect(peer)).build(send, receive)
            }
        }
    }
  }
}
