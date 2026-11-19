package skaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import skaka.db.*

/** アプリのルートをまとめる（REST API・会議の WebSocket・MCP）. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes {

  /** recorder は skawau（録画の保存先）、stt は Whisper（文字起こし）、minutes は skakapo（議事録）を設定したときだけ. */
  final case class Deps(oidc: Option[sbirds.auth.OidcClient],
                        users: UserStore,
                        rooms: RoomStore,
                        meetings: MeetingStore,
                        links: LinkStore,
                        hub: Hub,
                        skuaka: Option[SkuakaClient],
                        box: skaka.auth.SecretBox,
                        sessions: SessionAuth,
                        cfg: ApiConfig,
                        recordings: RecordingStore,
                        skawauLinks: SkawauStore,
                        oauth: OAuthStore,
                        recorder: Option[Recorder] = None,
                        stt: Option[SpeechToText] = None,
                        minutes: Option[MinutesService] = None)

  /** REST API と MCP（WebSocket は SignalRoutes）. */
  def apply(d: Deps): HttpRoutes[IO] = {
    val rooms = new RoomRoutes(d)
    val rest = new AuthRoutes(d).routes <+> rooms.routes <+> new SkuakaRoutes(d, rooms).routes <+>
      new SkawauRoutes(d).routes <+>
      new OAuthRoutes(d.oauth, d.cfg.oauthClients, d.sessions, d.cfg).routes <+>
      d.minutes.fold(HttpRoutes.empty[IO])(new MinutesRoutes(_, d.sessions).routes) <+>
      d.stt.fold(HttpRoutes.empty[IO])(new SpeechRoutes(d.hub, _).routes)
    rest <+> new McpRoutes(rest, d.sessions, d.cfg.publicUrl, d.cfg.version).routes
  }
}
