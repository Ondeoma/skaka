package skaka.api

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.headers.{Authorization, Cookie, Location}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import skaka.auth.{OidcClient, OidcConfig, SecretBox}
import skaka.db.*
import skaka.devidp.DevIdp
import skaka.shared.*

import java.util.UUID
import scala.concurrent.duration.*

/**
 * OIDC のログインから会議室・会議・MCP・skuaka との連携までを、開発用の IdP（DevIdp）・偽の skuaka・本物の DB（skaka_test）で通しで確かめる.
 * DB は docker-compose.dev.yml（:54350）. TEST_DATABASE_URL で変えられる.
 */
class FlowSpec extends munit.CatsEffectSuite:

  private val dbCfg  = DbConfig(sys.env.getOrElse("TEST_DATABASE_URL", "jdbc:postgresql://localhost:54350/skaka_test"), "skaka", "skaka", 4)
  private val issuer = "http://localhost:9999"
  private val apiCfg = ApiConfig(cookieSecure = false, publicUrl = "http://localhost:8096",
    ice = IceConfig(turnUrls = List("turn:turn.example.com:3478"), turnSecret = Some("turn-secret")),
    oauthClients = List(OAuthClient("skuaka", "skuaka-test-secret-0123456789", "http://skuaka.test/api/integrations/skaka/callback", "skuaka",
      List("read", "write"))))

  final case class App(idp: HttpApp[IO], app: HttpApp[IO], hub: Hub, skuaka: FakeSkuaka, skawau: FakeSkawau, recorder: Recorder)

  /** IdP・偽の skuaka・アプリ（ルート）を用意する. DB はまっさらにする. */
  private val fixture = ResourceSuiteLocalFixture("app", for
    _  <- Resource.eval(IO.blocking {
            val c = java.sql.DriverManager.getConnection(dbCfg.url, dbCfg.user, dbCfg.password)
            try c.createStatement().execute("drop schema public cascade; create schema public") finally c.close()
          })
    _    <- Resource.eval(Database.migrate(dbCfg))
    xa   <- Database.transactor(dbCfg)
    idp  <- Resource.eval(DevIdp.create(issuer, "skaka", "skaka-test-secret-0123456789"))
    oidc <- Resource.eval(OidcClient.create(OidcConfig(issuer, "skaka", "skaka-test-secret-0123456789", "http://localhost:8096/auth/callback"),
              Client.fromHttpApp(idp.routes.orNotFound)))
    users    = new UserStore(xa)
    rooms    = new RoomStore(xa, users)
    meetings = new MeetingStore(xa)
    hub  <- Hub.resource(rooms, meetings)
    fake <- Resource.eval(FakeSkuaka.create)
    fsw  <- Resource.eval(FakeSkawau.create)
    dir  <- Resource.make(IO.blocking(java.nio.file.Files.createTempDirectory("skaka-rec")))(d =>
              IO.blocking { d.toFile.listFiles().foreach(_.delete()); d.toFile.delete() }.void)
    box   = new SecretBox("test-key")
    recs  = new RecordingStore(xa)
    links = new SkawauStore(xa)
    swc   = Client.fromHttpApp(fsw.routes.orNotFound)
    rec  <- Recorder.resource(recs, links, new SkawauClient(SkawauConfig("http://skawau.test", "http://skawau.test", "skaka", "skaka-secret-0123456789",
              dir.toString, 1024 * 1024), swc, swc), box, hub)
  yield
    val sk  = new SkuakaClient("http://skuaka.test", Client.fromHttpApp(fake.routes.orNotFound))
    val app = ApiRoutes(ApiRoutes.Deps(Some(oidc), users, rooms, meetings, new LinkStore(xa), hub, Some(sk), box,
      new SessionAuth(users, apiCfg), apiCfg, recs, links, new OAuthStore(xa), Some(rec))).orNotFound
    App(idp.routes.orNotFound, app, hub, fake, fsw, rec))

  override def munitFixtures = List(fixture)

  private def location(r: Response[IO]): Uri = r.headers.get[Location].map(_.uri).getOrElse(fail(s"no Location (${r.status})"))

  /** IdP でログインして、セッションの Cookie を受け取る. */
  private def login(name: String, email: String): IO[String] =
    val f = fixture()
    for
      r1 <- f.app.run(Request[IO](Method.GET, uri"/auth/login?returnTo=/r/abc"))
      _   = assertEquals(r1.status, Status.Found)
      auth = location(r1)
      _   = assertEquals(auth.params.get("code_challenge_method"), Some("S256"))
      form = UrlForm(List("client_id", "redirect_uri", "state", "nonce", "code_challenge").map(k => k -> auth.params(k)) ++
               List("name" -> name, "email" -> email)*)
      r2 <- f.idp.run(Request[IO](Method.POST, uri"/authorize").withEntity(form))
      cb  = location(r2)
      r3 <- f.app.run(Request[IO](Method.GET, Uri(path = cb.path, query = cb.query)))
      _   = assertEquals(r3.status, Status.Found, "callback")
      _   = assertEquals(location(r3).renderString, "/r/abc")
    yield r3.cookies.find(_.name == "skaka_session").map(_.content).getOrElse(fail("no session cookie"))

  /** token は Cookie のセッション（Bearer で始まれば API トークン）. None ならログインしていない（画面からの要求）. */
  private def call(token: Option[String], m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] =
    val base0 = Request[IO](m, Uri.unsafeFromString(path))
    val base = token match
      case Some(t) if t.startsWith("skaka_") => base0.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, t)))
      case Some(t) => base0.putHeaders(Cookie(RequestCookie("skaka_session", t)), Header.Raw(CIString("X-Skaka"), "1"))
      case None    => base0.putHeaders(Header.Raw(CIString("X-Skaka"), "1"))
    fixture().app.run(body.fold(base)(base.withEntity(_)))

  private def call(token: String, m: Method, path: String): IO[Response[IO]] = call(Some(token), m, path)
  private def call(token: String, m: Method, path: String, body: Json): IO[Response[IO]] = call(Some(token), m, path, Some(body))

  private def json[A: io.circe.Decoder](r: Response[IO]): IO[A] =
    if r.status.isSuccess then r.as[A] else r.bodyText.compile.string.flatMap(t => IO(fail(s"${r.status}: $t")))

  /** 切符で接続し、届いたメッセージを読むための小道具. */
  private final class Conn(val id: String, q: Queue[IO, Option[ServerMsg]]):
    /** 条件に合うメッセージが届くまで読む（それまでのものは捨てる）. */
    def await[A](pf: PartialFunction[ServerMsg, A]): IO[A] =
      q.take.flatMap {
        case Some(m) if pf.isDefinedAt(m) => IO.pure(pf(m))
        case Some(_)                      => await(pf)
        case None                         => IO.raiseError(new AssertionError("切断された"))
      }.timeout(3.seconds)
    def closed: IO[String] = await { case ServerMsg.Closed(r) => r }

  private def connect(ticket: String): IO[Conn] =
    fixture().hub.connect(ticket).map(_.getOrElse(fail("切符が使えない"))).map((id, q) => Conn(id, q))

  private def join(token: Option[String], code: String, guest: Option[String] = None): IO[JoinDto] =
    call(token, Method.POST, s"/api/rooms/$code/join", Some(JoinReq(guest).asJson)).flatMap(json[JoinDto])

  test("OIDC でログインすると自分の会議室ができる. ログアウトで IdP からもログアウトする") {
    for
      t  <- login("山田 太郎", "yamada@example.com")
      rs <- call(t, Method.GET, "/api/rooms").flatMap(json[List[RoomDto]])
      _   = assertEquals(rs.map(r => (r.name, r.personal, r.isOwner)), List(("山田 太郎 の会議室", true, true)))
      _   = assert(RoomCode.valid(rs.head.code), rs.head.code)
      out <- call(t, Method.POST, "/auth/logout").flatMap(json[Json])
      url  = out.hcursor.get[String]("redirect").getOrElse("")
      _    = assert(url.startsWith(s"$issuer/logout?") && url.contains("id_token_hint="), url)
      gone <- call(t, Method.GET, "/api/me")
      _    = assertEquals(gone.status, Status.Unauthorized)
    yield ()
  }

  test("会議室: 作成・変更は持ち主だけ・自分の会議室は削除できない・X-Skaka のない更新は断る") {
    val f = fixture()
    for
      a  <- login("鈴木", "suzuki@example.com")
      b  <- login("田中", "tanaka@example.com")
      bad <- call(a, Method.POST, "/api/rooms", RoomReq("  ").asJson)
      _   = assertEquals(bad.status, Status.BadRequest)
      r  <- call(a, Method.POST, "/api/rooms", Json.obj("name" -> "定例".asJson)).flatMap(json[RoomDto])
      _   = assertEquals((r.guestAccess, r.lobby, r.url), (false, false, s"http://localhost:8096/r/${r.code}"))
      // コードは URL・大文字・ハイフンなしでも
      got <- call(b, Method.GET, s"/api/rooms/${r.code.toUpperCase.replace("-", "")}").flatMap(json[RoomDto])
      _   = assertEquals((got.id, got.isOwner), (r.id, false))
      nb <- call(b, Method.PUT, s"/api/rooms/${r.code}", RoomReq("乗っ取り").asJson)
      _   = assertEquals(nb.status, Status.Forbidden)
      up <- call(a, Method.PUT, s"/api/rooms/${r.code}", RoomReq("定例（変更）", "毎週", guestAccess = true, lobby = true).asJson).flatMap(json[RoomDto])
      _   = assertEquals((up.name, up.guestAccess, up.lobby), ("定例（変更）", true, true))
      csrf <- f.app.run(Request[IO](Method.DELETE, Uri.unsafeFromString(s"/api/rooms/${r.code}")).putHeaders(Cookie(RequestCookie("skaka_session", a))))
      _   = assertEquals(csrf.status, Status.Forbidden)
      personal <- call(a, Method.GET, "/api/rooms").flatMap(json[List[RoomDto]]).map(_.find(_.personal).get)
      np <- call(a, Method.DELETE, s"/api/rooms/${personal.code}")
      _   = assertEquals(np.status, Status.BadRequest)
      del <- call(a, Method.DELETE, s"/api/rooms/${r.code}")
      _   = assertEquals(del.status, Status.NoContent)
      nf <- call(a, Method.GET, s"/api/rooms/${r.code}")
      _   = assertEquals(nf.status, Status.NotFound)
    yield ()
  }

  test("会議: 入る・シグナリングの中継・チャット・ゲストの待機室・ロック・履歴") {
    for
      a  <- login("主催 花子", "host@example.com")
      b  <- login("参加 次郎", "member@example.com")
      r  <- call(a, Method.POST, "/api/rooms", RoomReq("週次", guestAccess = true).asJson).flatMap(json[RoomDto])
      // 入る前の画面はゲストにも見える（ゲスト可の会議室だけ）
      en <- call(None, Method.GET, s"/api/rooms/${r.code}/entry").flatMap(json[RoomEntryDto])
      _   = assertEquals((en.name, en.me, en.guestAccess), ("週次", None, true))
      // ログインしていない人には、持ち主のメールアドレスを見せない
      _   = assertEquals((en.owner.name, en.owner.email), ("主催 花子", None))
      // 切符には ICE サーバー（STUN と、一時的な TURN の認証情報）
      ja <- join(Some(a), r.code)
      _   = assertEquals(ja.iceServers.size, 2)
      _   = assert(ja.iceServers(1).username.exists(_.contains(":")) && ja.iceServers(1).credential.nonEmpty)
      ca <- connect(ja.ticket)
      me <- ca.await { case ServerMsg.Welcome(self, peers, _, _, _, _) => (self, peers) }
      _   = assertEquals((me._1.host, me._2), (true, Nil))
      // 切符は1回だけ
      again <- fixture().hub.connect(ja.ticket)
      _   = assertEquals(again, None)
      cb <- join(Some(b), r.code).flatMap(j => connect(j.ticket))
      wb <- cb.await { case ServerMsg.Welcome(self, peers, _, _, _, _) => (self, peers) }
      _   = assertEquals((wb._1.host, wb._2.map(_.name)), (false, List("主催 花子")))
      jn <- ca.await { case ServerMsg.Joined(p) => p }
      _   = assertEquals(jn.name, "参加 次郎")
      // SDP の中継
      _  <- fixture().hub.receive(cb.id, ClientMsg.Signal(ca.id, Json.obj("description" -> Json.obj("type" -> "offer".asJson))))
      sg <- ca.await { case ServerMsg.Signal(from, d) => (from, d) }
      _   = assertEquals(sg._1, cb.id)
      // チャット（全員宛は履歴に残る）・参加者でない人は主催者の操作ができない
      _  <- fixture().hub.receive(cb.id, ClientMsg.Chat("  こんにちは  "))
      ch <- ca.await { case ServerMsg.Chat(m) => m }
      _   = assertEquals((ch.text, ch.name), ("こんにちは", "参加 次郎"))
      _  <- fixture().hub.receive(cb.id, ClientMsg.Lock(true))
      er <- cb.await { case ServerMsg.Error(m) => m }
      _   = assert(er.contains("主催者"))
      // ゲスト: ログインしていなければ名前が要る. 待機室で待ち、主催者が許可する
      noName <- call(None, Method.POST, s"/api/rooms/${r.code}/join", Some(JoinReq(None).asJson))
      _   = assertEquals(noName.status, Status.BadRequest)
      cg <- join(None, r.code, Some("ゲスト")).flatMap(j => connect(j.ticket))
      _  <- cg.await { case ServerMsg.Waiting(hp) => assert(hp) }
      lb <- ca.await { case ServerMsg.Lobby(ws) => ws }
      _   = assertEquals(lb.map(p => (p.name, p.guest)), List(("ゲスト", true)))
      _  <- fixture().hub.receive(ca.id, ClientMsg.Admit(cg.id))
      wg <- cg.await { case ServerMsg.Welcome(_, peers, chat, _, _, _) => (peers, chat) }
      _   = assertEquals((wg._1.size, wg._2.map(_.text)), (2, List("こんにちは")))
      // ロック中は新しく入れない
      _  <- fixture().hub.receive(ca.id, ClientMsg.Lock(true))
      cl <- join(Some(b), r.code).flatMap(j => connect(j.ticket))
      rs <- cl.closed
      _   = assert(rs.contains("ロック"), rs)
      live <- call(b, Method.GET, s"/api/rooms/${r.code}/live").flatMap(json[LiveDto])
      _   = assertEquals((live.peers.size, live.locked), (3, true))
      // 主催者が全員の会議を終える → 履歴
      _  <- fixture().hub.receive(ca.id, ClientMsg.EndAll())
      _  <- cb.closed
      _  <- cg.closed
      _  <- IO.sleep(300.millis) // 履歴は裏で書く
      ms <- call(b, Method.GET, "/api/meetings").flatMap(json[List[MeetingDto]])
      _   = assertEquals(ms.map(m => (m.roomName, m.participants, m.messages, m.endedAt.isDefined)), List(("週次", 3, 1, true)))
      d  <- call(a, Method.GET, s"/api/meetings/${ms.head.id}").flatMap(json[MeetingDetailDto])
      _   = assertEquals(d.participants.map(_.name).sorted, List("ゲスト", "主催 花子", "参加 次郎"))
      // 参加していない人には見えない
      c  <- login("無関係", "other@example.com")
      no <- call(c, Method.GET, s"/api/meetings/${ms.head.id}")
      _   = assertEquals(no.status, Status.NotFound)
    yield ()
  }

  test("ゲストを受け付けない会議室は、ログインしていない人には見せない. API トークンでは入れない") {
    for
      a  <- login("内緒", "secret@example.com")
      r  <- call(a, Method.POST, "/api/rooms", RoomReq("非公開").asJson).flatMap(json[RoomDto])
      e  <- call(None, Method.GET, s"/api/rooms/${r.code}/entry")
      _   = assertEquals(e.status, Status.Unauthorized)
      j  <- call(None, Method.POST, s"/api/rooms/${r.code}/join", Some(JoinReq(Some("誰か")).asJson))
      _   = assertEquals(j.status, Status.Unauthorized)
      tk <- call(a, Method.POST, "/api/tokens", ApiTokenReq("t", TokenScope.Write, Some(1)).asJson).flatMap(json[ApiTokenCreated])
      jt <- call(Some(tk.token), Method.POST, s"/api/rooms/${r.code}/join", Some(JoinReq(None).asJson))
      _   = assertEquals(jt.status, Status.Forbidden)
    yield ()
  }

  private def mcp(token: String, method: String, params: Json = Json.obj()): IO[Json] =
    call(Some(token), Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> method.asJson, "params" -> params)))
      .flatMap(json[Json]).map(_.hcursor.downField("result").focus.getOrElse(fail("no result")))

  private def tool(token: String, name: String, args: (String, Json)*): IO[(Boolean, String)] =
    mcp(token, "tools/call", Json.obj("name" -> name.asJson, "arguments" -> Json.obj(args*))).map { r =>
      (r.hcursor.get[Boolean]("isError").getOrElse(true), r.hcursor.downField("content").downArray.get[String]("text").getOrElse(""))
    }

  test("MCP: 参照のみのトークンには変更するツールを出さない. 会議室の作成・一覧") {
    for
      a  <- login("AI 使い", "ai@example.com")
      rw <- call(a, Method.POST, "/api/tokens", ApiTokenReq("rw", TokenScope.Write, None).asJson).flatMap(json[ApiTokenCreated])
      ro <- call(a, Method.POST, "/api/tokens", ApiTokenReq("ro", TokenScope.Read, None).asJson).flatMap(json[ApiTokenCreated])
      init <- mcp(rw.token, "initialize", Json.obj("protocolVersion" -> "2025-03-26".asJson))
      _   = assertEquals(init.hcursor.get[String]("protocolVersion"), Right("2025-03-26"))
      all <- mcp(rw.token, "tools/list").map(_.hcursor.downField("tools").as[List[Json]].toOption.get.flatMap(_.hcursor.get[String]("name").toOption))
      rdo <- mcp(ro.token, "tools/list").map(_.hcursor.downField("tools").as[List[Json]].toOption.get.flatMap(_.hcursor.get[String]("name").toOption))
      _   = assert(all.contains("create_room") && all.contains("schedule_meeting"))
      _   = assert(!rdo.contains("create_room") && rdo.contains("list_rooms"), rdo.toString)
      no <- tool(ro.token, "create_room", "name" -> "x".asJson)
      _   = assert(no._1 && no._2.contains("参照のみ"), no.toString)
      cr <- tool(rw.token, "create_room", "name" -> "AI の会議室".asJson, "guestAccess" -> true.asJson)
      _   = assert(!cr._1 && cr._2.contains("AI の会議室") && cr._2.contains("ゲスト可"), cr.toString)
      ls <- tool(ro.token, "list_rooms")
      _   = assert(ls._2.contains("AI の会議室") && ls._2.contains("自分の会議室"), ls.toString)
      // 参照のみのトークンでは REST API の更新もできない
      up <- call(Some(ro.token), Method.POST, "/api/rooms", Some(RoomReq("x").asJson))
      _   = assertEquals(up.status, Status.Forbidden)
    yield ()
  }

  test("skuaka との連携: トークンの登録・会議の予定を立てる・予定に会議室を付ける・今後の予定") {
    val f = fixture()
    for
      a   <- login("予定 太郎", "plan@example.com")
      bad <- call(a, Method.PUT, "/api/skuaka", SkuakaLinkReq("skuaka_wrong").asJson)
      _    = assertEquals(bad.status, Status.BadRequest)
      ln  <- call(a, Method.PUT, "/api/skuaka", SkuakaLinkReq(FakeSkuaka.Token).asJson).flatMap(json[SkuakaLinkDto])
      _    = assertEquals((ln.linked, ln.account, ln.canWrite), (true, Some("予定 太郎"), true))
      // 予定を立てる: skuaka に予定（場所に会議室の URL）. 招待はメールで skuaka の人を探す
      sc  <- call(a, Method.POST, "/api/skuaka/schedule", ScheduleReq("設計レビュー", "2026-11-20T01:00:00Z", "2026-11-20T02:00:00Z",
               attendees = List("guest@example.com", "nobody@example.com")).asJson).flatMap(json[ScheduleDto])
      _    = assertEquals((sc.invited, sc.notFound), (List("guest@example.com"), List("nobody@example.com")))
      ev  <- f.skuaka.events.get.map(_(sc.eventId))
      _    = assertEquals(ev.hcursor.get[String]("location"), Right(sc.room.url))
      _    = assertEquals(ev.hcursor.get[List[UUID]]("attendees"), Right(List(FakeSkuaka.Guest)))
      _    = assertEquals(sc.room.event.map(_.title), Some("設計レビュー"))
      // ほかの予定に会議室を付ける（場所の後ろに URL を足す. 2回目は同じ会議室）
      other <- f.skuaka.add("朝会", "会議室A")
      r1  <- call(a, Method.POST, s"/api/skuaka/events/$other/room").flatMap(json[RoomDto])
      loc <- f.skuaka.events.get.map(_(other).hcursor.get[String]("location").getOrElse(""))
      _    = assertEquals(loc, s"会議室A / ${r1.url}")
      r2  <- call(a, Method.POST, s"/api/skuaka/events/$other/room").flatMap(json[RoomDto])
      _    = assertEquals(r2.code, r1.code)
      up  <- call(a, Method.GET, "/api/skuaka/events?from=2026-11-19&to=2026-11-26").flatMap(json[List[UpcomingDto]])
      _    = assertEquals(up.map(u => (u.title, u.room)).toSet, Set(("設計レビュー", Some(sc.room.code)), ("朝会", Some(r1.code))))
      // 連携をやめると使えない
      _   <- call(a, Method.DELETE, "/api/skuaka")
      gone <- call(a, Method.GET, "/api/skuaka/events")
      _    = assertEquals(gone.status, Status.Conflict)
    yield ()
  }

  /** 録画の断片（WebM の先頭: EBML・Segment（大きさ不明）・Info と、Cluster の始まり）. */
  private val webmHead: Array[Byte] = Array(0x1a, 0x45, 0xdf, 0xa3, 0x80, 0x18, 0x53, 0x80, 0x67, 0x01, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0x15, 0x49, 0xa9, 0x66, 0x87, 0x2a, 0xd7, 0xb1, 0x83, 0x0f, 0x42, 0x40, 0x1f, 0x43, 0xb6, 0x75, 0x01, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff)
    .map(_.toByte)

  private def chunk(t: String, id: UUID, seq: Int, b: Array[Byte]): IO[Response[IO]] =
    fixture().app.run(Request[IO](Method.PUT, Uri.unsafeFromString(s"/api/recordings/$id/chunks/$seq")).withEntity(b)(using EntityEncoder.byteArrayEncoder)
      .putHeaders(Cookie(RequestCookie("skaka_session", t)), Header.Raw(CIString("X-Skaka"), "1")))

  /** 保存し終わるまで待つ. */
  private def saved(t: String, id: UUID, n: Int = 30): IO[RecordingDto] =
    call(t, Method.GET, s"/api/recordings/$id").flatMap(json[RecordingDto]).flatMap { r =>
      if r.status == RecordingStatus.Saving && n > 0 then IO.sleep(100.millis) *> saved(t, id, n - 1) else IO.pure(r)
    }

  test("skawau との連携と録画: OAuth で許可・主催者だけが録画・全員に録画中を知らせる・断片を順に受け取り・skawau に置いて参加者と共有") {
    val f = fixture()
    for
      a   <- login("録画 主催", "host2@example.com")
      b   <- login("録画 参加", "member2@example.com")
      st  <- call(a, Method.GET, "/api/skawau").flatMap(json[SkawauLinkDto])
      _    = assertEquals((st.available, st.linked), (true, false))
      // 連携: skawau の許可の画面へ（read write・PKCE）→ 戻ってきたらトークンを保存
      cn  <- call(a, Method.GET, "/api/integrations/skawau/connect?returnTo=/settings")
      au   = location(cn)
      _    = assertEquals((au.path.renderString, au.params.get("scope"), au.params.get("code_challenge_method")), ("/oauth/authorize", Some("read write"), Some("S256")))
      // ほかの人の状態では使えない
      st2 <- call(b, Method.GET, s"/api/integrations/skawau/callback?state=${au.params("state")}&code=${FakeSkawau.Code}")
      _    = assert(location(st2).renderString.contains("skawau=error"))
      cn2 <- call(a, Method.GET, "/api/integrations/skawau/connect?returnTo=/settings").map(location)
      cb  <- call(a, Method.GET, s"/api/integrations/skawau/callback?state=${cn2.params("state")}&code=${FakeSkawau.Code}")
      _    = assertEquals(location(cb).renderString, "/settings?skawau=connected")
      ok  <- call(a, Method.GET, "/api/skawau").flatMap(json[SkawauLinkDto])
      _    = assert(ok.linked)
      // 会議: 主催者と参加者
      r   <- call(a, Method.POST, "/api/rooms", RoomReq("録画する会議").asJson).flatMap(json[RoomDto])
      ca  <- join(Some(a), r.code).flatMap(j => connect(j.ticket))
      _   <- ca.await { case w: ServerMsg.Welcome => () }
      cb2 <- join(Some(b), r.code).flatMap(j => connect(j.ticket))
      _   <- cb2.await { case w: ServerMsg.Welcome => () }
      nb  <- call(b, Method.POST, "/api/recordings", RecordingStartReq(r.code, "video/webm").asJson)
      _    = assertEquals(nb.status, Status.Conflict) // b は skawau と連携していない
      // 動画以外の形式は断る（skawau に置くときの Content-Type になるため）
      html <- call(a, Method.POST, "/api/recordings", RecordingStartReq(r.code, "text/html").asJson)
      _    = assertEquals(html.status, Status.BadRequest)
      rec <- call(a, Method.POST, "/api/recordings", RecordingStartReq(r.code, "video/webm;codecs=vp8,opus").asJson).flatMap(json[RecordingDto])
      _    = assertEquals((rec.status, rec.mine, rec.title.startsWith("録画する会議 ")), (RecordingStatus.Recording, true, true))
      on  <- cb2.await { case ServerMsg.Recording(n) => n }
      _    = assertEquals(on.map(x => (x.id, x.by)), Some((rec.id, "録画 主催")))
      two <- call(a, Method.POST, "/api/recordings", RecordingStartReq(r.code, "video/webm").asJson)
      _    = assertEquals(two.status, Status.Conflict)
      // 断片: 順に. 同じものの送り直しは受け流し、飛ばしたものは断る. 録画した人だけ
      c0  <- chunk(a, rec.id, 0, webmHead)
      _    = assertEquals(c0.status, Status.Ok)
      c0b <- chunk(a, rec.id, 0, webmHead)
      _    = assertEquals(c0b.status, Status.Ok)
      c2  <- chunk(a, rec.id, 2, Array[Byte](1))
      _    = assertEquals(c2.status, Status.Conflict)
      cbx <- chunk(b, rec.id, 1, Array[Byte](1))
      _    = assertEquals(cbx.status, Status.Forbidden)
      c1  <- chunk(a, rec.id, 1, Array.fill[Byte](100)(7))
      _    = assertEquals(c1.status, Status.Ok)
      big <- chunk(a, rec.id, 2, Array.fill[Byte](1024 * 1024)(0))
      _    = assertEquals(big.status, Status.PayloadTooLarge)
      fin <- call(a, Method.POST, s"/api/recordings/${rec.id}/finish", RecordingFinishReq(5000).asJson)
      _    = assertEquals(fin.status, Status.Accepted)
      off <- cb2.await { case ServerMsg.Recording(n) => n }
      _    = assertEquals(off, None)
      sv  <- saved(a, rec.id)
      _    = assertEquals((sv.status, sv.error, sv.shared, sv.fileUrl), (RecordingStatus.Saved, None, 1, Some(s"http://skawau.test/view/${FakeSkawau.File}")))
      up  <- f.skawau.uploads.get.map(_.last)
      _    = assertEquals(up._1.get("path").exists(_.startsWith("skaka 録画/")), true)
      _    = assert(up._1("name").startsWith("録画する会議 ") && up._1("name").endsWith(".webm"), up._1.toString)
      // WebM の Info に長さ（Duration）が足されている（11 バイト増える）
      _    = assertEquals(up._2.length, webmHead.length + 100 + 11)
      _    = assertEquals(java.nio.ByteBuffer.wrap(up._2, 29 + 3, 8).getDouble, 5000.0)
      sh  <- f.skawau.shares.get
      _    = assertEquals(sh, List(FakeSkawau.File -> FakeSkawau.Member))
      // 会議の履歴にも出る（参加者に見える. 関係ない人には見えない）
      _   <- fixture().hub.receive(ca.id, ClientMsg.EndAll())
      _   <- IO.sleep(300.millis)
      ms  <- call(b, Method.GET, s"/api/recordings?room=${r.code}").flatMap(json[List[RecordingDto]])
      _    = assertEquals(ms.map(x => (x.id, x.mine)), List(rec.id -> false))
      md  <- call(b, Method.GET, s"/api/meetings/${sv.meetingId.get}").flatMap(json[MeetingDetailDto])
      _    = assertEquals(md.recordings.map(_.id), List(rec.id))
      c   <- login("部外者", "outsider@example.com")
      nf  <- call(c, Method.GET, s"/api/recordings/${rec.id}")
      _    = assertEquals(nf.status, Status.NotFound)
      // MCP からも見える
      tk  <- call(b, Method.POST, "/api/tokens", ApiTokenReq("ro", TokenScope.Read, None).asJson).flatMap(json[ApiTokenCreated])
      lr  <- tool(tk.token, "list_recordings", "room" -> r.code.asJson)
      _    = assert(!lr._1 && lr._2.contains("保存済み") && lr._2.contains(s"/view/${FakeSkawau.File}"), lr.toString)
      // 消せるのは録画した人（と会議室の持ち主）
      db  <- call(b, Method.DELETE, s"/api/recordings/${rec.id}")
      _    = assertEquals(db.status, Status.Forbidden)
      da  <- call(a, Method.DELETE, s"/api/recordings/${rec.id}")
      _    = assertEquals(da.status, Status.NoContent)
    yield ()
  }

  test("録画: 画面を閉じて断片が届かなくなったら、それまでの分を保存する. 録画中を知らせる表示も消える") {
    val f = fixture()
    for
      a   <- login("閉じる人", "closer@example.com")
      cn  <- call(a, Method.GET, "/api/integrations/skawau/connect").map(location)
      _   <- call(a, Method.GET, s"/api/integrations/skawau/callback?state=${cn.params("state")}&code=${FakeSkawau.Code}")
      r   <- call(a, Method.POST, "/api/rooms", RoomReq("途中で閉じる").asJson).flatMap(json[RoomDto])
      ca  <- join(Some(a), r.code).flatMap(j => connect(j.ticket))
      _   <- ca.await { case w: ServerMsg.Welcome => () }
      rec <- call(a, Method.POST, "/api/recordings", RecordingStartReq(r.code, "video/webm").asJson).flatMap(json[RecordingDto])
      _   <- chunk(a, rec.id, 0, webmHead)
      _   <- ca.await { case ServerMsg.Recording(Some(_)) => () }
      _   <- f.recorder.sweep(0.millis)
      off <- ca.await { case ServerMsg.Recording(n) => n }
      _    = assertEquals(off, None)
      sv  <- saved(a, rec.id)
      _    = assertEquals((sv.status, sv.bytes, sv.endedAt.isDefined), (RecordingStatus.Saved, webmHead.length.toLong, true))
      // 終えた録画には断片を足せない
      late <- chunk(a, rec.id, 1, Array[Byte](1))
      _    = assertEquals(late.status, Status.Conflict)
    yield ()
  }

  test("skuaka への認可（OAuth）: 許可の画面・コードをトークンに・トークンで予定付きの会議室を作る・予定の写しを変える・失効") {
    val f        = fixture()
    val redirect = "http://skuaka.test/api/integrations/skaka/callback"
    val verifier = skaka.auth.Crypto.randomToken(48)
    val basic    = Authorization(BasicCredentials("skuaka", "skuaka-test-secret-0123456789"))
    def authorize(t: String, extra: String = "") =
      call(t, Method.GET, s"/oauth/authorize?client_id=skuaka&redirect_uri=${java.net.URLEncoder.encode(redirect, "UTF-8")}&response_type=code" +
        s"&scope=read+write&state=st1&code_challenge=${skaka.auth.Crypto.pkceChallenge(verifier)}&code_challenge_method=S256$extra")
    def token(code: String, auth: Authorization = basic) =
      f.app.run(Request[IO](Method.POST, uri"/oauth/token").withEntity(UrlForm("grant_type" -> "authorization_code", "code" -> code,
        "redirect_uri" -> redirect, "code_verifier" -> verifier)).putHeaders(auth))
    for
      a    <- login("予定から会議", "fromcal@example.com")
      // 登録していない戻り先には戻さない
      bad  <- call(a, Method.GET, "/oauth/authorize?client_id=skuaka&redirect_uri=https://evil.example/cb&response_type=code")
      _     = assertEquals(bad.status, Status.BadRequest)
      // ログインしていなければログインへ
      anon <- call(None, Method.GET, "/oauth/authorize?client_id=skuaka&redirect_uri=" + java.net.URLEncoder.encode(redirect, "UTF-8") + "&response_type=code&code_challenge=" +
                skaka.auth.Crypto.pkceChallenge(verifier) + "&code_challenge_method=S256")
      _     = assert(location(anon).renderString.startsWith("/auth/login?returnTo="))
      pg   <- authorize(a)
      html <- pg.bodyText.compile.string
      _     = assert(html.contains("skuaka との連携") && html.contains("会議室を作成"), html.take(300))
      csrf  = pg.cookies.find(_.name == "skaka_oauth_csrf").map(_.content).getOrElse(fail("no csrf cookie"))
      form  = UrlForm(List("client_id" -> "skuaka", "redirect_uri" -> redirect, "response_type" -> "code", "scope" -> "read write", "state" -> "st1",
                "code_challenge" -> skaka.auth.Crypto.pkceChallenge(verifier), "code_challenge_method" -> "S256", "csrf" -> csrf, "decision" -> "allow")*)
      // CSRF の値がなければ断る
      nocs <- f.app.run(Request[IO](Method.POST, uri"/oauth/authorize").withEntity(form).putHeaders(Cookie(RequestCookie("skaka_session", a))))
      _     = assertEquals(nocs.status, Status.BadRequest)
      ok   <- f.app.run(Request[IO](Method.POST, uri"/oauth/authorize").withEntity(form)
                .putHeaders(Cookie(RequestCookie("skaka_session", a), RequestCookie("skaka_oauth_csrf", csrf))))
      cb    = location(ok)
      _     = assertEquals((cb.path.renderString, cb.params.get("state")), ("/api/integrations/skaka/callback", Some("st1")))
      code  = cb.params("code")
      wrong <- token(code, Authorization(BasicCredentials("skuaka", "wrong-secret-0123456789")))
      _     = assertEquals(wrong.status, Status.Unauthorized)
      tk   <- token(code).flatMap(json[Json])
      at    = tk.hcursor.get[String]("access_token").getOrElse("")
      _     = assertEquals((at.startsWith("skaka_"), tk.hcursor.get[String]("scope")), (true, Right("write")))
      // トークンで、予定の写し付きの会議室を作る（skuaka の予定の場所に URL を入れる）
      ev    = RoomEventDto(UUID.randomUUID(), "設計レビュー", "2026-11-20T01:00:00Z", "2026-11-20T02:00:00Z", allDay = false)
      room <- call(Some(at), Method.POST, "/api/rooms", Some(RoomReq("設計レビュー", event = Some(ev)).asJson)).flatMap(json[RoomDto])
      _     = assertEquals((room.event, room.isOwner, room.url), (Some(ev), true, s"http://localhost:8096/r/${room.code}"))
      moved = ev.copy(title = "設計レビュー（延期）", start = "2026-11-21T01:00:00Z", end = "2026-11-21T02:00:00Z")
      up   <- call(Some(at), Method.PUT, s"/api/rooms/${room.code}/event", Some(moved.asJson)).flatMap(json[RoomDto])
      _     = assertEquals(up.event, Some(moved))
      // 名前が予定のタイトルのままなら、名前も合わせる. 手で名前を変えた会議室は変えない
      _     = assertEquals(up.name, "設計レビュー（延期）")
      _    <- call(Some(at), Method.PUT, s"/api/rooms/${room.code}", Some(RoomReq("レビュー会").asJson)).flatMap(json[RoomDto])
      kept <- call(Some(at), Method.PUT, s"/api/rooms/${room.code}/event", Some(moved.copy(title = "設計レビュー（再延期）").asJson)).flatMap(json[RoomDto])
      _     = assertEquals((kept.name, kept.event.map(_.title)), ("レビュー会", Some("設計レビュー（再延期）")))
      cl   <- call(Some(at), Method.PUT, s"/api/rooms/${room.code}/event", Some(Json.Null)).flatMap(json[RoomDto])
      _     = assertEquals(cl.event, None)
      // 設定の API トークンの一覧に「skuaka 連携」として出る
      ts   <- call(a, Method.GET, "/api/tokens").flatMap(json[List[ApiTokenDto]])
      _     = assertEquals(ts.map(t => (t.name, t.scope)), List(("skuaka 連携", "write")))
      // 失効させると使えない
      _    <- f.app.run(Request[IO](Method.POST, uri"/oauth/revoke").withEntity(UrlForm("token" -> at)).putHeaders(basic))
      gone <- call(Some(at), Method.GET, "/api/rooms")
      _     = assertEquals(gone.status, Status.Unauthorized)
      // コードは1回だけ. もう一度使われたら（漏れた）、そのコードで発行したトークンも失効させる
      ok2  <- f.app.run(Request[IO](Method.POST, uri"/oauth/authorize").withEntity(form)
                .putHeaders(Cookie(RequestCookie("skaka_session", a), RequestCookie("skaka_oauth_csrf", csrf))))
      code2 = location(ok2).params("code")
      at2  <- token(code2).flatMap(json[Json]).map(_.hcursor.get[String]("access_token").getOrElse(""))
      live <- call(Some(at2), Method.GET, "/api/rooms")
      _     = assertEquals(live.status, Status.Ok)
      again <- token(code2)
      _     = assertEquals(again.status, Status.BadRequest)
      dead <- call(Some(at2), Method.GET, "/api/rooms")
      _     = assertEquals(dead.status, Status.Unauthorized)
    yield ()
  }

  test("守り: 本文の大きさの上限・セキュリティのヘッダー・ログアウトの CSRF・メッセージの連投") {
    val echo = Guard(HttpRoutes.of[IO] {
      case req @ POST -> Root / "api" / "echo" => req.bodyText.compile.string.flatMap(t => Ok(t.length.toString))
      case GET -> Root / "page"                => IO.pure(TextResponse(Status.Ok, "<p>hi</p>", MediaType.text.html))
    }).orNotFound
    val big = "x" * (Guard.MaxBody.toInt + 1)
    for
      small <- echo.run(Request[IO](Method.POST, uri"/api/echo").withEntity("hello"))
      _      = assertEquals(small.status, Status.Ok)
      // Content-Length で分かるときはすぐ断る. 分からない（chunked）ときは上限を超えたところで止める
      large <- echo.run(Request[IO](Method.POST, uri"/api/echo").withEntity(big))
      _      = assertEquals(large.status, Status.PayloadTooLarge)
      chunked <- echo.run(Request[IO](Method.POST, uri"/api/echo").withBodyStream(fs2.Stream.emits(big.getBytes).covary[IO])).attempt
      _      = assert(chunked.left.exists(_.isInstanceOf[BodyTooLarge]), chunked)
      page  <- echo.run(Request[IO](Method.GET, uri"/page"))
      _      = assertEquals(List("X-Frame-Options", "X-Content-Type-Options").map(h => page.headers.get(CIString(h)).map(_.head.value)),
                 List(Some("DENY"), Some("nosniff")))
      _      = assert(page.headers.get(CIString("Content-Security-Policy")).exists(_.head.value.contains("frame-ancestors 'none'")))
      // ログアウトは画面からだけ（他のサイトに送らせない）
      a     <- login("守り", "guard@example.com")
      out   <- fixture().app.run(Request[IO](Method.POST, uri"/auth/logout").putHeaders(Cookie(RequestCookie("skaka_session", a))))
      _      = assertEquals(out.status, Status.Forbidden)
      still <- call(a, Method.GET, "/api/me")
      _      = assertEquals(still.status, Status.Ok)
      // 会議のメッセージ: チャットは10秒に40件まで
      fl    <- Flood.create
      oks   <- (1 to 45).toList.traverse(_ => fl.allow(ClientMsg.Chat("hi")))
      _      = assertEquals(oks.count(identity), Flood.MaxBroadcasts)
      ping  <- fl.allow(ClientMsg.Ping())
      _      = assert(ping)
    yield ()
  }

  test("返事のない接続は切る（参加者から外す）") {
    for
      a  <- login("寝落ち", "sleep@example.com")
      r  <- call(a, Method.POST, "/api/rooms", RoomReq("夜").asJson).flatMap(json[RoomDto])
      c  <- join(Some(a), r.code).flatMap(j => connect(j.ticket))
      _  <- c.await { case ServerMsg.Welcome(_, _, _, _, _, _) => () }
      _  <- IO.sleep(20.millis) *> fixture().hub.reap(10.millis)
      rs <- c.closed
      _   = assert(rs.contains("応答"), rs)
      lv <- call(a, Method.GET, s"/api/rooms/${r.code}/live").flatMap(json[LiveDto])
      _   = assertEquals(lv.peers, Nil)
    yield ()
  }

/** テスト用の偽の skawau（OAuth のトークンの発行・ルート・アップロード・ユーザーの照会・共有）. */
final class FakeSkawau(val uploads: Ref[IO, List[(Map[String, String], Array[Byte])]], val shares: Ref[IO, List[(UUID, UUID)]]):
  import FakeSkawau.*

  private def authed(req: Request[IO])(f: => IO[Response[IO]]): IO[Response[IO]] =
    if req.headers.get[Authorization].exists(_.credentials == Credentials.Token(AuthScheme.Bearer, Token)) then f
    else IO.pure(Response[IO](Status.Unauthorized))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "oauth" / "token" =>
      req.as[UrlForm].flatMap { f =>
        val client = req.headers.get[Authorization].collect { case Authorization(BasicCredentials(u, p)) => (u, p) }.contains(("skaka", "skaka-secret-0123456789"))
        if client && f.getFirst("code").contains(Code) && f.getFirst("code_verifier").exists(_.length >= 43) then
          Ok(Json.obj("access_token" -> Token.asJson, "token_type" -> "Bearer".asJson, "scope" -> "read write".asJson))
        else BadRequest(Json.obj("error" -> "invalid_grant".asJson))
      }
    case POST -> Root / "oauth" / "revoke" => Ok(Json.obj())
    case req @ GET -> Root / "api" / "files" / "root" => authed(req)(Ok(Json.obj("folder" -> Json.obj("id" -> Root0.asJson))))
    case req @ PUT -> Root / "api" / "files" / _ / "upload" =>
      authed(req)(req.body.compile.to(Array).flatMap { b =>
        uploads.update(_ :+ (req.params -> b)) *> Ok(Json.obj("id" -> File.asJson, "name" -> req.params("name").asJson))
      })
    case req @ POST -> Root / "api" / "users" / "lookup" =>
      authed(req)(req.as[Json].flatMap(j => Ok(Json.obj("users" -> j.hcursor.get[List[String]]("emails").getOrElse(Nil)
        .filter(_ == "member2@example.com").map(_ => Json.obj("id" -> Member.asJson)).asJson))))
    case req @ POST -> Root / "api" / "files" / id / "shares" =>
      authed(req)(req.as[Json].flatMap(j => shares.update(_ :+ (UUID.fromString(id), j.hcursor.get[UUID]("userId").toOption.get)) *> Created(Json.obj())))
  }

object FakeSkawau:
  val Token  = "skawau_test-token"
  val Code   = "good-code"
  val Root0  = UUID.randomUUID()
  val File   = UUID.randomUUID()
  val Member = UUID.randomUUID()
  def create: IO[FakeSkawau] = (Ref.of[IO, List[(Map[String, String], Array[Byte])]](Nil), Ref.of[IO, List[(UUID, UUID)]](Nil)).mapN(new FakeSkawau(_, _))

/** テスト用の偽の skuaka（トークン1つ・ユーザー2人・メインのカレンダー1つ・予定）. */
final class FakeSkuaka(val events: Ref[IO, Map[UUID, Json]]):
  import FakeSkuaka.*

  def add(title: String, location: String): IO[UUID] =
    val id = UUID.randomUUID()
    events.update(_.updated(id, Json.obj("id" -> id.asJson, "calendarId" -> Calendar.asJson, "title" -> title.asJson, "description" -> "".asJson,
      "location" -> location.asJson, "allDay" -> false.asJson, "start" -> "2026-11-21T00:00:00Z".asJson, "end" -> "2026-11-21T00:30:00Z".asJson,
      "timezone" -> "Asia/Tokyo".asJson, "attendees" -> Json.arr(), "canEdit" -> true.asJson, "version" -> 1.asJson))).as(id)

  private def authed(req: Request[IO])(f: => IO[Response[IO]]): IO[Response[IO]] =
    if req.headers.get[Authorization].exists(_.credentials == Credentials.Token(AuthScheme.Bearer, Token)) then f
    else IO.pure(Response[IO](Status.Unauthorized))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "me" => authed(req)(Ok(Json.obj("id" -> Me.asJson, "name" -> "予定 太郎".asJson)))
    case req @ PATCH -> Root / "api" / "me" => authed(req)(Ok(Json.obj("id" -> Me.asJson)))
    case req @ GET -> Root / "api" / "calendars" =>
      authed(req)(Ok(Json.arr(Json.obj("id" -> Calendar.asJson, "primary" -> true.asJson, "role" -> "owner".asJson, "owner" -> Json.obj("id" -> Me.asJson)))))
    case req @ GET -> Root / "api" / "users" =>
      authed(req)(Ok(if req.params.get("q").contains("guest@example.com") then Json.arr(Json.obj("id" -> Guest.asJson, "email" -> "guest@example.com".asJson)) else Json.arr()))
    case req @ POST -> Root / "api" / "events" =>
      authed(req)(req.as[Json].flatMap { j =>
        val id = UUID.randomUUID()
        val e  = j.deepMerge(Json.obj("id" -> id.asJson, "canEdit" -> true.asJson, "version" -> 1.asJson))
        events.update(_.updated(id, e)) *> Created(e)
      })
    case req @ GET -> Root / "api" / "events" / id =>
      authed(req)(events.get.flatMap(_.get(UUID.fromString(id)).fold(NotFound())(Ok(_))))
    case req @ PUT -> Root / "api" / "events" / id =>
      authed(req)(req.as[Json].flatMap(j => events.modify { m =>
        val k = UUID.fromString(id)
        val e = m(k).deepMerge(j.mapObject(_.filterKeys(_ != "attendees")))
        (m.updated(k, e), e)
      }.flatMap(Ok(_))))
    case req @ GET -> Root / "api" / "events" =>
      authed(req)(events.get.flatMap(m => Ok(m.values.toList.map(e => e.deepMerge(Json.obj("eventId" -> e.hcursor.downField("id").focus.get))))))
  }

object FakeSkuaka:
  val Token    = "skuaka_test-token"
  val Me       = UUID.randomUUID()
  val Guest    = UUID.randomUUID()
  val Calendar = UUID.randomUUID()
  def create: IO[FakeSkuaka] = Ref.of[IO, Map[UUID, Json]](Map.empty).map(new FakeSkuaka(_))
