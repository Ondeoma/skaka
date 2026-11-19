package skaka.core

import io.circe.Json
import skaka.shared.*

import java.time.Instant
import java.util.UUID

class CoreSpec extends munit.FunSuite {

  private val owner = UUID.randomUUID()
  private val other = UUID.randomUUID()
  private val now = Instant.parse("2026-11-15T01:00:00Z")
  private def fresh = { var n = 0L; () => { n += 1; new UUID(0, n) } }

  private def peer(id: String, user: Option[UUID], guest: Boolean = false) =
    PeerInfo(
      id,
      user,
      id.toUpperCase,
      None,
      host = false,
      guest = guest,
      MediaState(audio = true, video = true),
      now.toString
    )

  private def sent(fx: List[Effect], to: String): List[ServerMsg] = fx.collect { case Effect.Send(`to`, m) => m }

  /** 何人かを入れた状態. */
  private def withPeers(lobby: Boolean = false)(ps: PeerInfo*): (Live, List[Effect]) = {
    val f = fresh
    ps.foldLeft((Live(UUID.randomUUID(), owner, lobby), List.empty[Effect])) {
      case ((l, fx), p) => {
        val (l2, fx2) = Conference.arrive(l, p, now, f)
        (l2, fx ++ fx2)
      }
    }
  }

  test("戻り先は同じサイトのパスだけ（他のサイトへ飛ばさない）") {
    assertEquals(List("/r/abc", "/settings?x=1").map(SafePath.check), List(Some("/r/abc"), Some("/settings?x=1")))
    for p <-
        List("//evil.example", "/\\evil.example", "/\tevil", "https://evil.example", "javascript:alert(1)", "", null)
    do
      assertEquals(SafePath.check(p), None, p)
    assertEquals(SafePath.orElse(Some("//x"), "/settings"), "/settings")
  }

  test("文字起こし: 主催者だけが始める・全員に知らせる・文字起こし中だけ字幕を配って保存する・全員が出たら止まる") {
    val (l0, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    val f = fresh
    val line = TranscriptLine("t1", "b", "B", Some(other), "こんにちは", now.toString)
    assertEquals(Conference.caption(l0, line, now)._2, Nil)
    val (l1, fx1) = Conference.handle(l0, "b", ClientMsg.Transcribe(true), now, f)
    assertEquals((l1.transcribing, sent(fx1, "b")), (None, List(ServerMsg.Error("主催者だけができる操作です"))))
    val (l2, fx2) = Conference.handle(l0, "a", ClientMsg.Transcribe(true), now, f)
    assertEquals(sent(fx2, "b"), List(ServerMsg.Transcribing(Some(TranscribeNotice("A", now.toString)))))
    val (_, fx3) = Conference.caption(l2, line, now)
    assertEquals(fx3.collect { case Effect.SaveTranscript(_, l, _) => l }, List(line))
    assertEquals((sent(fx3, "a"), sent(fx3, "b")), (List(ServerMsg.Caption(line)), List(ServerMsg.Caption(line))))
    // 後から来た人にも、文字起こし中であることを知らせる
    val (l3, fx4) = Conference.arrive(l2, peer("c", Some(UUID.randomUUID())), now, f)
    assert(sent(fx4, "c").exists { case w: ServerMsg.Welcome => w.transcribing.isDefined; case _ => false })
    // 出ていった人の音声は配らない. 全員が出たら止まる
    assertEquals(Conference.caption(l3, line.copy(peer = "x"), now)._2, Nil)
    val l4 = List("a", "b", "c").foldLeft(l3)((l, id) => Conference.leave(l, id, now)._1)
    assertEquals(l4.transcribing, None)
    assertEquals(Conference.handle(l2, "a", ClientMsg.Transcribe(false), now, f)._1.transcribing, None)
  }

  test("最初の人が入ると会議が始まる. 持ち主は主催者. 後から来た人に知らせる") {
    val (l, fx) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    assertEquals(l.peers.map(p => (p.id, p.host)), Vector(("a", true), ("b", false)))
    assertEquals(fx.collect { case e: Effect.StartMeeting => e }.size, 1)
    assertEquals(fx.collect { case e: Effect.Joined => e.peer.id }, List("a", "b"))
    assert(sent(fx, "a").contains(ServerMsg.Joined(l.peers(1))))
    sent(fx, "b").collectFirst { case w: ServerMsg.Welcome => w } match {
      case Some(w) => assertEquals(w.peers.map(_.id), List("a"))
      case None => fail("Welcome がない")
    }
  }

  test("ゲストは待機室へ. 主催者がいなければメンバーが入れられる. 断ると切る") {
    val (l0, _) = withPeers()(peer("b", Some(other)))
    val (l1, fx1) = Conference.arrive(l0, peer("g", None, guest = true), now, fresh)
    assertEquals(l1.waiting.map(_.id), Vector("g"))
    assertEquals(sent(fx1, "g"), List(ServerMsg.Waiting(false)))
    assert(sent(fx1, "b").exists { case ServerMsg.Lobby(ws) => ws.map(_.id) == List("g"); case _ => false })
    val (l2, fx2) = Conference.handle(l1, "b", ClientMsg.Admit("g"), now, fresh)
    assertEquals(l2.peers.map(_.id), Vector("b", "g"))
    assert(sent(fx2, "g").exists(_.isInstanceOf[ServerMsg.Welcome]))
    // もう1人のゲストを断る
    val (l3, _) = Conference.arrive(l2, peer("h", None, guest = true), now, fresh)
    val (l4, fx4) = Conference.handle(l3, "b", ClientMsg.Deny("h"), now, fresh)
    assertEquals(l4.waiting, Vector.empty)
    assert(fx4.contains(Effect.Disconnect("h")))
  }

  test("ゲストは待機室の人を入れられない. 主催者がいれば主催者だけ") {
    val (l0, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    val (l1, _) = Conference.arrive(l0, peer("g", None, guest = true), now, fresh)
    val (l2, fx) = Conference.handle(l1, "b", ClientMsg.Admit("g"), now, fresh)
    assertEquals(l2.waiting.map(_.id), Vector("g"))
    assert(sent(fx, "b").exists(_.isInstanceOf[ServerMsg.Error]))
  }

  test("lobby の会議室ではメンバーも待つ（持ち主は待たない）") {
    val (l, fx) = withPeers(lobby = true)(peer("b", Some(other)), peer("a", Some(owner)))
    assertEquals(l.waiting.map(_.id), Vector("b"))
    assertEquals(l.peers.map(_.id), Vector("a"))
    // 主催者が入ったので、待機室の人を知らせる
    assert(sent(fx, "a").exists { case ServerMsg.Lobby(ws) => ws.map(_.id) == List("b"); case _ => false })
  }

  test("ロック中は主催者のほかは入れない. 主催者だけがロックできる") {
    val (l0, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    val (_, fx0) = Conference.handle(l0, "b", ClientMsg.Lock(true), now, fresh)
    assert(sent(fx0, "b").exists(_.isInstanceOf[ServerMsg.Error]))
    val (l1, _) = Conference.handle(l0, "a", ClientMsg.Lock(true), now, fresh)
    val (l2, fx2) = Conference.arrive(l1, peer("c", Some(UUID.randomUUID())), now, fresh)
    assertEquals(l2.peers.size, 2)
    assert(fx2.contains(Effect.Disconnect("c")))
    val (l3, _) = Conference.arrive(l1, peer("a2", Some(owner)), now, fresh)
    assertEquals(l3.peers.size, 3)
  }

  test("シグナリングは相手だけに. チャットは全員（個別は2人だけ・保存しない）") {
    val (l, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)), peer("c", Some(UUID.randomUUID())))
    val (_, s) = Conference.handle(l, "a", ClientMsg.Signal("b", Json.obj()), now, fresh)
    assertEquals(s, List(Effect.Send("b", ServerMsg.Signal("a", Json.obj()))))
    val (l2, c) = Conference.handle(l, "a", ClientMsg.Chat(" やあ "), now, fresh)
    assertEquals(c.collect { case Effect.Send(to, _: ServerMsg.Chat) => to }, List("a", "b", "c"))
    assertEquals(c.collect { case e: Effect.SaveChat => e.msg.text }, List("やあ"))
    assertEquals(l2.chat.map(_.text), Vector("やあ"))
    val (_, p) = Conference.handle(l, "a", ClientMsg.Chat("内緒", Some("c")), now, fresh)
    assertEquals(p.collect { case Effect.Send(to, _: ServerMsg.Chat) => to }.toSet, Set("a", "c"))
    assert(!p.exists(_.isInstanceOf[Effect.SaveChat]))
  }

  test("主催者にする・外す（持ち主は外せない）. 主催者にされた人は入り直しても主催者") {
    val (l, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    val (l1, _) = Conference.handle(l, "a", ClientMsg.SetHost("b", true), now, fresh)
    assert(l1.peer("b").exists(_.host))
    val (l2, fx2) = Conference.handle(l1, "b", ClientMsg.SetHost("a", false), now, fresh)
    assert(l2.peer("a").exists(_.host))
    assert(sent(fx2, "b").exists(_.isInstanceOf[ServerMsg.Error]))
    val (l3, _) = Conference.leave(l2, "b", now)
    val (l4, _) = Conference.arrive(l3, peer("b2", Some(other)), now, fresh)
    assert(l4.peer("b2").exists(_.host))
  }

  test("退出させる・全員が出たら会議を終える（チャット・ロックも消える）") {
    val (l, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    val (l1, fx1) = Conference.handle(l, "a", ClientMsg.Remove("b"), now, fresh)
    assertEquals(l1.peers.map(_.id), Vector("a"))
    assert(fx1.contains(Effect.Disconnect("b")))
    assert(sent(fx1, "a").contains(ServerMsg.Left("b")))
    val (l2, _) = Conference.handle(l1, "a", ClientMsg.Chat("x"), now, fresh)
    val (l3, fx3) = Conference.leave(l2, "a", now)
    assert(l3.isEmpty && l3.chat.isEmpty && l3.meetingId.isEmpty)
    assertEquals(fx3.collect { case e: Effect.EndMeeting => e.id }, l.meetingId.toList)
  }

  test("会議室のコードを URL・入力から読む. 予定の場所から skaka の会議室を探す") {
    assertEquals(RoomCode.parse("https://meet.example.com/r/abc-defg-hij?x=1"), Some("abc-defg-hij"))
    assertEquals(RoomCode.parse(" ABCDEFGHIJ "), Some("abc-defg-hij"))
    assertEquals(RoomCode.parse("abc-defg"), None)
    assertEquals(
      RoomLinks.find("https://meet.example.com", "会議室A / https://meet.example.com/r/abc-defg-hij"),
      Some("abc-defg-hij")
    )
    assertEquals(RoomLinks.find("https://meet.example.com", "https://other.example.com/r/abc-defg-hij"), None)
  }

  test("録画中であることを全員に知らせ、後から入った人にも Welcome で伝える. 別の録画の終わりでは消さない") {
    val (l, _) = withPeers()(peer("a", Some(owner)), peer("b", Some(other)))
    val rec = RecordingNotice(UUID.randomUUID(), "A", now.toString)
    val (l2, fx) = Conference.recording(l, Some(rec), rec.id)
    assertEquals(sent(fx, "b"), List(ServerMsg.Recording(Some(rec))))
    val (_, fx2) = Conference.arrive(l2, peer("c", Some(UUID.randomUUID())), now, fresh)
    assert(sent(fx2, "c").exists { case w: ServerMsg.Welcome => w.recording.contains(rec); case _ => false })
    assertEquals(Conference.recording(l2, None, UUID.randomUUID()), (l2, Nil))
    val (l3, fx3) = Conference.recording(l2, None, rec.id)
    assertEquals(l3.recording, None)
    assertEquals(sent(fx3, "a"), List(ServerMsg.Recording(None)))
  }
}
