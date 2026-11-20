package skaka.web

import io.circe.parser.{decode, parse}
import io.circe.syntax.*
import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skaka.shared.*

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.util.{Failure, Success, Try}

/** 会議の画面に並べる映像の枠. screen は画面共有. key はその枠の id（自分のカメラは "self"）. */
final case class Tile(key: String, peer: String, screen: Boolean)

/** 浮かんで消えるリアクション. */
final case class Reaction(id: Int, name: String, emoji: String, left: Int)

/**
 * 会議（1度に1つ）. 映像・音声は参加者どうしが WebRTC で直接送り合う（メッシュ. 相手ごとに RTCPeerConnection）.
 * 接続の手順は「Perfect Negotiation」（どちらから offer を出してもぶつからない. id の小さいほうが譲る）.
 * サーバーとは WebSocket で、SDP・ICE の中継と、参加者の状態・チャットをやり取りする.
 */
object Meeting {

  // ---------------------------------------------------------------- 画面に出す状態

  /** idle（入る前）/ connecting / waiting（待機室）/ in（会議中）/ closed（終わった・入れなかった）. */
  val phase: RV[String] = RV("idle")
  val reason: RV[String] = RV("")
  val code: RV[String] = RV("")
  val roomName: RV[String] = RV("")
  val self: RV[Option[PeerInfo]] = RV(None)
  val peers: RV[List[PeerInfo]] = RV(Nil)
  val lobby: RV[List[PeerInfo]] = RV(Nil)
  val hostPresent: RV[Boolean] = RV(false)
  val chat: RV[List[ChatMsg]] = RV(Nil)
  val unread: RV[Int] = RV(0)
  val locked: RV[Boolean] = RV(false)
  val startedAt: RV[Double] = RV(0)
  val now: RV[Double] = RV(js.Date.now())
  val reactions: RV[List[Reaction]] = RV(Nil)

  /** 話している人（自分は "self"）. */
  val speaking: RV[Set[String]] = RV(Set.empty)

  /** 最後に話した人（スピーカーの表示で大きく出す）. */
  val active: RV[Option[String]] = RV(None)

  /** 映像の割り当てが変わったら増やす（枠が video に入れ直す）. */
  val streams: RV[Int] = RV(0)

  /** 相手ごとの接続の状態（connected / connecting / failed など）. */
  val links: RV[Map[String, String]] = RV(Map.empty)

  val mic: RV[Boolean] = RV(Prefs.get("mic", "true") == "true")
  val cam: RV[Boolean] = RV(Prefs.get("cam", "true") == "true")
  val sharing: RV[Boolean] = RV(false)
  val hand: RV[Boolean] = RV(false)

  /** マイク・カメラが使えるか（許可されていない・機器がない）. */
  val micError: RV[String] = RV("")
  val camError: RV[String] = RV("")
  val devices: RV[List[(String, String, String)]] = RV(Nil) // (kind, deviceId, label)

  /** 右の欄（chat / people / なし）・表示（gallery / speaker）・固定した枠. */
  val panel: RV[String] = RV("")
  val layout: RV[String] = RV(Prefs.get("layout", "gallery"))
  val pinned: RV[Option[String]] = RV(None)

  def isHost: Boolean = self.v.exists(_.host)
  def selfId: String = self.v.map(_.id).getOrElse("")

  /** 待機室の人を入れられるか: 主催者. 主催者がいなければ、ログインしているメンバー（サーバーの Live.admitters と同じ）. */
  val canAdmit: CRV[Boolean] = computed { self.v.exists(me => me.host || (!me.guest && !peers.v.exists(_.host))) }

  /** 参加者のアイコン用（ゲストは接続の id から色を決める）. */
  def person(p: PeerInfo): Person = Person(p.userId.fold(p.id)(_.toString), p.name, p.avatarUrl)

  /** 画面の大きさ（ギャラリーの並べ方に使う）. */
  val viewport: RV[(Int, Int)] = RV((dom.window.innerWidth.toInt, dom.window.innerHeight.toInt))
  dom.window.addEventListener(
    "resize",
    (_: dom.Event) => viewport := ((dom.window.innerWidth.toInt, dom.window.innerHeight.toInt))
  )

  // ---------------------------------------------------------------- 内部の状態

  private var ws: Option[dom.WebSocket] = None

  /** 文字起こしの音声を送るときの鍵（切符と一緒に受け取る）. */
  var speechKey: String = ""
  private var ice: js.Array[js.Any] = js.Array()
  private var conns: Map[String, PeerConn] = Map.empty

  /** 相手 → (MediaStream の id → MediaStream). */
  private var remote: Map[String, Map[String, js.Dynamic]] = Map.empty
  private var audioEls: Map[String, dom.HTMLMediaElement] = Map.empty
  private var timers: List[Int] = Nil
  private var reactionSeq = 0
  // 話している人の検出（Web Audio）
  private var audioCtx: Option[js.Dynamic] = None
  private var analysers: Map[String, (js.Dynamic, js.Dynamic)] = Map.empty // key → (source, analyser)
  private var levelTimer: Option[Int] = None
  private var lastLoud: Map[String, Double] = Map.empty

  /** 自分のカメラ・マイクの MediaStream（トラックは出し入れする）と、画面共有の MediaStream. */
  var local: Option[js.Dynamic] = None
  var screen: Option[js.Dynamic] = None

  private def mediaDevices: js.Dynamic = dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices

  private def tracks(s: js.Dynamic, kind: String): List[js.Dynamic] =
    s.getTracks().asInstanceOf[js.Array[js.Dynamic]].toList.filter(_.kind.asInstanceOf[String] == kind)

  def audioTrack: Option[js.Dynamic] = local.flatMap(tracks(_, "audio").headOption)

  /** 送っている映像（背景を変えていれば作った映像、そうでなければカメラ）. */
  private def videoTrack: Option[js.Dynamic] = local.flatMap(tracks(_, "video").headOption)

  /** カメラそのもの（背景を変えるときの元. 止める・解像度を変えるのはこちら）. */
  private var camera: Option[js.Dynamic] = None

  /** 送る映像を入れ替える（自分の枠・相手・録画に届く）. */
  private def setLocalVideo(t: Option[js.Dynamic]): Unit = {
    videoTrack.foreach(old => local.foreach(_.removeTrack(old)))
    t.foreach(x => local.foreach(_.addTrack(x)))
    conns.values.foreach(_.setVideo(t))
    streams := streams.v + 1
    publish()
  }

  /**
   * カメラから送る映像を作る（背景を変えていれば切り抜いた映像）. 背景を変えるときは、できあがるまで映像を送らない
   * （元の背景が一瞬でも相手に見えないように）.
   */
  private def route(cam: js.Dynamic): Unit =
    if (!Background.active) { Background.detach(); setLocalVideo(Some(cam)) }
    else {
      if (videoTrack.contains(cam)) setLocalVideo(None)
      Background.attach(cam).foreach(out => if (camera.contains(cam)) setLocalVideo(Some(out)))
    }

  /** 背景の設定を変えた. */
  def applyBackground(): Unit = camera.foreach(route)

  // ---------------------------------------------------------------- 機器

  /** カメラ・マイクの条件（品質の設定から. 機器を選んでいればその機器）. */
  private def constraint(kind: String, withDevice: Boolean = true): js.Dynamic = {
    val id = Prefs.get(kind, "")
    val c = kind match {
      case "audioinput" =>
        val a = js.Dynamic.literal(
          echoCancellation = Quality.echo.v,
          noiseSuppression = Quality.noise.v,
          autoGainControl = Quality.gain.v
        )
        if (Quality.hifi.v) { a.sampleRate = 48000; a.channelCount = 1 }
        a
      case _ =>
        val h = Quality.resolution.v
        js.Dynamic.literal(
          width = js.Dynamic.literal(ideal = h * 16 / 9),
          height = js.Dynamic.literal(ideal = h),
          frameRate = js.Dynamic.literal(ideal = Quality.fps.v, max = Quality.fps.v)
        )
    }
    if (withDevice && id.nonEmpty) c.deviceId = js.Dynamic.literal(exact = id)
    c
  }

  /** 品質の設定を変えた: 使っているカメラ・マイク・画面共有と、送る量に反映する. */
  def applyQuality(kind: String): Unit = kind match {
    case "video" =>
      // カメラはそのまま条件を変える（できなければ取り直す）
      camera.foreach(t =>
        t.applyConstraints(constraint(
          "videoinput",
          withDevice = false
        )).asInstanceOf[js.Promise[Unit]].toFuture.onComplete {
          case Success(_) => if (Background.active) applyBackground() else streams := streams.v + 1; updateBitrates()
          case Failure(_) => enableVideo().foreach(_ => updateBitrates())
        }
      )
      if (camera.isEmpty) updateBitrates()
    case "audio" =>
      // エコー除去などは途中で変えられないブラウザがあるため、マイクを取り直す. Chrome は同じマイクの開いているトラックと
      // 音声処理を共有するので、前のトラックを先に止める（一瞬だけ音が途切れる）
      audioTrack.foreach { old =>
        old.stop()
        local.foreach(_.removeTrack(old))
        enableAudio()
      }
      updateBitrates()
    case "screen" =>
      screen.foreach(s =>
        tracks(s, "video").foreach { t =>
          t.contentHint = screenHint
          t.applyConstraints(screenConstraint).asInstanceOf[js.Promise[Unit]].toFuture.failed.foreach(_ => ())
        }
      )
    case _ => updateBitrates()
  }

  /** 画面共有の条件: 文字をくっきり（解像度を優先. 15fps まで）か、動きをなめらかに（30fps. 720p）. */
  private def screenConstraint: js.Dynamic =
    if (Quality.screen.v == "motion") js.Dynamic.literal(
      frameRate = js.Dynamic.literal(ideal = 30, max = 30),
      width = js.Dynamic.literal(max = 1280),
      height = js.Dynamic.literal(max = 720)
    )
    else js.Dynamic.literal(
      frameRate = js.Dynamic.literal(ideal = 15, max = 15),
      width = js.Dynamic.literal(max = 2560),
      height = js.Dynamic.literal(max = 1440)
    )

  private def screenHint: String = if (Quality.screen.v == "motion") "motion" else "detail"

  /** いま送っているカメラの映像（「1280×720・24fps」. カメラが止まっていれば空）. */
  def videoInfo: String = camera.fold("") { t =>
    val s = t.getSettings()
    val (w, h, f) = (
      s.width.asInstanceOf[js.UndefOr[Double]],
      s.height.asInstanceOf[js.UndefOr[Double]],
      s.frameRate.asInstanceOf[js.UndefOr[Double]]
    )
    if (w.isEmpty) "" else s"${w.get.toInt}×${h.getOrElse(0.0).toInt}" + f.fold("")(x => s"・${math.round(x)}fps")
  }

  private def getUserMedia(audio: Boolean, video: Boolean): Future[js.Dynamic] =
    if (js.isUndefined(mediaDevices)) Future.failed(new Exception("このブラウザ（または http の接続）ではカメラ・マイクを使えません"))
    else
      mediaDevices.getUserMedia(js.Dynamic.literal(
        audio = if (audio) constraint("audioinput") else false,
        video = if (video) constraint("videoinput") else false
      )).asInstanceOf[js.Promise[js.Dynamic]].toFuture

  private def mediaError(e: Throwable): String = {
    val name = Try(
      e.asInstanceOf[js.JavaScriptException].exception.asInstanceOf[js.Dynamic].name.asInstanceOf[String]
    ).getOrElse("")
    name match {
      case "NotAllowedError" => "使用が許可されていません（ブラウザのサイトの設定で許可してください）"
      case "NotFoundError" => "機器が見つかりません"
      case "NotReadableError" => "ほかのアプリが使っているため使えません"
      case "OverconstrainedError" => "選んだ機器が見つかりません（設定で選び直してください）"
      case _ => Option(e.getMessage).filter(_.nonEmpty).getOrElse("使えません")
    }
  }

  /** 機器の一覧（許可されるまでは名前が空）. */
  def refreshDevices(): Unit =
    if (!js.isUndefined(mediaDevices))
      mediaDevices.enumerateDevices().asInstanceOf[js.Promise[js.Array[js.Dynamic]]].toFuture.foreach { ds =>
        devices := ds.toList.map(d =>
          (d.kind.asInstanceOf[String], d.deviceId.asInstanceOf[String], d.label.asInstanceOf[String])
        )
      }

  /** 入る前: カメラ・マイクを用意する（オンのものだけ）. */
  def prepare(): Unit = {
    if (local.isEmpty) local = Some(js.Dynamic.newInstance(js.Dynamic.global.MediaStream)())
    if (mic.v && audioTrack.isEmpty) enableAudio()
    if (cam.v && camera.isEmpty) enableVideo()
    refreshDevices()
    if (!js.isUndefined(mediaDevices)) mediaDevices.ondevicechange = (_: js.Any) => refreshDevices()
  }

  private def enableAudio(): Future[Unit] =
    getUserMedia(audio = true, video = false).map { s =>
      tracks(s, "audio").headOption.foreach { t =>
        audioTrack.foreach(old => { old.stop(); local.foreach(_.removeTrack(old)) })
        local.foreach(_.addTrack(t))
        t.enabled = mic.v
        conns.values.foreach(_.setAudio(Some(t)))
        watchSpeaking("self", local.get)
      }
      micError := ""
      refreshDevices()
    }.recover { case e => micError := mediaError(e); mic := false; publish() }

  private def enableVideo(): Future[Unit] =
    getUserMedia(audio = false, video = true).map { s =>
      tracks(s, "video").headOption.foreach { t =>
        if (!cam.v) t.stop()
        else {
          camera.foreach(_.stop())
          camera = Some(t)
          route(t)
        }
      }
      camError := ""
      refreshDevices()
    }.recover { case e => camError := mediaError(e); cam := false; publish() }

  def toggleMic(): Unit = {
    mic := !mic.v
    Prefs.set("mic", mic.v.toString)
    audioTrack match {
      case Some(t) => t.enabled = mic.v
      case None if mic.v => enableAudio()
      case None => ()
    }
    publish()
  }

  def toggleCam(): Unit = {
    cam := !cam.v
    Prefs.set("cam", cam.v.toString)
    if (cam.v) enableVideo()
    else {
      // カメラを止める（ランプも消える）. 相手には何も送らない
      Background.detach()
      camera.foreach(_.stop())
      camera = None
      setLocalVideo(None)
    }
    publish()
  }

  /** 機器を選び直した（使っているものを差し替える）. */
  def setDevice(kind: String, id: String): Unit = {
    Prefs.set(kind, id)
    kind match {
      case "audioinput" => if (audioTrack.isDefined || mic.v) enableAudio()
      case "videoinput" => if (cam.v) enableVideo()
      case _ =>
        audioEls.values.foreach(sink)
    }
  }

  /** 音を出す機器（setSinkId に対応したブラウザだけ）. */
  private def sink(el: dom.HTMLMediaElement): Unit = {
    val id = Prefs.get("audiooutput", "")
    val d = el.asInstanceOf[js.Dynamic]
    if (id.nonEmpty && !js.isUndefined(d.setSinkId))
      d.setSinkId(id).asInstanceOf[js.Promise[Unit]].toFuture.failed.foreach(_ => ())
  }

  // ---------------------------------------------------------------- 画面共有

  def toggleShare(): Unit = if (sharing.v) stopShare() else startShare()

  private def startShare(): Unit =
    if (js.isUndefined(mediaDevices) || js.isUndefined(mediaDevices.getDisplayMedia))
      Toasts.show("このブラウザは画面の共有に対応していません", error = true)
    else if (peers.v.exists(_.media.screen.isDefined)) Toasts.show("ほかの人が画面を共有しています", error = true)
    else
      mediaDevices.getDisplayMedia(js.Dynamic.literal(video = screenConstraint, audio = true))
        .asInstanceOf[js.Promise[js.Dynamic]].toFuture.onComplete {
          case Success(s) =>
            screen = Some(s)
            tracks(s, "video").foreach { t =>
              t.contentHint = screenHint
              // ブラウザの「共有を停止」で止めたとき
              t.onended = (_: js.Any) => stopShare()
            }
            conns.values.foreach(_.addScreen(s))
            sharing := true
            publish()
          case Failure(_) => () // 選ぶのをやめた
        }

  def stopShare(): Unit = if (sharing.v) {
    screen.foreach(s => tracks(s, "video").foreach(_.stop()))
    screen.foreach(s => tracks(s, "audio").foreach(_.stop()))
    screen = None
    conns.values.foreach(_.removeScreen())
    sharing := false
    publish()
  }

  // ---------------------------------------------------------------- 入る・出る

  /** 入る（切符をもらって WebSocket につなぐ）. */
  def join(roomCode: String, name: String, guestName: Option[String]): Unit = {
    code := roomCode
    roomName := name
    phase := "connecting"
    reason := ""
    prepare()
    Api.join(roomCode, guestName).onComplete {
      case Failure(e) => phase := "closed"; reason := e.getMessage
      case Success(j) =>
        ice = j.iceServers.map(s => js.JSON.parse(s.asJson.dropNullValues.noSpaces).asInstanceOf[js.Any]).toJSArray
        speechKey = j.key
        connect(j.ticket)
    }
  }

  private def connect(ticket: String): Unit = {
    val proto = if (dom.window.location.protocol == "https:") "wss:" else "ws:"
    val sock = new dom.WebSocket(s"$proto//${dom.window.location.host}/ws/$ticket")
    ws = Some(sock)
    sock.onmessage = (e: dom.MessageEvent) => decode[ServerMsg](e.data.toString).foreach(onServer)
    // 切れた・退出させられた・会議が終わった: カメラ・マイクも止める（終わった画面でランプが点いたままにしない）
    sock.onclose = (_: dom.CloseEvent) =>
      if (ws.contains(sock)) {
        ws = None
        if (phase.v != "closed") { phase := "closed"; if (reason.v.isEmpty) reason := "接続が切れました" }
        cleanup()
        stopLocal()
      }
    timers ::= dom.window.setInterval(() => send(ClientMsg.Ping()), 20000)
    timers ::= dom.window.setInterval(() => now := js.Date.now(), 1000)
  }

  /** 出る（画面を離れたときも）. カメラ・マイクも止める. */
  def leave(): Unit = {
    ws.foreach { s => ws = None; s.close() }
    if (phase.v != "idle") phase := "closed"
    cleanup()
    stopLocal()
  }

  /** 入る前の状態に戻す（もう一度入る）. */
  def reset(): Unit = {
    leave()
    phase := "idle"
    reason := ""
    chat := Nil
    unread := 0
  }

  def stopLocal(): Unit = {
    Background.detach()
    camera.foreach(_.stop())
    camera = None
    local.foreach(s => s.getTracks().asInstanceOf[js.Array[js.Dynamic]].foreach(_.stop()))
    local = None
    stopShare()
    stopSpeaking("self")
    streams := streams.v + 1
  }

  private def cleanup(): Unit = {
    Recorder.leave()
    Transcriber.leave()
    timers.foreach(dom.window.clearInterval)
    timers = Nil
    conns.values.foreach(_.close())
    conns = Map.empty
    remote = Map.empty
    audioEls.values.foreach(_.remove())
    audioEls = Map.empty
    peers := Nil
    lobby := Nil
    links := Map.empty
    speaking := Set.empty
    analysers.keys.filterNot(_ == "self").foreach(stopSpeaking)
    self := None
    hand := false
    pinned := None
    stopShare()
    streams := streams.v + 1
  }

  def send(m: ClientMsg): Unit = ws.filter(_.readyState == dom.WebSocket.OPEN).foreach(_.send(m.asJson.noSpaces))

  /** 自分の状態を知らせる. */
  def publish(): Unit = {
    val st = MediaState(
      mic.v && audioTrack.isDefined,
      cam.v && videoTrack.isDefined,
      screen.map(_.id.asInstanceOf[String]),
      hand.v
    )
    self := self.v.map(_.copy(media = st))
    send(ClientMsg.Media(st))
  }

  def toggleHand(): Unit = { hand := !hand.v; publish() }

  def react(emoji: String): Unit = send(ClientMsg.React(emoji))

  def say(text: String, to: Option[String]): Unit = send(ClientMsg.Chat(text, to))

  // ---------------------------------------------------------------- サーバーから

  private def name(id: String): String = if (id == selfId) "あなた" else peers.v.find(_.id == id).map(_.name).getOrElse("")

  private def onServer(m: ServerMsg): Unit = m match {
    case ServerMsg.Welcome(me, ps, history, lk, started, recording, transcribing) =>
      self := Some(me)
      Recorder.notice := recording
      Transcriber.set(transcribing)
      peers := ps
      chat := history
      locked := lk
      startedAt := Time.epoch(started)
      phase := "in"
      publish()
      ps.foreach(p => open(p.id, initiator = true))
    case ServerMsg.Waiting(hp) =>
      phase := "waiting"
      hostPresent := hp
    case ServerMsg.Closed(r) =>
      reason := r
      phase := "closed"
    case ServerMsg.Joined(p) =>
      // 接続は、入ってきた側からの offer を受けてから作る
      peers := peers.v.filterNot(_.id == p.id) :+ p
      Sound.play("join")
    case ServerMsg.Left(id) =>
      peers := peers.v.filterNot(_.id == id)
      drop(id)
      Sound.play("leave")
    case ServerMsg.Updated(p) =>
      if (p.id == selfId) {
        if (p.host && !isHost) Toasts.show("主催者になりました")
        self := Some(p.copy(media = self.v.map(_.media).getOrElse(p.media)))
      } else peers := peers.v.map(x => if (x.id == p.id) p else x)
    case ServerMsg.Signal(from, data) =>
      conns.get(from).orElse(Option.when(peers.v.exists(_.id == from))(open(
        from,
        initiator = false
      ))).foreach(_.onSignal(data))
    case ServerMsg.Media(id, st) =>
      val before = peers.v.find(_.id == id)
      peers := peers.v.map(p => if (p.id == id) p.copy(media = st) else p)
      if (before.exists(b => !b.media.hand && st.hand)) Toasts.show(s"✋ ${name(id)} さんが手を挙げました")
      streams := streams.v + 1
    case ServerMsg.Chat(msg) =>
      chat := (chat.v :+ msg).takeRight(500)
      if (panel.v != "chat" && msg.from != selfId) {
        unread := unread.v + 1
        Sound.play("chat")
      }
    case ServerMsg.React(from, emoji) =>
      reactionSeq += 1
      val r = Reaction(reactionSeq, name(from), emoji, 5 + scala.util.Random.nextInt(25))
      reactions := (reactions.v :+ r).takeRight(20)
      dom.window.setTimeout(() => reactions := reactions.v.filterNot(_.id == r.id), 4200)
    case ServerMsg.Lobby(ws) =>
      if (ws.size > lobby.v.size) {
        Toasts.show(s"${ws.lastOption.map(_.name).getOrElse("")} さんが待機室にいます"); Sound.play("knock")
      }
      lobby := ws
    case ServerMsg.MuteRequested(by) =>
      if (mic.v) toggleMic()
      Toasts.show(s"$by さんがあなたのマイクをオフにしました")
    case ServerMsg.HandLowered(by) =>
      hand := false
      publish()
      Toasts.show(s"$by さんがあなたの挙手を下ろしました")
    case ServerMsg.Locked(lk, by) =>
      locked := lk
      Toasts.show(if (lk) s"$by さんが会議をロックしました" else s"$by さんが会議のロックを外しました")
    case ServerMsg.Recording(info) =>
      if (info.isDefined && !Recorder.mine.v) Toasts.show(s"${info.get.by} さんが録画を始めました")
      if (info.isEmpty && Recorder.notice.v.isDefined && !Recorder.mine.v) Toasts.show("録画が終わりました")
      Recorder.notice := info
    case ServerMsg.Transcribing(info) =>
      if (info.isDefined && Transcriber.notice.v.isEmpty)
        Toasts.show(if (self.v.exists(_.name == info.get.by) && isHost) "文字起こしを始めました"
        else s"${info.get.by} さんが文字起こしを始めました")
      if (info.isEmpty && Transcriber.notice.v.isDefined) Toasts.show("文字起こしが終わりました")
      Transcriber.set(info)
    case ServerMsg.Caption(line) => Transcriber.caption(line)
    case ServerMsg.Error(msg) => Toasts.show(msg, error = true)
    case ServerMsg.Pong() => ()
  }

  // ---------------------------------------------------------------- 相手との接続

  /**
   * 相手との接続を作る. initiator は最初の offer を出す側（あとから入った側）. 両方が同時に offer を出すと、
   * 譲った側の ICE の候補が捨てられて ICE が始まらないことがあるため、最初の1回はどちらか一方からにする.
   */
  private def open(id: String, initiator: Boolean): PeerConn = conns.getOrElse(
    id, {
      val c = new PeerConn(id, polite = selfId < id, initiator)
      conns += id -> c
      updateBitrates()
      c
    }
  )

  private def drop(id: String): Unit = {
    conns.get(id).foreach(_.close())
    conns -= id
    remote -= id
    audioEls.filter(_._1.startsWith(id + "/")).foreach {
      (k, el) => el.remove(); audioEls -= k
    }
    analysers.keys.filter(_.startsWith(id)).foreach(stopSpeaking)
    lastLoud -= id
    if (active.v.contains(id)) active := None
    links := links.v - id
    if (pinned.v.exists(_.startsWith(id))) pinned := None
    streams := streams.v + 1
    updateBitrates()
  }

  /** 映像・音声の送る量（品質の設定と人数から. メッシュなので、人数が増えたら下げる）. */
  private def updateBitrates(): Unit = {
    val bps = Quality.videoBitrate(conns.size)
    conns.values.foreach { c => c.setBitrate(bps); c.setAudioBitrate(Quality.audioBitrate) }
  }

  /** 相手の MediaStream すべて（録画で音をまとめるため）. */
  def remoteStreams: List[js.Dynamic] = remote.values.flatMap(_.values).toList

  /** 相手の MediaStream（screen = true なら画面共有）. */
  def streamOf(peer: String, screenShare: Boolean): Option[js.Dynamic] = {
    val sid = peers.v.find(_.id == peer).flatMap(_.media.screen)
    remote.getOrElse(
      peer,
      Map.empty
    ).find((id, _) => if (screenShare) sid.contains(id) else !sid.contains(id)).map(_._2)
  }

  private def onRemoteStream(peer: String, s: js.Dynamic): Unit = {
    val sid = s.id.asInstanceOf[String]
    if (!remote.getOrElse(peer, Map.empty).contains(sid)) {
      remote = remote.updated(peer, remote.getOrElse(peer, Map.empty).updated(sid, s))
      // 音は映像の枠と別の audio 要素で鳴らす（表示のしかたで枠が消えても聞こえるように）
      val el = dom.document.createElement("audio").asInstanceOf[dom.HTMLMediaElement]
      el.autoplay = true
      el.asInstanceOf[js.Dynamic].srcObject = s
      el.style.display = "none"
      dom.document.body.appendChild(el)
      sink(el)
      audioEls += s"$peer/$sid" -> el
      s.onremovetrack = (_: js.Any) => streams := streams.v + 1
      watchSpeaking(peer, s)
    }
    streams := streams.v + 1
  }

  /** 1人の相手との RTCPeerConnection. */
  private final class PeerConn(val id: String, polite: Boolean, initiator: Boolean) {
    val pc: js.Dynamic =
      js.Dynamic.newInstance(js.Dynamic.global.RTCPeerConnection)(js.Dynamic.literal(iceServers = ice))
    private var makingOffer = false
    private var ignoreOffer = false
    private var chain: Future[Unit] = Future.unit
    private var bitrate = 1500000

    private def opts(s: js.Dynamic) = js.Dynamic.literal(direction = "sendonly", streams = js.Array(s))

    // 自分のカメラ・マイクは送る枠を作っておき、オン・オフはトラックの差し替えで行う（交渉し直さない）.
    // 送る枠を足すと offer を出すので、受ける側は最初の offer に答えてから足す
    private var audioTx: Option[js.Dynamic] = None
    private var videoTx: Option[js.Dynamic] = None
    private var screenTx: List[js.Dynamic] = Nil

    private def addSenders(): Unit = if (audioTx.isEmpty) {
      val stream = local.getOrElse(js.Dynamic.newInstance(js.Dynamic.global.MediaStream)())
      audioTx = Some(pc.addTransceiver(audioTrack.getOrElse("audio".asInstanceOf[js.Dynamic]), opts(stream)))
      videoTx = Some(pc.addTransceiver(videoTrack.getOrElse("video".asInstanceOf[js.Dynamic]), opts(stream)))
      screen.foreach(addScreen)
    }

    // つながらないまま（ICE が始まらない・途中で切れた）なら、譲らない側が ICE をやり直す
    private val watchdog = dom.window.setInterval(
      () => {
        val st = pc.connectionState.asInstanceOf[String]
        if (!polite && audioTx.isDefined && st != "connected" && pc.signalingState.asInstanceOf[String] == "stable")
          pc.restartIce()
      },
      8000
    )

    pc.onnegotiationneeded = (_: js.Any) => {
      makingOffer = true
      pc.setLocalDescription().asInstanceOf[js.Promise[Unit]].toFuture
        .map(_ => signal(js.Dynamic.literal(description = pc.localDescription)))
        .recover { case e => dom.console.warn(s"offer failed: $e") }
        .onComplete(_ => makingOffer = false)
    }
    pc.onicecandidate = (e: js.Dynamic) => if (e.candidate != null) signal(js.Dynamic.literal(candidate = e.candidate))
    pc.ontrack =
      (e: js.Dynamic) => e.streams.asInstanceOf[js.Array[js.Dynamic]].headOption.foreach(onRemoteStream(id, _))
    pc.onconnectionstatechange = (_: js.Any) => {
      val st = pc.connectionState.asInstanceOf[String]
      links := links.v.updated(id, st)
      if (st == "failed") pc.restartIce()
      if (st == "connected") { setBitrate(bitrate); setAudioBitrate(Quality.audioBitrate) }
    }

    private def signal(d: js.Dynamic): Unit =
      parse(js.JSON.stringify(d)).foreach(j => send(ClientMsg.Signal(id, j)))

    /** 相手からの SDP・ICE（順番に処理する）. */
    def onSignal(data: io.circe.Json): Unit = {
      val d = js.JSON.parse(data.noSpaces).asInstanceOf[js.Dynamic]
      chain = chain.flatMap(_ => handle(d)).recover { case e => dom.console.warn(s"signal failed: $e") }
    }

    private def handle(d: js.Dynamic): Future[Unit] = {
      val desc = d.description
      if (!js.isUndefined(desc) && desc != null) {
        val offer = desc.`type`.asInstanceOf[String] == "offer"
        val collision = offer && (makingOffer || pc.signalingState.asInstanceOf[String] != "stable")
        ignoreOffer = !polite && collision
        if (ignoreOffer) Future.unit
        else
          pc.setRemoteDescription(desc).asInstanceOf[js.Promise[Unit]].toFuture.flatMap { _ =>
            if (offer) pc.setLocalDescription().asInstanceOf[js.Promise[Unit]].toFuture.map { _ =>
              signal(js.Dynamic.literal(description = pc.localDescription))
              addSenders() // 受ける側: 答えたあとで自分の送る枠を足す（安定してから offer を出す）
            }
            else Future.unit
          }
      } else if (!js.isUndefined(d.candidate) && d.candidate != null)
        pc.addIceCandidate(d.candidate).asInstanceOf[js.Promise[Unit]].toFuture.recover { case e if ignoreOffer => () }
      else Future.unit
    }

    if (initiator) addSenders()

    def setAudio(t: Option[js.Dynamic]): Unit = audioTx.foreach(_.sender.replaceTrack(t.orNull))
    def setVideo(t: Option[js.Dynamic]): Unit = {
      videoTx.foreach(_.sender.replaceTrack(t.orNull)); setBitrate(bitrate)
    }

    /** マイクの音の上限（None ならブラウザの既定）. */
    def setAudioBitrate(bps: Option[Int]): Unit = limit(audioTx, bps)

    /** 送る枠の上限（maxBitrate）を変える（None なら外す）. */
    private def limit(tx: Option[js.Dynamic], bps: Option[Int]): Unit = tx.foreach(t =>
      Try {
        val p = t.sender.getParameters()
        p.encodings.asInstanceOf[js.UndefOr[js.Array[js.Dynamic]]].toOption.filter(_.nonEmpty).foreach { es =>
          bps match {
            case Some(b) => es(0).maxBitrate = b
            case None => es(0).asInstanceOf[js.Dictionary[js.Any]].remove("maxBitrate")
          }
          t.sender.setParameters(p).asInstanceOf[js.Promise[Unit]].toFuture.failed.foreach(_ => ())
        }
      }
    )

    /** 画面共有の送る枠を足す（自分の送る枠がまだなら、足すときに一緒に足す）. */
    def addScreen(s: js.Dynamic): Unit = if (audioTx.isDefined)
      screenTx = s.getTracks().asInstanceOf[js.Array[js.Dynamic]].toList.map(t => pc.addTransceiver(t, opts(s)))

    def removeScreen(): Unit = {
      screenTx.foreach(tx => Try(tx.stop()))
      screenTx = Nil
    }

    /** カメラの映像の上限（画面共有は上限を付けない）. */
    def setBitrate(bps: Int): Unit = { bitrate = bps; limit(videoTx, Some(bps)) }

    def close(): Unit = { dom.window.clearInterval(watchdog); Try(pc.close()) }
  }

  // ---------------------------------------------------------------- 話している人

  private def ctx(): Option[js.Dynamic] = {
    if (audioCtx.isEmpty) audioCtx = Try {
      val C = if (!js.isUndefined(js.Dynamic.global.AudioContext)) js.Dynamic.global.AudioContext
      else js.Dynamic.global.webkitAudioContext
      js.Dynamic.newInstance(C)()
    }.toOption
    audioCtx.foreach(c => if (c.state.asInstanceOf[String] == "suspended") c.resume())
    audioCtx
  }

  /** stream の音の大きさを見る（key は "self" か相手の id + "/" + stream の id）. */
  private def watchSpeaking(peer: String, s: js.Dynamic): Unit = if (tracks(s, "audio").nonEmpty) ctx().foreach { c =>
    val key = if (peer == "self") "self" else s"$peer/${s.id}"
    stopSpeaking(key)
    Try {
      val src = c.createMediaStreamSource(s)
      val an = c.createAnalyser()
      an.fftSize = 512
      src.connect(an)
      analysers += key -> (src, an)
    }
    if (levelTimer.isEmpty) levelTimer = Some(dom.window.setInterval(() => measure(), 150))
  }

  private def stopSpeaking(key: String): Unit = {
    analysers.get(key).foreach((src, _) => Try(src.disconnect()))
    analysers -= key
    // 見るものがなくなったら止める
    if (analysers.isEmpty) { levelTimer.foreach(dom.window.clearInterval); levelTimer = None; lastLoud = Map.empty }
  }

  private val buf = new js.typedarray.Uint8Array(512)

  private def measure(): Unit = {
    val t = js.Date.now()
    analysers.foreach {
      (key, v) =>
        v._2.getByteTimeDomainData(buf)
        var sum = 0.0
        var i = 0
        while (i < buf.length) { val x = (buf(i) - 128) / 128.0; sum += x * x; i += 1 }
        val rms = math.sqrt(sum / buf.length)
        val who = key.takeWhile(_ != '/')
        val audible = if (who == "self") mic.v else peers.v.exists(p => p.id == who && p.media.audio)
        if (rms > 0.04 && audible) lastLoud += who -> t
    }
    val now = lastLoud.filter(_._2 > t - 600).keySet
    if (now != speaking.v) speaking := now
    // いちばん最近に話し始めた相手（自分は除く）
    now.filterNot(_ == "self").headOption.foreach(p => if (!active.v.contains(p)) active := Some(p))
  }

  // ---------------------------------------------------------------- 枠

  /** 画面に並べる枠: 画面共有、自分、相手（入った順）. */
  val tiles: CRV[List[Tile]] = computed {
    streams.v
    val ps = peers.v
    ps.filter(_.media.screen.isDefined).map(p => Tile(s"${p.id}/screen", p.id, screen = true)) ++
      self.v.toList.map(_ => Tile("self", "self", screen = false)) ++
      ps.map(p => Tile(p.id, p.id, screen = false))
  }

  /** スピーカーの表示で大きく出す枠: 固定した枠、画面共有、話している人、最初の相手. */
  val stage: CRV[Option[String]] = computed {
    val ts = tiles.v
    pinned.v.filter(k => ts.exists(_.key == k))
      .orElse(ts.find(_.screen).map(_.key))
      .orElse(active.v.filter(a => ts.exists(_.key == a)))
      .orElse(ts.find(_.key != "self").map(_.key))
      .orElse(ts.headOption.map(_.key))
  }

  /** 表示: 画面共有があれば自動でスピーカーの表示. */
  val effectiveLayout: CRV[String] = computed {
    if (tiles.v.exists(_.screen) || pinned.v.isDefined) "speaker" else layout.v
  }

  def setLayout(v: String): Unit = { layout := v; pinned := None; Prefs.set("layout", v) }
}
