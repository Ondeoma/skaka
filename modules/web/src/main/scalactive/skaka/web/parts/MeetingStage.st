<script lang="scala">
import skaka.web.*
import skaka.shared.*
import org.scalajs.dom

import scala.scalajs.js

// 会議中の画面: 映像（ギャラリー / スピーカー）、下の操作の列、右の欄（チャット・参加者）、リアクション
case class Props(url: String)

case class State(reactions: RV[Boolean] = RV(false), more: RV[Boolean] = RV(false), devices: RV[Boolean] = RV(false),
                 keys: RV[Option[js.Function1[dom.KeyboardEvent, Unit]]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = {
  val f: js.Function1[dom.KeyboardEvent, Unit] = (e: dom.KeyboardEvent) =>
    if (!Dom.typing(e) && !e.ctrlKey && !e.metaKey && !e.altKey && Dialog.current.v.isEmpty) {
      val done = e.key match {
        case "m" => Meeting.toggleMic(); true
        case "v" => Meeting.toggleCam(); true
        case "h" => Meeting.toggleHand(); true
        case "c" => togglePanel("chat"); true
        case "p" => togglePanel("people"); true
        case "g" => Meeting.setLayout(if (Meeting.layout.v == "gallery") "speaker" else "gallery"); true
        case _   => false
      }
      if (done) e.preventDefault()
    }
  dom.window.addEventListener("keydown", f)
  s.keys := Some(f)
}

def unmounted(s: State): Unit = s.keys.v.foreach(f => dom.window.removeEventListener("keydown", f))

def togglePanel(p: String): Unit = {
  Meeting.panel := (if (Meeting.panel.v == p) "" else p)
  if (p == "chat") Meeting.unread := 0
}

def leave(): Unit = { Meeting.leave(); Meeting.reason := "会議から退出しました" }

def endAll(s: State): Unit = {
  s.more := false
  Dialog.confirm("全員を退出させて、会議を終了しますか？", "会議を終了")(Meeting.send(ClientMsg.EndAll()))
}

def react(s: State, e: String): Unit = { Meeting.react(e); s.reactions := false }

/** 録画: 主催者だけ. skawau と連携していなければ、連携の画面を新しいタブで開く. */
def record(s: State): Unit = {
  s.more := false
  if (Recorder.mine.v) Dialog.confirm("録画を止めますか？（skawau に保存します）", "録画を止める")(Recorder.stop())
  else Store.report(Api.skawau()) { l =>
    if (!l.linked)
      Dialog.confirm("録画は skawau に保存します。skawau と連携していないため、新しいタブで連携の画面を開きますか？（連携したら、もう一度録画を押してください）",
        "連携の画面を開く", danger = false)(dom.window.open(Api.connectSkawauUrl("/settings"), "_blank"))
    else
      Dialog.confirm("録画を始めますか？ 参加者全員に「録画中」と表示されます。録り終えると、あなたの skawau の「skaka 録画」フォルダに保存し、" +
        "会議に参加したメンバーと共有します。", "録画を始める", danger = false)(Recorder.start())
  }
}

def canRecord = computed { Store.skawauEnabled && Meeting.isHost && (Recorder.notice.v.isEmpty || Recorder.mine.v) }
def recLabel = computed { Recorder.notice.v.fold("")(n => if (Recorder.mine.v) "録画中" else s"録画中（${n.by} さん）") }

def elapsed = computed { if (Meeting.startedAt.v > 0) Time.elapsed(Meeting.now.v - Meeting.startedAt.v) else "" }
def count = computed { Meeting.peers.v.size + 1 }

/** ギャラリーの列の数（枠の数と画面の縦横比から）. */
def grid = computed {
  val n = math.max(1, Meeting.tiles.v.size)
  val (w, h) = Meeting.viewport.v
  val wide = w > h
  val cols = if (n == 1) 1 else if (n == 2) (if (wide) 2 else 1) else if (n <= 4) 2 else if (n <= 9) 3 else if (n <= 16) 4 else 5
  val rows = (n + cols - 1) / cols
  s"grid-template-columns: repeat($cols, minmax(0, 1fr)); grid-template-rows: repeat($rows, minmax(0, 1fr))"
}
def stageTile = computed { Meeting.stage.v.flatMap(k => Meeting.tiles.v.find(_.key == k)).toList }
def strip = computed { Meeting.tiles.v.filterNot(t => Meeting.stage.v.contains(t.key)) }
def gallery = Meeting.effectiveLayout.mapC(_ == "gallery")
def btn(on: Boolean): String = if (on) "ctl" else "ctl off"
def panelBtn(p: String) = Meeting.panel.mapC(x => if (x == p) "ctl active" else "ctl")
def knock = computed { Meeting.lobby.v.nonEmpty && Meeting.canAdmit.v && Meeting.panel.v != "people" }
def panelTitle = Meeting.panel.mapC(p => if (p == "chat") "チャット" else "参加者")
def rstyle(r: Reaction): String = s"left: ${r.left}%"
</script>

<template>
  <div class="meet">
    <div class="bar">
      <b class="nm">{{ Meeting.roomName }}</b>
      <span class="muted">{{ elapsed }}</span>
      <span v-if="Meeting.locked" title="ロック中">🔒</span>
      <span v-if="Recorder.notice.mapC(_.isDefined)" class="rec" title="この会議は録画されています"><i></i>{{ recLabel }}</span>
      <button v-if="Recorder.mine" class="small rec-stop" @click="record(s)">録画を止める</button>
      <span v-if="Recorder.finishing" class="muted small">録画を送っています…</span>
      <span class="grow"></span>
      <button class="ghost small" title="会議室の URL をコピー" @click='Dom.copy(props.url, "会議室の URL をコピーしました")'>🔗 招待</button>
      <button class="ghost small" :title='if (gallery.v) "スピーカーの表示に（g）" else "ギャラリーの表示に（g）"'
        @click='Meeting.setLayout(if (gallery.v) "speaker" else "gallery")'>{{ if (gallery.v) "▣ スピーカー" else "▦ ギャラリー" }}</button>
    </div>

    <div class="body">
      <div class="videos">
        <div v-if="Meeting.sharing" class="banner">画面を共有しています <button class="small" @click="Meeting.stopShare()">共有を停止</button></div>
        <div v-if="knock" class="banner knock">待機室に {{ Meeting.lobby.v.size }} 人います
          <button class="small primary" @click='togglePanel("people")'>確認する</button></div>
        <div v-if="gallery.v" class="gallery" :style="grid">
          <VideoTile v-for="t in Meeting.tiles" :key="t.key" :tile="t" />
        </div>
        <div v-else class="speaker">
          <div class="main"><VideoTile v-for="t in stageTile" :key="t.key" :tile="t" /></div>
          <div v-if="strip.v.nonEmpty" class="strip">
            <div v-for="t in strip" :key="t.key" class="cell"><VideoTile :tile="t" :small="true" /></div>
          </div>
        </div>
        <div class="floating">
          <div v-for="r in Meeting.reactions" :key="r.id" class="float" :style="rstyle(r)"><span class="emoji">{{ r.emoji }}</span><span class="who">{{ r.name }}</span></div>
        </div>
      </div>
      <aside v-if="Meeting.panel.mapC(_.nonEmpty)" class="panel-side">
        <div class="phead"><b>{{ panelTitle }}</b><button class="ghost small" @click='Meeting.panel := ""'>×</button></div>
        <ChatPanel v-if='Meeting.panel.mapC(_ == "chat")' />
        <PeoplePanel v-else />
      </aside>
    </div>

    <div class="controls">
      <button :class="btn(Meeting.mic.v)" @click="Meeting.toggleMic()" title="マイク（m）">{{ if (Meeting.mic.v) "🎙" else "🔇" }}<span>マイク</span></button>
      <button :class="btn(Meeting.cam.v)" @click="Meeting.toggleCam()" title="カメラ（v）">{{ if (Meeting.cam.v) "📷" else "🚫" }}<span>カメラ</span></button>
      <button :class='if (Meeting.sharing.v) "ctl active" else "ctl"' @click="Meeting.toggleShare()" title="画面を共有">🖥<span>{{ if (Meeting.sharing.v) "共有を停止" else "画面共有" }}</span></button>
      <button :class='if (Meeting.hand.v) "ctl active" else "ctl"' @click="Meeting.toggleHand()" title="挙手（h）">✋<span>挙手</span></button>
      <div class="pop">
        <button class="ctl" @click="s.reactions := !s.reactions.v" title="リアクション">😀<span>リアクション</span></button>
        <div v-if="s.reactions" class="popup emojis">
          <button v-for="e in Reactions.all" class="ghost" @click="react(s, e)">{{ e }}</button>
        </div>
      </div>
      <button :class='panelBtn("chat").v' @click='togglePanel("chat")' title="チャット（c）">💬<span>チャット</span>
        <i v-if="Meeting.unread.mapC(_ > 0)" class="dot-n">{{ Meeting.unread }}</i></button>
      <button :class='panelBtn("people").v' @click='togglePanel("people")' title="参加者（p）">👥<span>{{ count }}人</span>
        <i v-if="Meeting.lobby.mapC(_.nonEmpty)" class="dot-n">{{ Meeting.lobby.v.size }}</i></button>
      <div class="pop">
        <button class="ctl" @click="s.more := !s.more.v" title="その他">⋯<span>その他</span></button>
        <div v-if="s.more" class="popup menu" @click="s.more := false">
          <a href="#" @click.prevent="s.devices := true">⚙ カメラ・マイクの設定</a>
          <a href="#" @click.prevent='Dom.copy(props.url, "会議室の URL をコピーしました")'>🔗 会議室の URL をコピー</a>
          <a v-if="canRecord" href="#" @click.prevent="record(s)">{{ if (Recorder.mine.v) "⏹ 録画を止める" else "⏺ 録画する（skawau に保存）" }}</a>
          <a v-if="Meeting.isHost" href="#" @click.prevent="Meeting.send(ClientMsg.Lock(!Meeting.locked.v))">{{ if (Meeting.locked.v) "🔓 ロックを外す" else "🔒 会議をロック" }}</a>
          <a v-if="Meeting.isHost" href="#" class="danger" @click.prevent="endAll(s)">⏹ 全員の会議を終了</a>
        </div>
      </div>
      <button class="ctl leave" @click="leave()" title="退出">📞<span>退出</span></button>
    </div>
    <DeviceSettings v-if="s.devices" :onClose="(_: Unit) => s.devices := false" />
  </div>
</template>

<style scoped>
.meet { display: flex; flex-direction: column; height: 100%; background: #0b1220; color: #e5e7eb; }
.bar { display: flex; align-items: center; gap: .7rem; padding: .45rem .9rem; font-size: .9rem; }
.bar .nm { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.bar button { color: #e5e7eb; }
.bar button:hover { background: rgba(255, 255, 255, .08); }
.body { flex: 1; display: flex; min-height: 0; }
.rec { display: inline-flex; align-items: center; gap: .35rem; color: #fca5a5; font-weight: 600; white-space: nowrap; }
.rec i { width: .6rem; height: .6rem; border-radius: 50%; background: #ef4444; animation: blink 1.4s ease-in-out infinite; }
.bar .rec-stop { background: #dc2626; border-color: #dc2626; color: #fff; padding: .1rem .55rem; }
@keyframes blink { 50% { opacity: .25 } }
.videos { position: relative; flex: 1; min-width: 0; padding: .4rem .6rem; display: flex; flex-direction: column; gap: .4rem; }
.gallery { flex: 1; display: grid; gap: .5rem; min-height: 0; }
.speaker { flex: 1; display: flex; flex-direction: column; gap: .5rem; min-height: 0; }
.speaker .main { flex: 1; min-height: 0; }
.strip { display: flex; gap: .5rem; height: 120px; overflow-x: auto; flex: none; }
.strip .cell { width: 180px; flex: none; height: 100%; }
.banner { display: flex; gap: .6rem; align-items: center; justify-content: center; background: rgba(59, 130, 246, .25); border-radius: 8px; padding: .3rem .6rem; font-size: .85rem; }
.banner.knock { background: rgba(217, 119, 6, .3); }
.banner button { color: inherit; }
.panel-side { width: 340px; flex: none; background: var(--bg); color: var(--fg); border-radius: 12px 0 0 12px; margin: .4rem 0; display: flex; flex-direction: column; min-height: 0; }
.phead { display: flex; justify-content: space-between; align-items: center; padding: .5rem .7rem; border-bottom: 1px solid var(--line); }
.controls { display: flex; gap: .35rem; justify-content: center; align-items: center; padding: .5rem; flex-wrap: wrap; }
.ctl { position: relative; flex-direction: column; gap: .1rem; min-width: 4.4rem; padding: .35rem .5rem; font-size: 1.15rem; border-radius: 12px;
  background: rgba(255, 255, 255, .08); border: none; color: #e5e7eb; }
.ctl span { font-size: .68rem; }
.ctl:hover { background: rgba(255, 255, 255, .16); }
.ctl.off { background: #dc2626; color: #fff; }
.ctl.active { background: #2563eb; color: #fff; }
.ctl.leave { background: #dc2626; color: #fff; margin-left: .6rem; }
.dot-n { position: absolute; top: .1rem; right: .5rem; background: #ef4444; color: #fff; font-style: normal; font-size: .65rem; border-radius: 999px; padding: 0 .35rem; }
.pop { position: relative; }
.popup { position: absolute; bottom: 4rem; left: 50%; transform: translateX(-50%); background: var(--surface); color: var(--fg); border: 1px solid var(--line);
  border-radius: 12px; box-shadow: var(--shadow); padding: .3rem; z-index: 20; }
.emojis { display: flex; gap: .1rem; }
.emojis button { font-size: 1.4rem; padding: .2rem .35rem; }
.menu { display: flex; flex-direction: column; min-width: 230px; }
.menu a { padding: .5rem .7rem; color: var(--fg); border-radius: 6px; white-space: nowrap; font-size: .88rem; }
.menu a:hover { background: var(--hover); text-decoration: none; }
.menu a.danger { color: var(--danger); }
.floating { position: absolute; left: 0; right: 0; bottom: 0; height: 60%; pointer-events: none; overflow: hidden; }
.float { position: absolute; bottom: 0; display: flex; flex-direction: column; align-items: center; animation: rise 4s ease-out forwards; }
.float .emoji { font-size: 2.4rem; }
.float .who { font-size: .7rem; background: rgba(0, 0, 0, .5); padding: 0 .4rem; border-radius: 4px; }
@keyframes rise { 0% { transform: translateY(0); opacity: 0 } 10% { opacity: 1 } 80% { opacity: 1 } 100% { transform: translateY(-320px); opacity: 0 } }
@media (max-width: 720px) {
  .panel-side { position: fixed; inset: 0 0 3.6rem 0; width: auto; margin: 0; border-radius: 0; z-index: 30; }
  .controls { flex-wrap: nowrap; gap: .2rem; padding: .4rem .3rem; justify-content: space-between; }
  .ctl { min-width: 0; flex: 1 1 0; max-width: 3.2rem; padding: .45rem .1rem; font-size: 1rem; }
  .ctl span { display: none; }
  .ctl.leave { margin-left: .2rem; }
  .pop { flex: 1 1 0; max-width: 3.2rem; display: flex; }
  .pop .ctl { max-width: none; }
  .bar .ghost { padding: .15rem .35rem; }
  .strip { height: 80px; }
  .strip .cell { width: 110px; }
}
</style>
