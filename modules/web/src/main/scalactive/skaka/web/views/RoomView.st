<script lang="scala">
import skaka.web.*
import skaka.shared.*
import skaka.web.parts.*
import org.scalajs.dom

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

// 会議室: 入る前の確認（カメラ・マイクのプレビュー、ゲストの名前）→ 待機室 → 会議 → 終わったあと
case class State(entry: RV[Option[RoomEntryDto]] = RV(None), error: RV[String] = RV(""), guestName: RV[String] = RV(Prefs.get("guestName", "")),
                 preview: ElementRef[dom.HTMLVideoElement] = ElementRef(), devices: RV[Boolean] = RV(false),
                 pagehide: RV[Option[js.Function1[dom.Event, Unit]]] = RV(None), timer: RV[Int] = RV(0))

def initState() = State()

def code: String = RoomCode.parse(dom.window.location.pathname.stripPrefix("/r/")).getOrElse("")

def mounted(s: State): Unit = {
  Meeting.reset()
  load(s)
  watch(Meeting.streams)((_, _) => attachPreview(s))
  watch(Meeting.phase)((_, p) => if (p == "idle") dom.window.setTimeout(() => attachPreview(s), 0))
  val before: js.Function1[dom.Event, Unit] = (_: dom.Event) => Meeting.leave()
  dom.window.addEventListener("pagehide", before)
  s.pagehide := Some(before)
  // 入る前の画面: 会議中の人数を読み直す
  s.timer := dom.window.setInterval(() => if (Meeting.phase.v == "idle" && s.entry.v.isDefined)
    Api.entry(code).foreach(e => s.entry := Some(e)), 15000)
}

def unmounted(s: State): Unit = {
  s.pagehide.v.foreach(f => dom.window.removeEventListener("pagehide", f))
  dom.window.clearInterval(s.timer.v)
  Meeting.reset()
}

def load(s: State): Unit = Api.entry(code).onComplete {
  case Success(e) =>
    s.entry := Some(e)
    e.me.foreach { m => Store.me := Some(m); AppNames.set(m.appNames); Observe.setUser(m.id.toString) }
    pageTitle(e.name)
    Meeting.roomName := e.name
    Meeting.prepare()
  case Failure(ApiError(401, _)) => Api.login()
  case Failure(e)                => s.error := e.getMessage
}

def attachPreview(s: State): Unit = s.preview.foreach { v =>
  val d = v.asInstanceOf[js.Dynamic]
  val want = if (Meeting.cam.v) Meeting.local.orNull else null
  if (d.srcObject != want) d.srcObject = want
}

def guest(s: State) = s.entry.mapC(_.exists(_.me.isEmpty))

def join(s: State): Unit = s.entry.v.foreach { e =>
  val name = s.guestName.v.trim
  if (e.me.isEmpty && name.isEmpty) Store.toast("名前を入れてください", error = true)
  else {
    if (e.me.isEmpty) Prefs.set("guestName", name)
    Meeting.join(e.code, e.name, Option.when(e.me.isEmpty)(name))
  }
}

def rejoin(s: State): Unit = { Meeting.reset(); load(s) }

def url: String = s"${dom.window.location.origin}/r/$code"
def phase = Meeting.phase
def entryName(s: State) = s.entry.mapC(_.map(_.name).getOrElse(""))
def owner(s: State) = s.entry.mapC(_.map(e => s"${e.owner.name} さんの会議室").getOrElse(""))
def liveText(s: State) = s.entry.mapC(_.map(e => if (e.live == 0) "まだ誰もいません" else s"${e.live}人が会議中です").getOrElse(""))
def event(s: State) = s.entry.mapC(_.flatMap(_.event).map(e => if (e.allDay) s"📅 ${e.title}" else s"📅 ${Time.dateTime(e.start)}–${Time.time(e.end)} ${e.title}").getOrElse(""))
def lockedText(s: State) = s.entry.mapC(_.exists(_.locked))
def loggedIn = Store.me.mapC(_.isDefined)
def camOff = Meeting.cam.mapC(!_)
def btn(on: Boolean): String = if (on) "round" else "round off"
def meAvatar = Store.me.mapC(_.map(_.brief))
</script>

<template>
  <div class="room">
    <div v-if="s.error.mapC(_.nonEmpty)" class="center">
      <div class="card msg">
        <img :src='AppNames.logo("/logo.svg").v' height="72" alt="" class="brand-logo">
        <h1>会議室に入れません</h1>
        <p>{{ s.error }}</p>
        <Link v-if="loggedIn" to="/" cls="btn primary">ホームへ</Link>
      </div>
    </div>

    <div v-else-if='phase.mapC(_ == "idle")' class="center">
      <div class="prejoin" v-if="s.entry.mapC(_.isDefined)">
        <div class="preview">
          <video ref="s.preview" autoplay playsinline muted></video>
          <div v-if="camOff" class="noimg"><Avatar :user="meAvatar.v" :size="96" /><p class="muted">カメラはオフです</p></div>
          <div class="toggles">
            <button :class="btn(Meeting.mic.v)" @click="Meeting.toggleMic()" :title='if (Meeting.mic.v) "マイクをオフ" else "マイクをオン"'><img :src='if (Meeting.mic.v) "/icons/mic.svg" else "/icons/mic-off.svg"' alt=""></button>
            <button :class="btn(Meeting.cam.v)" @click="Meeting.toggleCam()" :title='if (Meeting.cam.v) "カメラをオフ" else "カメラをオン"'><img :src='if (Meeting.cam.v) "/icons/cam.svg" else "/icons/cam-off.svg"' alt=""></button>
            <button class="round" @click="s.devices := true" title="カメラ・マイクの設定"><img src="/icons/gear.svg" alt=""></button>
          </div>
        </div>
        <div class="info">
          <h1>{{ entryName(s) }}</h1>
          <p class="muted">{{ owner(s) }}</p>
          <p v-if="event(s).mapC(_.nonEmpty)" class="small">{{ event(s) }}</p>
          <p class="small">{{ liveText(s) }}</p>
          <p v-if="lockedText(s)" class="small error">🔒 会議はロックされています（主催者のほかは入れません）</p>
          <p v-if="Meeting.micError.mapC(_.nonEmpty)" class="small error">マイク: {{ Meeting.micError }}</p>
          <p v-if="Meeting.camError.mapC(_.nonEmpty)" class="small error">カメラ: {{ Meeting.camError }}</p>
          <form @submit.prevent="join(s)">
            <label v-if="guest(s)" class="field"><span>あなたの名前（ゲスト）</span><input type="text" v-model="s.guestName" maxlength="50" autofocus></label>
            <button type="submit" class="primary big">会議に参加する</button>
          </form>
          <p v-if="guest(s)" class="muted small">ゲストは、主催者が許可してから会議に入ります。チームのメンバーは <a href="#" @click.prevent="Api.login()">ログイン</a> してください。</p>
          <p class="muted small">URL: <code>{{ url }}</code> <a href="#" @click.prevent='Dom.copy(url, "会議室の URL をコピーしました")'>コピー</a></p>
          <Link v-if="loggedIn" to="/" cls="small">← ホームへ</Link>
        </div>
      </div>
      <p v-else class="muted">読み込んでいます…</p>
    </div>

    <div v-else-if='phase.mapC(p => p == "connecting" || p == "waiting")' class="center">
      <div class="card msg">
        <div class="spinner"></div>
        <h1 v-if='phase.mapC(_ == "waiting")'>主催者の許可を待っています</h1>
        <h1 v-else>接続しています…</h1>
        <p v-if='phase.mapC(_ == "waiting")' class="muted">{{ if (Meeting.hostPresent.v) "主催者に、あなたが待機室にいることを知らせました。" else "主催者はまだ会議にいません。主催者が入ると許可を求めます。" }}</p>
        <button @click="rejoin(s)">やめる</button>
      </div>
    </div>

    <MeetingStage v-else-if='phase.mapC(_ == "in")' :url="url" />

    <div v-else class="center">
      <div class="card msg">
        <img :src='AppNames.logo("/logo.svg").v' height="72" alt="" class="brand-logo">
        <h1>{{ entryName(s) }}</h1>
        <p>{{ Meeting.reason }}</p>
        <div class="row center-row">
          <button class="primary" @click="rejoin(s)">もう一度入る</button>
          <Link v-if="loggedIn" to="/" cls="btn">ホームへ</Link>
          <Link v-if="loggedIn" to="/meetings" cls="btn">会議の履歴</Link>
        </div>
      </div>
    </div>
    <DeviceSettings v-if="s.devices" :onClose="(_: Unit) => s.devices := false" />
  </div>
</template>

<style scoped>
.room { height: 100%; }
.center { height: 100%; display: flex; align-items: center; justify-content: center; padding: 1rem; overflow-y: auto; }
.msg { text-align: center; max-width: 460px; padding: 2rem; }
.msg .brand-logo { width: auto; max-width: 100%; }
.msg h1 { font-size: 1.3rem; margin: .8rem 0 .5rem; }
.center-row { justify-content: center; }
.prejoin { display: flex; gap: 2rem; align-items: center; max-width: 1080px; width: 100%; flex-wrap: wrap; justify-content: center; }
.preview { position: relative; width: min(620px, 100%); aspect-ratio: 16 / 9; background: #1f2937; border-radius: 16px; overflow: hidden; }
.preview video { width: 100%; height: 100%; object-fit: cover; transform: scaleX(-1); }
.noimg { position: absolute; inset: 0; display: flex; flex-direction: column; align-items: center; justify-content: center; color: #d1d5db; }
.toggles { position: absolute; bottom: 1rem; left: 0; right: 0; display: flex; gap: .8rem; justify-content: center; }
.round { width: 3.8rem; height: 3.8rem; padding: 0; box-shadow: 0 2px 8px rgba(0, 0, 0, .25); border-radius: 50%; justify-content: center; font-size: 1.3rem; background: rgba(255, 255, 255, .9); border: none; color: #111; }
.round.off { background: #f87171; color: #fff; }
.round img { width: 2.1rem; height: 2.1rem; pointer-events: none; }
.round:hover img { transform: scale(1.08); }
.info { width: 340px; max-width: 100%; }
.info h1 { margin-bottom: .2rem; }
.big { font-size: 1rem; padding: .65rem 2rem; border-radius: 999px; margin: .6rem 0; }
.spinner { width: 42px; height: 42px; border: 4px solid var(--line); border-top-color: var(--accent); border-radius: 50%; margin: 0 auto; animation: spin 1s linear infinite; }
@keyframes spin { to { transform: rotate(360deg) } }
</style>
