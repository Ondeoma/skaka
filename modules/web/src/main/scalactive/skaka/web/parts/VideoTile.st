<script lang="scala">
import skaka.web.*
import skaka.shared.*
import org.scalajs.dom

import scala.scalajs.js

// 映像の枠（自分・相手のカメラ、画面共有）. 映像がなければアイコン. 音は Meeting が別に鳴らす（video はいつも消音）
case class Props(tile: Tile, small: Boolean = false)

case class State(video: ElementRef[dom.HTMLVideoElement] = ElementRef())

def initState() = State()

def info(props: Props): Option[PeerInfo] =
  if (props.tile.peer == "self") Meeting.self.v else Meeting.peers.v.find(_.id == props.tile.peer)

/** 映像を出すか. */
def hasVideo(props: Props): Boolean = {
  Meeting.streams.v
  if (props.tile.screen) true
  else if (props.tile.peer == "self") Meeting.cam.v && Meeting.local.isDefined
  else info(props).exists(_.media.video)
}

def stream(props: Props): Option[js.Dynamic] =
  if (props.tile.peer == "self") Meeting.local else Meeting.streamOf(props.tile.peer, props.tile.screen)

def attach(s: State, props: Props): Unit = s.video.foreach { v =>
  val want = stream(props).orNull
  val d = v.asInstanceOf[js.Dynamic]
  if (d.srcObject != want) d.srcObject = want
  if (want != null) scala.util.Try(v.asInstanceOf[js.Dynamic].play().asInstanceOf[js.Promise[Unit]].`catch`((_: Any) => ()))
}

def mounted(s: State, props: Props): Unit = {
  attach(s, props)
  watch(Meeting.streams)((_, _) => attach(s, props))
}

def name(props: Props): String = {
  // ゲストはメンバーの名前を名乗れるので、見分けられるようにする
  val n = info(props).map(p => p.name + (if (p.guest) "（ゲスト）" else "")).getOrElse("")
  if (props.tile.screen) s"$n の画面" else if (props.tile.peer == "self") s"$n（あなた）" else n
}
def micOff(props: Props): Boolean = !props.tile.screen && info(props).exists(!_.media.audio)
def handUp(props: Props): Boolean = !props.tile.screen && info(props).exists(_.media.hand)
def host(props: Props): Boolean = !props.tile.screen && info(props).exists(_.host)
def user(props: Props): Option[UserBrief] = info(props).map(Meeting.brief)
def link(props: Props): String = if (props.tile.peer == "self") "connected" else Meeting.links.v.getOrElse(props.tile.peer, "connecting")
def cls(props: Props): String = {
  val speaking = !props.tile.screen && Meeting.speaking.v.contains(props.tile.peer)
  List(Some("tile"), Option.when(speaking)("speaking"), Option.when(props.tile.screen)("screen"), Option.when(props.small)("small"),
    Option.when(props.tile.peer == "self" && !props.tile.screen)("mirror")).flatten.mkString(" ")
}
def pinned(props: Props): Boolean = Meeting.pinned.v.contains(props.tile.key)
def togglePin(props: Props): Unit = Meeting.pinned := (if (pinned(props)) None else Some(props.tile.key))
def avatarSize(props: Props): Int = if (props.small) 44 else 88
def full(s: State): Unit = s.video.foreach(v => scala.util.Try(v.asInstanceOf[js.Dynamic].requestFullscreen()))
</script>

<template>
  <div :class="cls(props)" @dblclick="full(s)">
    <video ref="s.video" autoplay playsinline muted :class='if (hasVideo(props)) "" else "hidden"'></video>
    <div v-if="!hasVideo(props)" class="noimg"><Avatar :user="user(props)" :size="avatarSize(props)" /></div>
    <div class="label">
      <span v-if="micOff(props)" class="ico" title="マイクオフ">🔇</span>
      <span v-if="host(props)" class="ico" title="主催者">★</span>
      <span class="nm">{{ name(props) }}</span>
    </div>
    <div v-if="handUp(props)" class="hand" title="挙手">✋</div>
    <div v-if='link(props) == "failed" || link(props) == "disconnected"' class="warn">接続が不安定です</div>
    <div v-else-if='link(props) != "connected"' class="warn">接続しています…</div>
    <button class="pin" :title='if (pinned(props)) "固定を外す" else "大きく表示"' @click="togglePin(props)">{{ if (pinned(props)) "📌" else "⤢" }}</button>
  </div>
</template>

<style scoped>
.tile { position: relative; background: #1f2937; border-radius: 12px; overflow: hidden; width: 100%; height: 100%; min-height: 0;
  display: flex; align-items: center; justify-content: center; box-shadow: inset 0 0 0 2px transparent; transition: box-shadow .15s; }
.tile.speaking { box-shadow: inset 0 0 0 3px #34d399; }
.tile video { width: 100%; height: 100%; object-fit: cover; background: #111827; }
.tile.screen video { object-fit: contain; background: #000; }
.tile.mirror video { transform: scaleX(-1); }
.noimg { position: absolute; inset: 0; display: flex; align-items: center; justify-content: center; }
.label { position: absolute; left: .5rem; bottom: .5rem; max-width: calc(100% - 1rem); display: flex; gap: .3rem; align-items: center;
  background: rgba(0, 0, 0, .55); color: #fff; font-size: .8rem; padding: .15rem .55rem; border-radius: 6px; }
.nm { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.ico { font-size: .75rem; }
.hand { position: absolute; top: .5rem; left: .5rem; font-size: 1.6rem; animation: wave 1s ease-in-out infinite alternate; }
@keyframes wave { from { transform: rotate(-12deg) } to { transform: rotate(12deg) } }
.warn { position: absolute; top: .5rem; right: 2.6rem; background: rgba(0, 0, 0, .6); color: #fde68a; font-size: .72rem; padding: .1rem .5rem; border-radius: 6px; }
.pin { position: absolute; top: .4rem; right: .4rem; opacity: 0; background: rgba(0, 0, 0, .55); color: #fff; border: none; padding: .15rem .45rem; }
.tile:hover .pin { opacity: 1; }
.tile.small .label { font-size: .7rem; left: .3rem; bottom: .3rem; padding: .05rem .4rem; }
.tile.small .warn { display: none; }
</style>
