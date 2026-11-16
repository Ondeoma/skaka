<script lang="scala">
import skaka.web.*
import skaka.shared.*

// 参加者の一覧と、主催者の操作（マイクをオフにする・挙手を下ろす・主催者にする・退出させる）、待機室、ロック
case class State(menu: RV[String] = RV(""))

def initState() = State()

def everyone = computed { Meeting.self.v.toList ++ Meeting.peers.v }
def isSelf(p: PeerInfo): Boolean = p.id == Meeting.selfId
def label(p: PeerInfo): String = p.name + (if (isSelf(p)) "（あなた）" else "") + (if (p.guest) "（ゲスト）" else "")
def user(p: PeerInfo): Option[UserBrief] = Some(Meeting.brief(p))
def toggleMenu(s: State, id: String): Unit = s.menu := (if (s.menu.v == id) "" else id)
def act(s: State, m: ClientMsg): Unit = { s.menu := ""; Meeting.send(m) }
def remove(s: State, p: PeerInfo): Unit = {
  s.menu := ""
  Dialog.confirm(s"${p.name} さんを会議から退出させますか？", "退出させる")(Meeting.send(ClientMsg.Remove(p.id)))
}
def muteAll(): Unit = Dialog.confirm("主催者以外の全員のマイクをオフにしますか？", "オフにする", danger = false)(Meeting.send(ClientMsg.MuteRequest(None)))
def admitAll(): Unit = Meeting.lobby.v.foreach(p => Meeting.send(ClientMsg.Admit(p.id)))
def count = computed { everyone.v.size }
</script>

<template>
  <div class="people">
    <div v-if="Meeting.lobby.v.nonEmpty" class="lobby">
      <h3>待機室（{{ Meeting.lobby.v.size }}人）</h3>
      <div v-for="p in Meeting.lobby" :key="p.id" class="person">
        <Avatar :user="user(p)" :size="28" /><span class="grow nm">{{ label(p) }}</span>
        <template v-if="Meeting.canAdmit">
          <button class="primary small" @click="Meeting.send(ClientMsg.Admit(p.id))">許可</button>
          <button class="small" @click="Meeting.send(ClientMsg.Deny(p.id))">拒否</button>
        </template>
      </div>
      <button v-if="Meeting.canAdmit.v && Meeting.lobby.v.size > 1" class="small" @click="admitAll()">全員を許可</button>
      <p v-if="!Meeting.canAdmit.v" class="muted small">主催者が許可します。</p>
    </div>
    <h3>参加者（{{ count }}人）</h3>
    <div v-for="p in everyone" :key="p.id" class="person">
      <Avatar :user="user(p)" :size="28" />
      <span class="grow nm">{{ label(p) }}<span v-if="p.host" class="badge">主催者</span></span>
      <span v-if="p.media.hand" title="挙手">✋</span>
      <span v-if="p.media.screen.isDefined" title="画面を共有中">🖥</span>
      <span :title='if (p.media.audio) "マイクオン" else "マイクオフ"'>{{ if (p.media.audio) "🎙" else "🔇" }}</span>
      <span :title='if (p.media.video) "カメラオン" else "カメラオフ"' class="dim">{{ if (p.media.video) "📷" else "🚫" }}</span>
      <div v-if="Meeting.isHost && !isSelf(p)" class="ops">
        <button class="ghost small" @click="toggleMenu(s, p.id)">⋯</button>
        <div v-if="s.menu.mapC(_ == p.id)" class="menu">
          <a v-if="p.media.audio" href="#" @click.prevent="act(s, ClientMsg.MuteRequest(Some(p.id)))">マイクをオフにする</a>
          <a v-if="p.media.hand" href="#" @click.prevent="act(s, ClientMsg.LowerHand(p.id))">挙手を下ろす</a>
          <a v-if="!p.guest && !p.host" href="#" @click.prevent="act(s, ClientMsg.SetHost(p.id, true))">主催者にする</a>
          <a v-if="p.host" href="#" @click.prevent="act(s, ClientMsg.SetHost(p.id, false))">主催者から外す</a>
          <a href="#" class="danger" @click.prevent="remove(s, p)">会議から退出させる</a>
        </div>
      </div>
      <button v-if="isSelf(p) && p.media.hand" class="ghost small" @click="Meeting.toggleHand()">下ろす</button>
    </div>
    <div v-if="Meeting.isHost" class="hostops">
      <button class="small" @click="muteAll()">全員のマイクをオフ</button>
      <button class="small" @click="Meeting.send(ClientMsg.Lock(!Meeting.locked.v))">{{ if (Meeting.locked.v) "🔓 ロックを外す" else "🔒 会議をロック" }}</button>
    </div>
    <p v-if="Meeting.locked" class="muted small">ロック中は、主催者のほかは新しく入れません。</p>
  </div>
</template>

<style scoped>
.people { padding: .4rem .7rem 1rem; overflow-y: auto; height: 100%; }
h3 { font-size: .85rem; color: var(--muted); margin: .8rem 0 .4rem; }
.person { display: flex; gap: .45rem; align-items: center; padding: .35rem 0; position: relative; font-size: .88rem; }
.nm { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.badge { margin-left: .35rem; background: var(--accent-weak); color: var(--accent); }
.dim { opacity: .75; }
.lobby { background: color-mix(in srgb, var(--warn) 12%, transparent); border-radius: 10px; padding: .3rem .6rem .6rem; }
.ops { position: relative; }
.menu { position: absolute; right: 0; top: 1.8rem; z-index: 5; background: var(--surface); border: 1px solid var(--line); border-radius: 8px;
  box-shadow: var(--shadow); min-width: 190px; display: flex; flex-direction: column; padding: .25rem; }
.menu a { padding: .4rem .6rem; color: var(--fg); border-radius: 6px; font-size: .85rem; }
.menu a:hover { background: var(--hover); text-decoration: none; }
.menu a.danger { color: var(--danger); }
.hostops { display: flex; gap: .4rem; flex-wrap: wrap; margin-top: 1rem; border-top: 1px solid var(--line); padding-top: .8rem; }
</style>
