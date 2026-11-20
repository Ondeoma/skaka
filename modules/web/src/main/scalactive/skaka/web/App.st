<script lang="scala">
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import org.scalajs.dom
import skaka.web.parts.*
import skaka.web.views.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// 画面全体の枠: ヘッダー（ロゴ・ナビ・メニュー）とルートごとの画面. 会議室（/r/…）はヘッダーなしで全画面
case class State(ready: RV[Boolean] = RV(false), menu: RV[Boolean] = RV(false))

def initState() = State()

val routes: PathPatternRoutes = List(
  ("/", _ => HomeView()),
  ("/r/:code", _ => RoomView()),
  ("/login", _ => LoginView()),
  ("/meetings", _ => HistoryView()),
  ("/meetings/:id", _ => MeetingView()),
  ("/settings", _ => SettingsView()),
)

def mounted(s: State): Unit = {
  Fallback.install(routes)
  val path = dom.window.location.pathname
  Api.clientConfig().foreach { c =>
    Store.config := Some(c)
    // 会議室は、ログインしていなくても開ける（ゲスト. ログインしているかは会議室の画面で確かめる）
    if (path.startsWith("/login") || path.startsWith("/r/")) s.ready := true
    else Toasts.report(Api.me()) { me =>
      Store.me := Some(me)
      AppNames.set(me.appNames)
      Observe.setUser(me.id.toString)
      Store.loadRooms()
      s.ready := true
    }
  }
}

def logout(): Unit = Toasts.report(Api.logout())(url => dom.window.location.href = url)

def inRoom = Router.href.mapC(h => new dom.URL(h).pathname.startsWith("/r/"))
def loggedIn = Store.me.mapC(_.isDefined)
def meBrief = Store.me.mapC(_.map(_.brief.person))
def userName = Store.me.mapC(_.map(_.name).getOrElse(""))
def userEmail = Store.me.mapC(_.flatMap(_.email).getOrElse(""))
def editors = Store.roomEditor.mapC(_.toList)
def schedulers = Store.scheduler.mapC(_.toList)
</script>

<template>
  <header v-if="!inRoom.v" class="top">
    <Link to="/" cls="logo"><span class="brand"><img :src='AppNames.logo("/logo.svg").v' alt="" height="34" class="brand-logo"><span class="hide-sp">{{ AppNames.self }}</span></span></Link>
    <nav v-if="loggedIn" class="navs">
      <RouterLink to="/" active-class="active">会議室</RouterLink>
      <RouterLink to="/meetings" active-class="active">履歴</RouterLink>
    </nav>
    <div class="spacer"></div>
    <div v-if="loggedIn" class="me" v-click-outside="s.menu := false">
      <button class="ghost me-btn" @click="s.menu := !s.menu.v"><Avatar :user="meBrief.v" :size="30" /></button>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <div class="who"><b>{{ userName }}</b><span class="muted small">{{ userEmail }}</span></div>
        <RouterLink to="/settings">設定</RouterLink>
        <a href="#" @click.prevent="logout()">ログアウト</a>
      </div>
    </div>
  </header>
  <main :class='if (inRoom.v) "main full" else "main"'>
    <RouterComponent v-if="s.ready" :routes="routes" />
  </main>
  <RoomEditor v-for="r in editors" :key="r" :room="r" />
  <Scheduler v-for="d in schedulers" :key="d" :draft="d" />
  <DialogHost />
  <ToastHost />
</template>

<style scoped>
.top { position: sticky; top: 0; z-index: 100; height: var(--header); display: flex; align-items: center; gap: .5rem; padding: 0 1rem;
  background: var(--bg); border-bottom: 1px solid var(--line); }
.brand { font-weight: 700; font-size: 1.3rem; color: var(--fg); letter-spacing: .01em; display: inline-flex; align-items: center; gap: .5rem;
  white-space: nowrap; margin-right: 1.2rem; }
.brand img { display: block; width: auto; max-width: 140px; }
.navs { display: flex; gap: .2rem; }
.navs a { padding: .4rem .9rem; border-radius: 999px; color: var(--muted); font-weight: 500; }
.navs a:hover { background: var(--hover); text-decoration: none; color: var(--fg); }
.navs a.active { background: var(--accent-weak); color: var(--accent); }
.spacer { flex: 1; }
.me { position: relative; }
.me-btn { padding: .15rem; border-radius: 999px; }
.menu { position: absolute; right: 0; top: 2.8rem; background: var(--surface); border: 1px solid var(--line); border-radius: 10px; box-shadow: var(--shadow);
  min-width: 220px; padding: .3rem; display: flex; flex-direction: column; z-index: 200; }
.who { display: flex; flex-direction: column; padding: .5rem .7rem; border-bottom: 1px solid var(--line); margin-bottom: .3rem; }
.main { min-height: calc(100vh - var(--header)); }
.main.full { height: 100vh; height: 100dvh; min-height: 0; }
@media (max-width: 720px) {
  .top { padding: 0 .5rem; }
  .brand { margin-right: .3rem; }
}
</style>
