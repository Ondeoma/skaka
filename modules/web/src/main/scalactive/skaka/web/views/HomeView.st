<script lang="scala">
import skaka.web.*
import skaka.shared.*
import skaka.web.parts.*
import org.scalajs.dom

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// ホーム: すぐに会議を始める・コードで入る・会議室の一覧・skuaka の今後の予定
case class State(code: RV[String] = RV(""), link: RV[Option[TokenLinkDto]] = RV(None),
                 upcoming: RV[Option[List[UpcomingDto]]] = RV(None), upError: RV[String] = RV(""), timer: RV[Int] = RV(0))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("会議室")
  Store.loadRooms()
  if (Store.skuakaEnabled) Store.report(Api.tokenLink("skuaka")) { l =>
    s.link := Some(l)
    if (l.linked) loadUpcoming(s)
  }
  // 会議中の人数は30秒ごと、予定は5分ごとに読み直す
  var n = 0
  s.timer := dom.window.setInterval(() => {
    n += 1
    Store.loadRooms()
    if (n % 10 == 0 && s.link.v.exists(_.linked)) loadUpcoming(s)
  }, 30000)
}

def unmounted(s: State): Unit = dom.window.clearInterval(s.timer.v)

def loadUpcoming(s: State): Unit =
  Api.upcoming(Time.today(), Time.addDays(Time.today(), 7)).onComplete {
    case scala.util.Success(es) => s.upcoming := Some(es); s.upError := ""
    case scala.util.Failure(e)  => s.upcoming := Some(Nil); s.upError := e.getMessage
  }

def personal = Store.rooms.mapC(_.find(r => r.personal && r.isOwner))
def mine = Store.rooms.mapC(_.filter(r => r.isOwner && !r.personal))
def recent = Store.rooms.mapC(_.filterNot(_.isOwner))
def go(r: RoomDto): Unit = Router.go(s"/r/${r.code}")

def startNow(): Unit = personal.v.foreach(go)

def enter(s: State): Unit = RoomCode.parse(s.code.v) match {
  case Some(c) => Router.go(s"/r/$c")
  case None    => Store.toast("会議室のコード（abc-defg-hij）か URL を入れてください", error = true)
}

def remove(r: RoomDto): Unit =
  if (r.isOwner) Dialog.confirm(s"会議室「${r.name}」を削除しますか？（URL は使えなくなります. 会議の履歴も消えます）", "削除") {
    Store.report(Api.deleteRoom(r.code))(_ => Store.loadRooms())
  }
  else Store.report(Api.forgetRoom(r.code))(_ => Store.loadRooms())

def attach(s: State, e: UpcomingDto): Unit =
  Store.report(Api.attachRoom(e.eventId)) { r =>
    Store.toast(s"「${e.title}」に会議室を付けました")
    Store.loadRooms()
    loadUpcoming(s)
  }

def linked(s: State) = s.link.mapC(_.exists(_.linked))
def notLinked(s: State) = s.link.mapC(_.exists(l => l.available && !l.linked))
def canSchedule(s: State) = s.link.mapC(_.exists(l => l.linked && l.canWrite))
/** 日ごとにまとめた予定. */
def days(s: State) = s.upcoming.mapC(_.getOrElse(Nil).groupBy(e => Time.dateOf(e.start)).toList.sortBy(_._1))
def noEvents(s: State) = s.upcoming.mapC(_.exists(_.isEmpty))
def when(e: UpcomingDto): String = if (e.allDay) "終日" else s"${Time.time(e.start)}–${Time.time(e.end)}"
def soon(e: UpcomingDto): Boolean = !e.allDay && Time.epoch(e.start) - scala.scalajs.js.Date.now() < 15 * 60000 && Time.epoch(e.end) > scala.scalajs.js.Date.now()
def evClass(e: UpcomingDto): String = if (soon(e)) "ev soon" else "ev"
</script>

<template>
  <div class="page">
    <section class="hero">
      <div class="actions">
        <button class="primary big" @click="startNow()" :disabled="personal.v.isEmpty"><span class="ico ico-video"></span> すぐに会議を始める</button>
        <button class="big" @click="Store.roomEditor := Some(None)">＋ 会議室を作る</button>
        <button v-if="canSchedule(s)" class="big" @click="Store.scheduler := Some(ScheduleDraft())">📅 会議の予定を立てる</button>
      </div>
      <form class="join" @submit.prevent="enter(s)">
        <input type="text" v-model="s.code" placeholder="会議室のコード・URL（abc-defg-hij）">
        <button type="submit">入る</button>
      </form>
    </section>

    <div class="cols">
      <div class="col">
        <h2>自分の会議室</h2>
        <RoomCard v-for="r in personal.v.toList" :key="r.id" :room="r" :onOpen="go" :onRemove="remove" />
        <RoomCard v-for="r in mine" :key="r.id" :room="r" :onOpen="go" :onRemove="remove" />
        <p v-if="mine.v.isEmpty" class="muted small">「＋ 会議室を作る」で、定例会議などに使う会議室を作れます。URL はずっと同じです。</p>
        <template v-if="recent.v.nonEmpty">
          <h2>最近入った会議室</h2>
          <RoomCard v-for="r in recent" :key="r.id" :room="r" :onOpen="go" :onRemove="remove" />
        </template>
      </div>

      <div v-if="Store.skuakaEnabled" class="col side">
        <h2>今後の予定 <span class="muted small">skuaka</span></h2>
        <div v-if="notLinked(s)" class="panel small">
          skuaka（カレンダー）と連携すると、今後の予定と会議室が並び、予定から会議に入れます。
          <p><RouterLink to="/settings">設定</RouterLink>で skuaka の API トークンを登録してください。</p>
        </div>
        <template v-if="linked(s)">
          <p v-if="s.upError.mapC(_.nonEmpty)" class="error small">{{ s.upError }}</p>
          <p v-if="noEvents(s)" class="muted small">7日以内の予定はありません。</p>
          <div v-for="d in days(s)" :key="d._1" class="day">
            <h3>{{ Time.dayLabel(d._1) }}</h3>
            <div v-for="e in d._2" :key="e.eventId.toString + e.start" :class="evClass(e)">
              <div class="grow">
                <div class="t">{{ e.title }}</div>
                <div class="muted small">{{ when(e) }}<span v-if="e.recurring"> ・繰り返し</span></div>
              </div>
              <Link v-if="e.room.isDefined" :to='"/r/" + e.room.getOrElse("")' cls="btn primary small">参加</Link>
              <button v-else-if="e.canEdit" class="small" @click="attach(s, e)" title="予定の場所に会議室の URL を入れます">会議室を付ける</button>
            </div>
          </div>
        </template>
      </div>
    </div>
  </div>
</template>

<style scoped>
.hero { display: flex; flex-wrap: wrap; gap: 1rem; align-items: center; justify-content: space-between; padding: 1.2rem; margin-bottom: 1.2rem;
  border-radius: 14px; background: linear-gradient(135deg, var(--accent-weak), transparent 70%), var(--side); border: 1px solid var(--line); }
.actions { display: flex; flex-wrap: wrap; gap: .5rem; }
.big { font-size: .98rem; padding: .55rem 1.1rem; border-radius: 999px; }
.join { display: flex; gap: .4rem; }
.join input { width: 18rem; border-radius: 999px; }
.cols { display: grid; grid-template-columns: minmax(0, 1fr) 360px; gap: 1.5rem; }
.col h2:first-child { margin-top: 0; }
.day h3 { font-size: .85rem; color: var(--muted); margin: .8rem 0 .3rem; }
.ev { display: flex; gap: .6rem; align-items: center; padding: .5rem .7rem; border: 1px solid var(--line); border-radius: 10px; margin-bottom: .4rem; background: var(--bg); }
.ev.soon { border-color: var(--accent); box-shadow: 0 0 0 2px var(--accent-weak); }
.ev .t { font-weight: 600; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
@media (max-width: 900px) {
  .cols { grid-template-columns: 1fr; }
  .join { width: 100%; }
  .join input { flex: 1; width: auto; }
}
</style>
