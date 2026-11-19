<script lang="scala">
import skaka.web.*
import skaka.shared.*

// 会議の予定を立てる: 会議室を作り、skuaka（カレンダー）の自分のカレンダーに予定を作って招待する
case class Props(draft: ScheduleDraft)

case class State(title: RV[String], date: RV[String], time: RV[String], minutes: RV[String], description: RV[String],
                 attendees: RV[List[UserBrief]], guest: RV[Boolean], lobby: RV[Boolean], busy: RV[Boolean] = RV(false))

def initState(props: Props) = {
  val d = props.draft
  State(RV(d.title), RV(d.date), RV(d.time), RV(d.minutes.toString), RV(d.description), RV(d.attendees), RV(d.guestAccess), RV(d.lobby))
}

def close(): Unit = Store.scheduler := None

def save(s: State): Unit = {
  val start = Time.instant(s.date.v, s.time.v)
  val end   = new scala.scalajs.js.Date(Time.epoch(start) + s.minutes.v.toIntOption.getOrElse(30) * 60000.0).toISOString()
  val req   = ScheduleReq(s.title.v.trim, start, end, s.description.v.trim, s.attendees.v.flatMap(_.email), s.guest.v, s.lobby.v)
  Store.submit(s.busy)(Api.schedule(req)) { r =>
    close()
    Store.loadRooms()
    val miss = if (r.notFound.nonEmpty) s"（${AppNames.nameOf("skuaka")} にいないため招待できなかった人: ${r.notFound.mkString(", ")}）" else ""
    Store.toast(s"予定「${r.room.name}」を ${AppNames.nameOf("skuaka")} に作りました$miss")
  }
}

def add(s: State, u: UserBrief): Unit =
  if (u.email.isEmpty) Store.toast(s"${u.name} さんはメールアドレスがないため招待できません", error = true)
  else s.attendees := (s.attendees.v.filterNot(_.id == u.id) :+ u)
def removeAt(s: State, u: UserBrief): Unit = s.attendees := s.attendees.v.filterNot(_.id == u.id)
def exclude(s: State) = s.attendees.mapC(as => as.map(_.id) ++ Store.me.v.map(_.id).toList)

val durations: List[(String, String)] = List(15, 30, 45, 60, 90, 120, 180).map(m => m.toString -> Time.minutes(m))
</script>

<template>
  <Modal :onClose="_ => close()">
    <h2>会議の予定を立てる</h2>
    <p class="muted small">会議室を作り、{{ AppNames.of("skuaka") }} の自分のカレンダーに予定を作ります。予定の場所に会議室の URL が入り、招待した人のカレンダーにも出ます。</p>
    <form @submit.prevent="save(s)">
      <label class="field"><span>タイトル</span><input type="text" v-model="s.title" maxlength="200" placeholder="例: 週次の振り返り" autofocus></label>
      <div class="row">
        <label class="field"><span>日付</span><input type="date" v-model="s.date"></label>
        <label class="field"><span>開始</span><input type="time" v-model="s.time" step="300"></label>
        <label class="field"><span>長さ</span><select v-model="s.minutes"><option v-for="d in durations" :value="d._1">{{ d._2 }}</option></select></label>
      </div>
      <div class="field"><span>招待する人</span>
        <UserPicker :onPick="u => add(s, u)" :exclude="exclude(s).v" />
        <div class="chips people">
          <span v-for="u in s.attendees" :key="u.id" class="chip"><Avatar :user="Some(u)" :size="18" /> {{ u.name }}
            <a href="#" @click.prevent="removeAt(s, u)">×</a></span>
        </div>
      </div>
      <label class="field"><span>説明（任意）</span><textarea v-model="s.description" maxlength="5000"></textarea></label>
      <label class="row"><input type="checkbox" v-model="s.guest"> ゲスト（ログインしない人）も入れる</label>
      <label class="row"><input type="checkbox" v-model="s.lobby"> 待機室を使う</label>
      <div class="actions">
        <button type="button" @click="close()">キャンセル</button>
        <button type="submit" class="primary" :disabled="s.busy">予定を作る</button>
      </div>
    </form>
  </Modal>
</template>

<style scoped>
.people { margin-top: .4rem; }
.chip { display: inline-flex; align-items: center; gap: .3rem; cursor: default; }
.chip a { color: var(--muted); }
</style>
