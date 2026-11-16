<script lang="scala">
import skaka.web.*
import skaka.shared.*


// 会議の履歴（自分の会議室の会議と、参加した会議. 新しい順. 続きを読める）
case class State(list: RV[List[MeetingDto]] = RV(Nil), more: RV[Boolean] = RV(false), loaded: RV[Boolean] = RV(false))

def initState() = State()

def mounted(s: State): Unit = { pageTitle("会議の履歴"); load(s, None) }

def load(s: State, before: Option[String]): Unit = Store.report(Api.meetings(before = before)) { ms =>
  s.list := s.list.v ++ ms
  s.more := ms.size >= 50
  s.loaded := true
}

def span(m: MeetingDto): String =
  s"${Time.dateTime(m.startedAt)}〜${m.endedAt.fold("開催中")(Time.time)}" +
    m.endedAt.fold("")(e => s"（${Time.elapsed(Time.epoch(e) - Time.epoch(m.startedAt))}）")
def empty(s: State) = computed { s.loaded.v && s.list.v.isEmpty }
</script>

<template>
  <div class="page narrow">
    <h1>会議の履歴</h1>
    <p v-if="empty(s)" class="empty">会議の履歴はまだありません。</p>
    <div class="table-wrap" v-if="s.list.mapC(_.nonEmpty)">
      <table class="list">
        <tr><th>日時</th><th>会議室</th><th>参加者</th><th>チャット</th></tr>
        <tr v-for="m in s.list" :key="m.id" class="clickable" @click='Router.go(s"/meetings/${m.id}")'>
          <td class="nowrap">{{ span(m) }}<span v-if="m.endedAt.isEmpty" class="badge live">開催中</span></td>
          <td>{{ m.roomName }} <span class="muted small">{{ m.roomCode }}</span></td>
          <td>{{ m.participants }}人</td>
          <td>{{ if (m.messages > 0) s"${m.messages}件" else "" }}</td>
        </tr>
      </table>
    </div>
    <p v-if="s.more"><button @click="load(s, s.list.v.lastOption.map(_.startedAt))">もっと見る</button></p>
  </div>
</template>

<style scoped>
.live { margin-left: .4rem; background: color-mix(in srgb, var(--ok) 18%, transparent); color: var(--ok); }
</style>
