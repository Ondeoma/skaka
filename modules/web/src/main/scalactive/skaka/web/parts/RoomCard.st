<script lang="scala">
import skaka.web.*
import skaka.shared.*

// 会議室の一覧の1行（入る・URL のコピー・設定・削除）
case class Props(room: RoomDto, onOpen: RoomDto => Unit, onRemove: RoomDto => Unit)

def tags(r: RoomDto): List[String] = List(
  Option.when(r.guestAccess)("ゲスト可"),
  Option.when(r.lobby)("待機室"),
  Option.when(!r.isOwner)(s"${r.owner.name} さんの会議室")
).flatten
def schedule(r: RoomDto): String = r.event.fold("") { e =>
  if (e.allDay) s"📅 ${e.title}" else s"📅 ${Time.dateTime(e.start)} ${e.title}"
}
def removeLabel(r: RoomDto): String = if (r.isOwner) "削除" else "一覧から外す"
</script>

<template>
  <div class="room">
    <div class="icon" @click="props.onOpen(props.room)"><template v-if="props.room.personal">🏠</template><span v-else class="ico ico-video"></span></div>
    <div class="grow body" @click="props.onOpen(props.room)">
      <div class="name">{{ props.room.name }}
        <span v-if="props.room.live > 0" class="live">● {{ props.room.live }}人が会議中</span>
      </div>
      <div class="muted small"><code>{{ props.room.code }}</code>
        <span v-for="t in tags(props.room)" class="badge">{{ t }}</span></div>
      <div v-if="props.room.event.isDefined" class="small ev">{{ schedule(props.room) }}</div>
    </div>
    <div class="ops">
      <button class="primary small" @click="props.onOpen(props.room)">入る</button>
      <button class="ghost small" title="URL をコピー" @click='Dom.copy(props.room.url, "会議室の URL をコピーしました")'>🔗</button>
      <button v-if="props.room.isOwner" class="ghost small gear" title="設定" aria-label="設定" @click="Store.roomEditor := Some(Some(props.room))"><span class="ico ico-gear"></span></button>
      <button v-if="!props.room.personal" class="ghost small" :title="removeLabel(props.room)" @click="props.onRemove(props.room)">✕</button>
    </div>
  </div>
</template>

<style scoped>
.room { display: flex; gap: .8rem; align-items: center; padding: .7rem .9rem; border: 1px solid var(--line); border-radius: 12px; margin-bottom: .5rem;
  background: var(--bg); transition: border-color .15s, box-shadow .15s; }
.room:hover { border-color: color-mix(in srgb, var(--accent) 45%, var(--line)); box-shadow: var(--shadow); }
.icon { font-size: 1.5rem; width: 2.6rem; height: 2.6rem; display: flex; align-items: center; justify-content: center; border-radius: 10px;
  background: var(--accent-weak); cursor: pointer; flex: none; }
.body { cursor: pointer; }
.name { font-weight: 600; display: flex; gap: .5rem; align-items: center; flex-wrap: wrap; }
.live { font-size: .75rem; color: var(--ok); font-weight: 600; }
.badge { margin-left: .3rem; }
.ev { margin-top: .15rem; color: var(--accent); }
.gear .ico { width: 1.35rem; height: 1.35rem; vertical-align: middle; }
.ops { display: flex; gap: .2rem; align-items: center; flex: none; }
</style>
