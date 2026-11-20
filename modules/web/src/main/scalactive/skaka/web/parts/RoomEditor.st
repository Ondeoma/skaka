<script lang="scala">
import skaka.web.*
import skaka.shared.*

// 会議室の作成・変更（room が None なら作成）
case class Props(room: Option[RoomDto])

case class State(name: RV[String], description: RV[String], guest: RV[Boolean], lobby: RV[Boolean], act: ActionState = useAction())

def initState(props: Props) = State(RV(props.room.map(_.name).getOrElse("")), RV(props.room.map(_.description).getOrElse("")),
  RV(props.room.exists(_.guestAccess)), RV(props.room.exists(_.lobby)))

def close(): Unit = Store.roomEditor := None

def save(s: State, props: Props): Unit = {
  val req = RoomReq(s.name.v.trim, s.description.v.trim, s.guest.v, s.lobby.v)
  Store.submit(s.act)(props.room.fold(Api.createRoom(req))(r => Api.updateRoom(r.code, req))) { r =>
    close()
    Store.loadRooms()
    Toasts.show(if (props.room.isEmpty) s"会議室「${r.name}」を作りました" else "保存しました")
  }
}

def title(props: Props): String = if (props.room.isEmpty) "会議室を作る" else "会議室の設定"
</script>

<template>
  <Modal :onClose="_ => close()">
    <h2>{{ title(props) }}</h2>
    <form @submit.prevent="save(s, props)">
      <label class="field"><span>名前</span><input type="text" v-model="s.name" maxlength="100" placeholder="例: 開発チームの定例" v-focus></label>
      <label class="field"><span>説明（任意）</span><textarea v-model="s.description" maxlength="2000"></textarea></label>
      <label class="check"><input type="checkbox" v-model="s.guest">
        <span><b>ゲストも入れる</b><br><span class="muted small">ログインしない人も URL から入れます（名前を入れて待機室で待ち、主催者が許可してから入ります）</span></span></label>
      <label class="check"><input type="checkbox" v-model="s.lobby">
        <span><b>待機室を使う</b><br><span class="muted small">チームのメンバーも、主催者（会議室の持ち主）が許可してから入ります</span></span></label>
      <p v-if="props.room.isDefined" class="small muted">URL: <code>{{ props.room.map(_.url).getOrElse("") }}</code></p>
      <div class="actions">
        <button type="button" @click="close()">キャンセル</button>
        <button type="submit" class="primary" :disabled="s.act.busy">保存</button>
      </div>
    </form>
  </Modal>
</template>

<style scoped>
.check { display: flex; gap: .6rem; align-items: flex-start; margin: .8rem 0; cursor: pointer; }
.check input { margin-top: .25rem; }
</style>
