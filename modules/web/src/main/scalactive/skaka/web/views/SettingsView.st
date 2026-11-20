<script lang="scala">
import skaka.web.*
import skaka.shared.*
import skaka.web.parts.*
import org.scalajs.dom

// 設定: アイコン・名前・タイムゾーン（サーバー）、画面の色・音・プッシュ通知（この端末）、skuaka・skawau との連携、API トークンと MCP
case class State(name: RV[String] = RV(Store.me.v.map(_.name).getOrElse("")),
                 tz: RV[String] = RV(Store.me.v.map(_.timezone).getOrElse(Time.timezone())),
                 theme: RV[String] = RV(Theme.current.v))

def initState() = State()

def mounted(s: State): Unit = {
  pageTitle("設定")
  watch(s.theme)((_, v) => Theme.set(v))
}

def save(s: State): Unit =
  Toasts.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), Some(s.tz.v)))) { me =>
    Store.me := Some(me)
    Toasts.show("保存しました")
  }

def zones: List[String] = Time.timezones()

// ---------------------------------------------------------------- アイコン

def meBrief = Store.me.mapC(_.map(_.brief.person))
def uploaded = Store.me.mapC(_.exists(_.avatarUploaded))

def avatarDone(me: MeDto, msg: String): Unit = {
  Store.me := Some(me)
  Toasts.show(msg)
}

def pickAvatar(e: dom.Event): Unit = Images.pickAvatar(e)(Api.uploadAvatar)(me => avatarDone(me, "アイコンを保存しました"))

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを消しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "消す") {
    Toasts.report(Api.deleteAvatar())(me => avatarDone(me, "アイコンを消しました"))
  }

/** 連携するサービスの表示名（sruru に登録した名前）. */
def name(slug: String): String = AppNames.of(slug).v
</script>

<template>
  <div class="page narrow">
    <h1>設定</h1>
    <section class="panel">
      <h2>アカウント</h2>
      <div class="field"><span>アイコン（会議でカメラがオフのときにも表示されます）</span>
        <div class="row avatar-row">
          <Avatar :user="meBrief.v" :size="64" />
          <label class="btn">画像を選ぶ<input type="file" accept="image/png,image/jpeg,image/gif,image/webp" class="hidden-file" @change="pickAvatar(event)"></label>
          <button v-if="uploaded" type="button" class="ghost" @click="removeAvatar()">消す</button>
        </div>
        <span class="muted small">中央を正方形に切り抜いて保存します。保存しないときは、ログインの仕組み（sruru など）に登録したアイコンを使います。</span>
      </div>
      <form @submit.prevent="save(s)">
        <label class="field"><span>表示名（会議で表示される名前）</span><input type="text" v-model="s.name" maxlength="100"></label>
        <label class="field"><span>タイムゾーン（MCP・API で日時を読み書きするときに使います. 画面はこの端末のタイムゾーン {{ Time.timezone() }} で表示します）</span>
          <select v-model="s.tz"><option v-for="z in zones" :value="z">{{ z }}</option></select></label>
        <button type="submit" class="primary">保存</button>
      </form>
    </section>
    <section class="panel">
      <h2>表示と音（この端末）</h2>
      <label class="field"><span>画面の色</span>
        <select v-model="s.theme"><option v-for="t in Theme.options" :value="t._1">{{ t._2 }}</option></select></label>
      <label class="row"><input type="checkbox" :checked="Sound.enabled" @change="Sound.set(Dom.checked(event))"> 会議中に入退室・チャットの音を鳴らす</label>
    </section>
    <section class="panel">
      <h2>通知（この端末）</h2>
      <p><PushToggle /></p>
      <p class="muted small">待機室で待っている人がいるとき（自分の会議室に入っていないとき）、{{ AppNames.of("skuaka") }} の予定に招待された会議が始まったとき、録画を保存したときに知らせます。</p>
    </section>
    <TokenLinkSettings v-if="Store.skuakaEnabled" service="skuaka" :title='name("skuaka") + "（カレンダー）との連携"'
      :about='s"${name("skuaka")} の予定と連携すると、ホームに今後の予定と会議室が並び、予定から会議に入れます。会議の予定を立てると、${name("skuaka")} に予定を作って参加者を招待します。"'
      writeNeed="会議の予定を立てる" />
    <TokenLinkSettings v-if="Store.skakapoEnabled" service="skakapo" :title='name("skakapo") + "（AI エージェント）との連携"'
      :about='s"${name("skakapo")} と連携すると、会議の記録の画面から、文字起こしとチャットをもとに議事録・要約を ${name("skakapo")} に作ってもらえます（あなたの ${name("skakapo")} の依頼になり、${name("skakapo")} のクレジットを使います）。"'
      writeNeed="議事録を頼む" />
    <SkawauSettings v-if="Store.skawauEnabled" />
    <ApiTokenSettings mcpName="skaka" curlPath="/api/rooms" writeLabel="読み書き（会議室の作成・変更・会議の予定も）" about="会議室・会議の記録・会議の予定を扱ったりできます。" :tools='s"会議室の一覧・作成・変更・削除、いま入っている人、会議の履歴とチャットの記録、${AppNames.nameOf("skuaka")} の予定と会議の予定を立てる、任意の REST API の呼び出し。"' />
    <section class="panel">
      <h2>会議中のキーボードの操作</h2>
      <table class="keys">
        <tr><td><kbd>m</kbd></td><td class="small">マイクのオン・オフ</td></tr>
        <tr><td><kbd>v</kbd></td><td class="small">カメラのオン・オフ</td></tr>
        <tr><td><kbd>h</kbd></td><td class="small">挙手</td></tr>
        <tr><td><kbd>c</kbd></td><td class="small">チャット</td></tr>
        <tr><td><kbd>p</kbd></td><td class="small">参加者</td></tr>
        <tr><td><kbd>g</kbd></td><td class="small">ギャラリー・スピーカーの表示の切り替え</td></tr>
      </table>
    </section>
  </div>
</template>

<style scoped>
.avatar-row { align-items: center; gap: .8rem; }
.hidden-file { display: none; }
.keys td { padding: .2rem .8rem .2rem 0; }
</style>
