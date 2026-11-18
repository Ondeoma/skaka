<script lang="scala">
import skaka.web.*
import skaka.shared.*
import skaka.web.parts.*
import org.scalajs.dom
import scala.concurrent.{Future, Promise}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

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
  Store.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), Some(s.tz.v)))) { me =>
    Store.me := Some(me)
    Store.toast("保存しました")
  }

def zones: List[String] = Time.timezones()

// ---------------------------------------------------------------- アイコン

val AvatarPx = 256

def meBrief = Store.me.mapC(_.map(_.brief))
def uploaded = Store.me.mapC(_.exists(_.avatarUploaded))

/** 画像の中央を正方形に切り抜き、256×256 にする（WebP. 対応していないブラウザでは PNG）. */
def square(file: dom.File): Future[dom.Blob] = {
  js.Dynamic.global.createImageBitmap(file).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { img =>
    val (w, h) = (img.width.asInstanceOf[Double], img.height.asInstanceOf[Double])
    val side = math.min(w, h)
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = AvatarPx; canvas.height = AvatarPx
    val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]
    ctx.imageSmoothingQuality = "high"
    ctx.drawImage(img, (w - side) / 2, (h - side) / 2, side, side, 0, 0, AvatarPx, AvatarPx)
    val p = Promise[dom.Blob]()
    canvas.asInstanceOf[js.Dynamic].toBlob((b: dom.Blob) =>
      if (b == null) p.failure(new Exception("画像を作れませんでした")) else p.success(b), "image/webp", 0.9)
    p.future
  }
}

def avatarDone(me: MeDto, msg: String): Unit = {
  Store.me := Some(me)
  Store.toast(msg)
}

def pickAvatar(e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  Option(input.files).filter(_.length > 0).map(_(0)).foreach { f =>
    input.value = "" // 同じファイルを選び直せるように
    if (!f.`type`.startsWith("image/")) Store.toast("画像のファイルを選んでください", error = true)
    else Store.report(square(f).flatMap(Api.uploadAvatar))(me => avatarDone(me, "アイコンを保存しました"))
  }
}

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを消しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "消す") {
    Store.report(Api.deleteAvatar())(me => avatarDone(me, "アイコンを消しました"))
  }

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
      <p class="muted small">待機室で待っている人がいるとき（自分の会議室に入っていないとき）、skuaka の予定に招待された会議が始まったとき、録画を保存したときに知らせます。</p>
    </section>
    <SkuakaSettings v-if="Store.skuakaEnabled" />
    <SkawauSettings v-if="Store.skawauEnabled" />
    <ApiTokenSettings />
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
