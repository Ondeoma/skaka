<script lang="scala">
import skaka.web.*
import skaka.shared.*

// skuaka（カレンダー）との連携: skuaka の API トークンを登録する
case class State(link: RV[Option[SkuakaLinkDto]] = RV(None), token: RV[String] = RV(""), busy: RV[Boolean] = RV(false))

def initState() = State()

def mounted(s: State): Unit = Store.report(Api.skuaka())(l => s.link := Some(l))

def save(s: State): Unit =
  Store.submit(s.busy)(Api.linkSkuaka(s.token.v.trim)) { l =>
    s.link := Some(l)
    s.token := ""
    Store.toast(s"skuaka（${l.account.getOrElse("")}）と連携しました")
  }

def unlink(s: State): Unit = Dialog.confirm("skuaka との連携をやめますか？（登録した API トークンを消します）", "やめる") {
  Store.report(Api.unlinkSkuaka())(_ => Store.report(Api.skuaka())(l => s.link := Some(l)))
}

def linked(s: State) = s.link.mapC(_.exists(_.linked))
def url(s: State) = s.link.mapC(_.flatMap(_.url).getOrElse(""))
def status(s: State) = s.link.mapC(_.fold("")(l => s"${l.account.getOrElse("")} さんとして連携しています（${l.prefix.getOrElse("")}…・" +
  (if (l.canWrite) "読み書き" else "参照のみ. 会議の予定を立てるには読み書きのトークンが必要です") + "）"))
def settingsUrl(s: State) = url(s).mapC(_ + "/settings")
</script>

<template>
  <section class="panel">
    <h2>skuaka（カレンダー）との連携</h2>
    <p class="small">skuaka の予定と連携すると、ホームに今後の予定と会議室が並び、予定から会議に入れます。会議の予定を立てると、skuaka に予定を作って参加者を招待します。</p>
    <template v-if="linked(s)">
      <p class="small ok">{{ status(s) }}</p>
      <button class="danger" @click="unlink(s)">連携をやめる</button>
    </template>
    <form v-else @submit.prevent="save(s)">
      <p class="small muted">skuaka の <a :href="settingsUrl(s).v" target="_blank" rel="noopener">設定の画面</a> の「API トークンと MCP」で、
        「読み書き」の API トークンを作り、ここに貼り付けてください。トークンはサーバーで暗号化して保存します。</p>
      <div class="row">
        <input type="password" class="grow mono" v-model="s.token" placeholder="skuaka_…" autocomplete="off">
        <button type="submit" class="primary" :disabled="s.busy">連携する</button>
      </div>
    </form>
  </section>
</template>

<style scoped>
.mono { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
</style>
