<script lang="scala">
import skaka.web.*
import skaka.shared.*

// API トークンでの連携（skuaka・skakapo）: そのサービスの API トークンを登録する
case class Props(service: String, title: String, about: String, writeNeed: String)

case class State(link: RV[Option[TokenLinkDto]] = RV(None), token: RV[String] = RV(""), busy: RV[Boolean] = RV(false))

def initState() = State()

def mounted(s: State, props: Props): Unit = Store.report(Api.tokenLink(props.service))(l => s.link := Some(l))

def save(props: Props, s: State): Unit =
  Store.submit(s.busy)(Api.linkToken(props.service, s.token.v.trim)) { l =>
    s.link := Some(l)
    s.token := ""
    Store.toast(s"${props.service}（${l.account.getOrElse("")}）と連携しました")
  }

def unlink(props: Props, s: State): Unit = Dialog.confirm(s"${props.service} との連携をやめますか？（登録した API トークンを消します）", "やめる") {
  Store.report(Api.unlinkToken(props.service))(_ => Store.report(Api.tokenLink(props.service))(l => s.link := Some(l)))
}

def linked(s: State) = s.link.mapC(_.exists(_.linked))
def status(props: Props, s: State) = s.link.mapC(_.fold("")(l => s"${l.account.getOrElse("")} さんとして連携しています（${l.prefix.getOrElse("")}…・" +
  (if (l.canWrite) "読み書き" else s"参照のみ. ${props.writeNeed}には読み書きのトークンが必要です") + "）"))
def settingsUrl(s: State) = s.link.mapC(_.flatMap(_.url).getOrElse("") + "/settings")
</script>

<template>
  <section class="panel">
    <h2>{{ props.title }}</h2>
    <p class="small">{{ props.about }}</p>
    <template v-if="linked(s)">
      <p class="small ok">{{ status(props, s) }}</p>
      <button class="danger" @click="unlink(props, s)">連携をやめる</button>
    </template>
    <form v-else @submit.prevent="save(props, s)">
      <p class="small muted">{{ props.service }} の <a :href="settingsUrl(s).v" target="_blank" rel="noopener">設定の画面</a> の「API トークンと MCP」で、
        「読み書き」の API トークンを作り、ここに貼り付けてください。トークンはサーバーで暗号化して保存します。</p>
      <div class="row">
        <input type="password" class="grow mono" v-model="s.token" :placeholder='props.service + "_…"' autocomplete="off">
        <button type="submit" class="primary" :disabled="s.busy">連携する</button>
      </div>
    </form>
  </section>
</template>

<style scoped>
.mono { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
</style>
