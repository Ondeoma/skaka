<script lang="scala">
import skaka.web.*
import skaka.shared.*
import org.scalajs.dom

// skawau（ファイル共有）との連携: 会議の録画の保存先. skawau の許可の画面（OAuth）で許可する
case class State(link: RV[Option[SkawauLinkDto]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = {
  // 許可の画面から戻ってきたとき（?skawau=…）
  val q = dom.window.location.search
  if (q.contains("skawau=")) {
    val r = new dom.URLSearchParams(q).get("skawau")
    if (r == "connected") Store.toast(s"${AppNames.nameOf("skawau")} と連携しました。会議の録画を ${AppNames.nameOf("skawau")} に保存できます")
    else if (r == "denied") Store.toast(s"${AppNames.nameOf("skawau")} との連携を取りやめました")
    else if (r == "readonly") Store.toast(s"${AppNames.nameOf("skawau")} が書き込みを許可していないため、連携できませんでした（${AppNames.nameOf("skawau")} の管理者に確認してください）", error = true)
    else Store.toast(s"${AppNames.nameOf("skawau")} と連携できませんでした。もう一度試してください", error = true)
    dom.window.history.replaceState(null, "", dom.window.location.pathname)
  }
  load(s)
}

def load(s: State): Unit = Store.report(Api.skawau())(l => s.link := Some(l))

def connect(): Unit = dom.window.location.href = Api.connectSkawauUrl("/settings")

def unlink(s: State): Unit = Dialog.confirm(s"${AppNames.nameOf("skawau")} との連携をやめますか？（${AppNames.nameOf("skawau")} に保存した録画はそのまま残ります）", "やめる") {
  Store.report(Api.unlinkSkawau())(_ => load(s))
}

def linked(s: State) = s.link.mapC(_.exists(_.linked))
def since(s: State) = s.link.mapC(_.flatMap(_.since).fold("")(t => s"${Time.dateTime(t)} から連携しています"))
def url(s: State) = s.link.mapC(_.flatMap(_.url).getOrElse(""))
</script>

<template>
  <section class="panel">
    <h2>{{ AppNames.of("skawau") }}（ファイル共有）との連携</h2>
    <p class="small">会議の録画は {{ AppNames.of("skawau") }} に保存します。主催者が会議中に「その他」→「録画する」を押すと録画が始まり、録り終えると、
      あなたの {{ AppNames.of("skawau") }} の「skaka 録画」フォルダに置いて、会議に参加したメンバーと共有します（閲覧のみ）。</p>
    <template v-if="linked(s)">
      <p class="small ok">{{ since(s) }}（<a :href="url(s).v" target="_blank" rel="noopener">{{ AppNames.of("skawau") }} を開く</a>）</p>
      <button class="danger" @click="unlink(s)">連携をやめる</button>
    </template>
    <template v-else>
      <p class="small muted">{{ AppNames.of("skawau") }} の画面で、{{ AppNames.self }} がファイルを置くことを許可します（あなたの {{ AppNames.of("skawau") }} の権限で、ファイルの作成と共有を行います）。</p>
      <button class="primary" @click="connect()">{{ AppNames.of("skawau") }} と連携する</button>
    </template>
  </section>
</template>
