<script lang="scala">
import skaka.web.*
import skaka.shared.*
import skaka.web.parts.*
import org.scalajs.dom

import java.util.UUID

// 会議の詳細: 議事録・要約（skakapo）、録画（skawau）、参加者（入った・出た時刻）、文字起こしとチャットの記録（テキストで書き出せる）
case class State(detail: RV[Option[MeetingDetailDto]] = RV(None), error: RV[String] = RV(""), timer: RV[Option[Int]] = RV(None),
                 asking: RV[Boolean] = RV(false))

def initState() = State()

def id: Option[UUID] = scala.util.Try(UUID.fromString(dom.window.location.pathname.stripPrefix("/meetings/"))).toOption

def mounted(s: State): Unit = {
  pageTitle("会議の記録")
  if (id.isEmpty) s.error := "会議が見つかりません" else load(s)
  // 録画中・保存中のもの、作っている議事録があれば、終わるまで読み直す
  s.timer := Some(dom.window.setInterval(() => if (recs(s).v.exists(r => r.status == RecordingStatus.Recording || r.status == RecordingStatus.Saving) ||
    mins(s).v.exists(_.status == MinutesStatus.Running)) load(s), 4000))
}

def unmounted(s: State): Unit = s.timer.v.foreach(dom.window.clearInterval)

def load(s: State): Unit = id.foreach(i => Api.meeting(i).onComplete {
  case scala.util.Success(d) => s.detail := Some(d)
  case scala.util.Failure(e) => s.error := e.getMessage
}(scala.scalajs.concurrent.JSExecutionContext.Implicits.queue))

def recs(s: State) = s.detail.mapC(_.toList.flatMap(_.recordings))
def retry(s: State, r: RecordingDto): Unit = Store.report(Api.retryRecording(r.id)) { _ => Store.toast("もう一度保存しています"); load(s) }
def remove(s: State, r: RecordingDto): Unit =
  Dialog.confirm(s"録画「${r.title}」を一覧から消しますか？" + (if (r.fileUrl.isDefined) s"（${AppNames.nameOf("skawau")} に保存したファイルは残ります. 消すときは ${AppNames.nameOf("skawau")} で消してください）" else ""), "消す") {
    Store.report(Api.deleteRecording(r.id))(_ => load(s))
  }
def size(r: RecordingDto): String = if (r.bytes >= 1048576) f"${r.bytes / 1048576.0}%.1f MB" else s"${r.bytes / 1024} KB"
def recSpan(r: RecordingDto): String =
  s"${Time.time(r.startedAt)}〜${r.endedAt.fold("")(Time.time)}" + r.endedAt.fold("")(e => s"（${Time.elapsed(Time.epoch(e) - Time.epoch(r.startedAt))}）")
def fileUrl(r: RecordingDto): String = r.fileUrl.getOrElse("")
def stClass(r: RecordingDto): String = s"badge st-${r.status}"
def sharedText(r: RecordingDto): String = if (r.shared > 0) s"・${r.shared}人と共有" else ""

def m(s: State) = s.detail.mapC(_.map(_.meeting))
def title(s: State) = m(s).mapC(_.fold("")(x => s"${x.roomName}（${Time.dateTime(x.startedAt)}〜${x.endedAt.fold("開催中")(Time.time)}）"))
def ps(s: State) = s.detail.mapC(_.toList.flatMap(_.participants))
def chat(s: State) = s.detail.mapC(_.toList.flatMap(_.chat))
def code(s: State) = m(s).mapC(_.map(_.roomCode).getOrElse(""))
def span(p: ParticipantDto): String = s"${Time.time(p.joinedAt)}〜${p.leftAt.fold("")(Time.time)}"

def lines(s: State) = s.detail.mapC(_.toList.flatMap(_.transcript))
def mins(s: State) = s.detail.mapC(_.toList.flatMap(_.minutes))

/** 参加者・文字起こし・チャットの記録をテキストに. */
def transcript(s: State): String = s.detail.v.fold("")(d => MeetingText.markdown(d, title(s).v, Time.time))

def save(text: String, name: String): Unit = {
  val blob = new dom.Blob(scala.scalajs.js.Array(text), new dom.BlobPropertyBag { `type` = "text/markdown" })
  val a = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
  a.href = dom.URL.createObjectURL(blob)
  a.asInstanceOf[scala.scalajs.js.Dynamic].download = name
  a.click()
}
def day(s: State): String = m(s).v.map(_.startedAt.take(10)).getOrElse("")
def download(s: State): Unit = save(transcript(s), s"meeting-${day(s)}.md")

// 議事録・要約（skakapo）
def canAsk(s: State) = computed { Store.skakapoEnabled && (lines(s).v.nonEmpty || chat(s).v.nonEmpty) }
def ask(s: State): Unit = id.foreach { i =>
  Dialog.confirm(s"${AppNames.nameOf("skakapo")} に議事録・要約を頼みますか？ 参加者・文字起こし・チャットを ${AppNames.nameOf("skakapo")} に渡し、あなたの ${AppNames.nameOf("skakapo")} の依頼として作ってもらいます" +
    s"（${AppNames.nameOf("skakapo")} のクレジットを使います）。", "頼む", danger = false) {
    Store.submit(s.asking)(Api.requestMinutes(i)) { _ => Store.toast(s"${AppNames.nameOf("skakapo")} に議事録を頼みました。できたらここに出ます"); load(s) }
  }
}
def refresh(s: State, x: MinutesDto): Unit = Store.report(Api.refreshMinutes(x.id)) { _ => Store.toast(s"${AppNames.nameOf("skakapo")} の返事を取り込みました"); load(s) }
def removeMinutes(s: State, x: MinutesDto): Unit =
  Dialog.confirm(s"この議事録を消しますか？（${AppNames.nameOf("skakapo")} の会話は残ります）", "消す")(Store.report(Api.deleteMinutes(x.id))(_ => load(s)))
def fileMinutes(s: State, x: MinutesDto): Unit = Store.report(Api.fileMinutes(x.id)) { _ => Store.toast(s"${AppNames.nameOf("skawau")} に保存しました"); load(s) }
def minFileUrl(x: MinutesDto): String = x.fileUrl.getOrElse("")
def minClass(x: MinutesDto): String = s"badge mn-${x.status}"
def minutesFile(s: State): String = s"minutes-${day(s)}.md"
</script>

<template>
  <div class="page narrow">
    <p><RouterLink to="/meetings">← 会議の履歴</RouterLink></p>
    <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
    <template v-if="s.detail.mapC(_.isDefined)">
      <h1>{{ title(s) }}</h1>
      <div class="row">
        <Link :to='"/r/" + code(s).v' cls="btn primary">会議室に入る</Link>
        <button @click='Dom.copy(transcript(s), "記録をコピーしました")'>記録をコピー</button>
        <button @click="download(s)">ダウンロード（.md）</button>
      </div>
      <section v-if="Store.skakapoEnabled" class="panel top">
        <div class="row head"><h2 class="grow">議事録・要約</h2>
          <button v-if="canAsk(s)" class="primary" :disabled="s.asking" @click="ask(s)">✨ {{ AppNames.of("skakapo") }} で議事録を作る</button></div>
        <p v-if="mins(s).mapC(_.isEmpty)" class="muted small">{{ if (canAsk(s).v) s"文字起こしとチャットをもとに、${AppNames.of("skakapo").v}（AI エージェント）が議事録と要約を作ります。設定の画面で ${AppNames.of("skakapo").v} と連携してから使ってください。" else "文字起こしもチャットもないため、議事録は作れません。会議中に主催者が「その他」→「文字起こしを始める」を押すと、文字起こしが残ります。" }}</p>
        <div v-for="x in mins(s)" :key="x.id" class="mn">
          <div class="row mhead">
            <span class="grow small muted">{{ x.by }}・{{ Time.dateTime(x.createdAt) }} <span :class="minClass(x)">{{ MinutesStatus.label(x.status) }}</span></span>
            <a v-if="x.fileUrl.isDefined" class="btn small primary" :href="minFileUrl(x)" target="_blank" rel="noopener" :title='"頼んだ人の " + AppNames.of("skawau").v + " の「skaka 議事録」に保存し、参加したメンバーと共有しています"'>{{ AppNames.of("skawau") }} で開く</a>
            <a class="btn small" :href="x.sessionUrl" target="_blank" rel="noopener" :title='AppNames.of("skakapo").v + " の会話で直してもらえます"'>{{ AppNames.of("skakapo") }} で開く</a>
            <button v-if="x.mine" class="small" @click="refresh(s, x)" :title='AppNames.of("skakapo").v + " で直してもらったあとに、最新の返事を取り込みます"'>取り込み直す</button>
            <button v-if='x.body.nonEmpty' class="small" @click='Dom.copy(x.body, "議事録をコピーしました")'>コピー</button>
            <button v-if='x.body.nonEmpty' class="small" @click="save(x.body, minutesFile(s))">.md</button>
            <button v-if="x.mine" class="small ghost danger" @click="removeMinutes(s, x)">消す</button>
          </div>
          <p v-if='x.status == "running" && x.body.isEmpty' class="muted small"><span class="spin"></span> {{ AppNames.of("skakapo") }} が作っています…（数分かかることがあります）</p>
          <p v-if="x.error.isDefined" class="error small">{{ x.error.getOrElse("") }}</p>
          <p v-if="x.fileError.isDefined" class="error small">{{ AppNames.of("skawau") }} に保存できませんでした: {{ x.fileError.getOrElse("") }}
            <button v-if="x.mine" class="small" @click="fileMinutes(s, x)">{{ AppNames.of("skawau") }} に保存</button></p>
          <div v-if="x.body.nonEmpty" class="md">{{ x.body }}</div>
        </div>
      </section>
      <section v-if="recs(s).mapC(_.nonEmpty)" class="panel top">
        <h2>録画</h2>
        <div v-for="r in recs(s)" :key="r.id" class="r">
          <div class="grow">
            <div><b>🎬 {{ r.title }}</b> <span :class="stClass(r)">{{ RecordingStatus.label(r.status) }}</span></div>
            <div class="muted small">{{ r.by }}・{{ recSpan(r) }}・{{ size(r) }}{{ sharedText(r) }}</div>
            <div v-if="r.error.isDefined" class="error small">{{ r.error.getOrElse("") }}</div>
          </div>
          <a v-if="r.fileUrl.isDefined" class="btn primary" :href="fileUrl(r)" target="_blank" rel="noopener">{{ AppNames.of("skawau") }} で開く</a>
          <button v-if='r.mine && r.status == "failed"' @click="retry(s, r)">もう一度保存</button>
          <button v-if="r.mine" class="ghost danger" title="一覧から消す" @click="remove(s, r)">消す</button>
        </div>
      </section>
      <section class="panel top">
        <h2>参加者</h2>
        <div v-for="p in ps(s)" class="p"><span class="grow">{{ p.name }}<span v-if="p.guest" class="muted small">（ゲスト）</span></span>
          <span class="muted small">{{ span(p) }}</span></div>
      </section>
      <section v-if="lines(s).mapC(_.nonEmpty)" class="panel">
        <h2>文字起こし</h2>
        <div v-for="t in lines(s)" :key="t.id" class="c"><span class="muted small">{{ Time.time(t.at) }}</span> <b>{{ t.name }}</b>
          <div class="t">{{ t.text }}</div></div>
      </section>
      <section class="panel">
        <h2>チャット</h2>
        <p v-if="chat(s).mapC(_.isEmpty)" class="muted small">チャットはありません。</p>
        <div v-for="c in chat(s)" :key="c.id" class="c"><span class="muted small">{{ Time.time(c.at) }}</span> <b>{{ c.name }}</b>
          <div class="t">{{ c.text }}</div></div>
      </section>
    </template>
  </div>
</template>

<style scoped>
.top { margin-top: 1rem; }
.r { display: flex; gap: .6rem; align-items: center; padding: .45rem 0; border-bottom: 1px solid var(--line); flex-wrap: wrap; }
.st-recording { background: color-mix(in srgb, #ef4444 18%, transparent); color: #ef4444; }
.st-saving { background: color-mix(in srgb, var(--accent, #2563eb) 18%, transparent); color: var(--accent, #2563eb); }
.st-saved { background: color-mix(in srgb, var(--ok) 18%, transparent); color: var(--ok); }
.st-failed { background: color-mix(in srgb, var(--danger) 18%, transparent); color: var(--danger); }
.head { align-items: center; gap: .6rem; }
.head h2 { margin: 0; }
.mn { border-top: 1px solid var(--line); padding: .6rem 0; }
.mhead { align-items: center; gap: .35rem; flex-wrap: wrap; }
.md { white-space: pre-wrap; word-break: break-word; line-height: 1.6; margin-top: .4rem; padding: .7rem .9rem; border-radius: 10px; background: var(--hover); }
.mn-running { background: color-mix(in srgb, var(--accent, #2563eb) 18%, transparent); color: var(--accent, #2563eb); }
.mn-done { background: color-mix(in srgb, var(--ok) 18%, transparent); color: var(--ok); }
.mn-failed { background: color-mix(in srgb, var(--danger) 18%, transparent); color: var(--danger); }
.spin { display: inline-block; width: .8rem; height: .8rem; border: 2px solid var(--line); border-top-color: var(--accent); border-radius: 50%;
  vertical-align: -.1rem; animation: spin 1s linear infinite; }
@keyframes spin { to { transform: rotate(360deg) } }
.p { display: flex; gap: .6rem; padding: .3rem 0; border-bottom: 1px solid var(--line); }
.c { padding: .35rem 0; }
.t { white-space: pre-wrap; word-break: break-word; }
</style>
