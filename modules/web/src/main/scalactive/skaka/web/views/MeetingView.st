<script lang="scala">
import skaka.web.*
import skaka.shared.*
import skaka.web.parts.*
import org.scalajs.dom

import java.util.UUID

// 会議の詳細: 録画（skawau）、参加者（入った・出た時刻）とチャットの記録（テキストで書き出せる）
case class State(detail: RV[Option[MeetingDetailDto]] = RV(None), error: RV[String] = RV(""), timer: RV[Option[Int]] = RV(None))

def initState() = State()

def id: Option[UUID] = scala.util.Try(UUID.fromString(dom.window.location.pathname.stripPrefix("/meetings/"))).toOption

def mounted(s: State): Unit = {
  pageTitle("会議の記録")
  if (id.isEmpty) s.error := "会議が見つかりません" else load(s)
  // 録画中・保存中のものがあれば、終わるまで読み直す
  s.timer := Some(dom.window.setInterval(() => if (recs(s).v.exists(r => r.status == RecordingStatus.Recording || r.status == RecordingStatus.Saving)) load(s), 4000))
}

def unmounted(s: State): Unit = s.timer.v.foreach(dom.window.clearInterval)

def load(s: State): Unit = id.foreach(i => Api.meeting(i).onComplete {
  case scala.util.Success(d) => s.detail := Some(d)
  case scala.util.Failure(e) => s.error := e.getMessage
}(scala.scalajs.concurrent.JSExecutionContext.Implicits.queue))

def recs(s: State) = s.detail.mapC(_.toList.flatMap(_.recordings))
def retry(s: State, r: RecordingDto): Unit = Store.report(Api.retryRecording(r.id)) { _ => Store.toast("もう一度保存しています"); load(s) }
def remove(s: State, r: RecordingDto): Unit =
  Dialog.confirm(s"録画「${r.title}」を一覧から消しますか？" + (if (r.fileUrl.isDefined) "（skawau に保存したファイルは残ります. 消すときは skawau で消してください）" else ""), "消す") {
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

/** チャットの記録をテキストに. */
def transcript(s: State): String = s.detail.v.fold("") { d =>
  (s"# ${title(s).v}" :: "" :: "## 参加者" :: d.participants.map(p => s"- ${p.name}${if (p.guest) "（ゲスト）" else ""} ${span(p)}") :::
    "" :: "## チャット" :: d.chat.map(c => s"[${Time.time(c.at)}] ${c.name}: ${c.text}")).mkString("\n")
}

def download(s: State): Unit = {
  val blob = new dom.Blob(scala.scalajs.js.Array(transcript(s)), new dom.BlobPropertyBag { `type` = "text/markdown" })
  val a = dom.document.createElement("a").asInstanceOf[dom.HTMLAnchorElement]
  a.href = dom.URL.createObjectURL(blob)
  a.asInstanceOf[scala.scalajs.js.Dynamic].download = s"meeting-${m(s).v.map(_.startedAt.take(10)).getOrElse("")}.md"
  a.click()
}
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
      <section v-if="recs(s).mapC(_.nonEmpty)" class="panel top">
        <h2>録画</h2>
        <div v-for="r in recs(s)" :key="r.id" class="r">
          <div class="grow">
            <div><b>🎬 {{ r.title }}</b> <span :class="stClass(r)">{{ RecordingStatus.label(r.status) }}</span></div>
            <div class="muted small">{{ r.by }}・{{ recSpan(r) }}・{{ size(r) }}{{ sharedText(r) }}</div>
            <div v-if="r.error.isDefined" class="error small">{{ r.error.getOrElse("") }}</div>
          </div>
          <a v-if="r.fileUrl.isDefined" class="btn primary" :href="fileUrl(r)" target="_blank" rel="noopener">skawau で開く</a>
          <button v-if='r.mine && r.status == "failed"' @click="retry(s, r)">もう一度保存</button>
          <button v-if="r.mine" class="ghost danger" title="一覧から消す" @click="remove(s, r)">消す</button>
        </div>
      </section>
      <section class="panel top">
        <h2>参加者</h2>
        <div v-for="p in ps(s)" class="p"><span class="grow">{{ p.name }}<span v-if="p.guest" class="muted small">（ゲスト）</span></span>
          <span class="muted small">{{ span(p) }}</span></div>
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
.p { display: flex; gap: .6rem; padding: .3rem 0; border-bottom: 1px solid var(--line); }
.c { padding: .35rem 0; }
.t { white-space: pre-wrap; word-break: break-word; }
</style>
