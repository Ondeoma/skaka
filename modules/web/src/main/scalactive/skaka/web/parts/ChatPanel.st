<script lang="scala">
import skaka.web.*
import skaka.shared.*
import org.scalajs.dom

// 会議中のチャット（全員宛・個別のメッセージ）. 全員宛のものは会議の履歴に残る
case class State(text: RV[String] = RV(""), to: RV[String] = RV(""), list: ElementRef[dom.HTMLElement] = ElementRef())

def initState() = State()

def scroll(s: State): Unit = dom.window.setTimeout(() => s.list.foreach(e => e.scrollTop = e.scrollHeight.toDouble), 0)

def mounted(s: State): Unit = {
  Meeting.unread := 0
  scroll(s)
  watch(Meeting.chat)((_, _) => { Meeting.unread := 0; scroll(s) })
}

def send(s: State): Unit = {
  val t = s.text.v.trim
  if (t.nonEmpty) {
    Meeting.say(t, Option(s.to.v).filter(_.nonEmpty))
    s.text := ""
  }
}

def key(s: State, ev: dom.Event): Unit = {
  val e = ev.asInstanceOf[dom.KeyboardEvent]
  val composing = ev.asInstanceOf[scala.scalajs.js.Dynamic].isComposing.asInstanceOf[Boolean]
  if (e.key == "Enter" && !e.shiftKey && !composing) { e.preventDefault(); send(s) }
}

def mine(m: ChatMsg): Boolean = m.from == Meeting.selfId
def cls(m: ChatMsg): String = (if (mine(m)) "msg mine" else "msg") + (if (m.to.isDefined) " private" else "")
def head(m: ChatMsg): String = {
  val to = m.to.map(t => if (t == Meeting.selfId) "（あなたへ個別に）" else s"（${Meeting.peers.v.find(_.id == t).map(_.name).getOrElse("")} さんへ個別に）").getOrElse("")
  s"${if (mine(m)) "あなた" else m.name + (if (m.userId.isEmpty) "（ゲスト）" else "")}$to"
}
/** URL をリンクにする（ほかは文字のまま）. */
def parts(text: String): List[(String, Boolean)] = {
  val re = "https?://[^\\s<>\"]+".r
  var pos = 0
  val out = scala.collection.mutable.ListBuffer.empty[(String, Boolean)]
  re.findAllMatchIn(text).foreach { m =>
    if (m.start > pos) out += text.substring(pos, m.start) -> false
    out += m.matched -> true
    pos = m.end
  }
  if (pos < text.length) out += text.substring(pos) -> false
  out.toList
}
def empty = Meeting.chat.mapC(_.isEmpty)
</script>

<template>
  <div class="chat">
    <div class="list" ref="s.list">
      <p v-if="empty" class="muted small center">メッセージはまだありません。<br>全員宛のメッセージは会議の履歴に残ります。</p>
      <div v-for="m in Meeting.chat" :key="m.id" :class="cls(m)">
        <div class="meta"><b>{{ head(m) }}</b> <span class="muted">{{ Time.time(m.at) }}</span></div>
        <div class="text"><template v-for="p in parts(m.text)"><a v-if="p._2" :href="p._1" target="_blank" rel="noopener">{{ p._1 }}</a><template v-else>{{ p._1 }}</template></template></div>
      </div>
    </div>
    <form class="input" @submit.prevent="send(s)">
      <select v-model="s.to" class="to">
        <option value="">全員へ</option>
        <option v-for="p in Meeting.peers" :value="p.id">{{ p.name }} さんへ（個別）</option>
      </select>
      <div class="row">
        <textarea v-model="s.text" rows="2" maxlength="2000" placeholder="メッセージ（Enter で送信、Shift+Enter で改行）" @keydown="key(s, event)"></textarea>
        <button type="submit" class="primary">送信</button>
      </div>
    </form>
  </div>
</template>

<style scoped>
.chat { display: flex; flex-direction: column; height: 100%; min-height: 0; }
.list { flex: 1; overflow-y: auto; padding: .6rem; display: flex; flex-direction: column; gap: .5rem; }
.center { text-align: center; margin-top: 2rem; }
.msg { max-width: 92%; }
.msg.mine { align-self: flex-end; text-align: right; }
.meta { font-size: .72rem; }
.text { display: inline-block; text-align: left; white-space: pre-wrap; word-break: break-word; background: var(--hover); padding: .35rem .6rem; border-radius: 10px; font-size: .88rem; }
.mine .text { background: var(--accent-weak); }
.private .text { outline: 1px dashed var(--warn); }
.input { border-top: 1px solid var(--line); padding: .5rem; display: flex; flex-direction: column; gap: .35rem; }
.input .row { flex-wrap: nowrap; align-items: stretch; }
.input textarea { min-height: 2.6rem; flex: 1; resize: none; }
.to { font-size: .8rem; padding: .2rem .4rem; }
</style>
