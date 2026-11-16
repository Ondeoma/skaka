<script lang="scala">
import org.scalajs.dom

// クラスを付けられる画面の中のリンク（RouterLink はクラスを受け取らないため）. Ctrl・⌘ を押したクリックは新しいタブで開く
case class Props(to: String, cls: String = "", title: String = "", slot: Slot = Slot.empty)

def go(e: dom.Event, props: Props): Unit = e match {
  case m: dom.MouseEvent if m.button == 0 && !m.ctrlKey && !m.metaKey && !m.shiftKey && !m.altKey =>
    m.preventDefault()
    Router.go(props.to)
  case _ => ()
}
</script>

<template>
  <a :href="props.to" :class="props.cls" :title="props.title" @click="go(event, props)"><slot></slot></a>
</template>
