<script lang="scala">
import skaka.web.*

// カメラ・マイク・スピーカーの選択、送る映像・音声の品質（この端末に保存. 会議中もすぐ反映）、入退室の音
case class Props(onClose: Unit => Unit, title: String = "カメラ・マイクの設定")

def list(kind: String) = Meeting.devices.mapC(_.filter(_._1 == kind).map(d => d._2 -> (if (d._3.nonEmpty) d._3 else s"${d._2.take(8)}…")))
def current(kind: String): String = Prefs.get(kind, "")
def speakerSupported: Boolean = !scala.scalajs.js.isUndefined(org.scalajs.dom.document.createElement("audio").asInstanceOf[scala.scalajs.js.Dynamic].setSinkId)
def mounted(): Unit = Meeting.refreshDevices()

def info = computed { Meeting.streams.v; Meeting.videoInfo }

/** 背景の選択肢: (id, 名前, 見本の画像（なければ空）). */
def bgs = computed {
  List(("none", "なし", ""), ("blur", "ぼかし", ""), ("blur-strong", "強くぼかす", "")) ++
    Background.presets.map(p => (p._1, p._2, p._3)) ++ Background.custom.v.map(u => ("custom", "自分の画像", u)).toList
}
def bgClass(id: String) = Background.mode.mapC(m => if (m == id) "bg on" else "bg")
def bgStyle(img: String): String = if (img.nonEmpty) s"background-image: url('$img')" else ""
def pick(e: org.scalajs.dom.Event): Unit = {
  val in = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement]
  if (in.files.length > 0) Background.upload(in.files(0))
  in.value = ""
}
def int(e: org.scalajs.dom.Event): Int = Dom.value(e).toIntOption.getOrElse(0)
def bitrate = computed { Quality.resolution.v; Quality.fps.v; Quality.video.v; s"相手1人あたり最大 ${Quality.videoBitrate(1) / 1000} kbps（人数が増えると自動で下げます）" }
</script>

<template>
  <Modal :onClose="props.onClose">
    <h2>{{ props.title }}</h2>
    <label class="field"><span>マイク</span>
      <select :value='current("audioinput")' @change='Meeting.setDevice("audioinput", Dom.value(event))'>
        <option value="">既定</option>
        <option v-for='d in list("audioinput")' :value="d._1">{{ d._2 }}</option>
      </select></label>
    <p v-if="Meeting.micError.mapC(_.nonEmpty)" class="error small">マイク: {{ Meeting.micError }}</p>
    <label class="field"><span>カメラ</span>
      <select :value='current("videoinput")' @change='Meeting.setDevice("videoinput", Dom.value(event))'>
        <option value="">既定</option>
        <option v-for='d in list("videoinput")' :value="d._1">{{ d._2 }}</option>
      </select></label>
    <p v-if="Meeting.camError.mapC(_.nonEmpty)" class="error small">カメラ: {{ Meeting.camError }}</p>
    <label v-if="speakerSupported" class="field"><span>スピーカー</span>
      <select :value='current("audiooutput")' @change='Meeting.setDevice("audiooutput", Dom.value(event))'>
        <option value="">既定</option>
        <option v-for='d in list("audiooutput")' :value="d._1">{{ d._2 }}</option>
      </select></label>
    <h3>背景 <span v-if="Background.loading" class="muted small">（読み込み中…）</span></h3>
    <div class="bgs">
      <button v-for="b in bgs" type="button" :class="bgClass(b._1).v" :style="bgStyle(b._3)" :title="b._2" @click="Background.set(b._1)">
        <span v-if='b._3.isEmpty' class="ic">{{ if (b._1 == "none") "🚫" else if (b._1 == "blur") "💧" else "🌫" }}</span>
        <span class="lb">{{ b._2 }}</span></button>
      <label class="bg add" title="画像を選ぶ">＋<span class="lb">画像を選ぶ</span>
        <input type="file" accept="image/*" @change="pick(event)"></label>
    </div>
    <p class="small muted">人物を切り抜いて背景をぼかす・差し替えます。処理はこの端末の中で行い、映像を外に送ることはありません（初回は数秒かかります）。</p>
    <h3>送る映像の品質</h3>
    <div class="grid2">
      <label class="field"><span>カメラの解像度</span>
        <select :value="Quality.resolution.v.toString" @change='Quality.setInt(Quality.resolution, "q.res", int(event), "video")'>
          <option v-for="o in Quality.resolutions" :value="o._1.toString">{{ o._2 }}</option>
        </select></label>
      <label class="field"><span>フレームレート</span>
        <select :value="Quality.fps.v.toString" @change='Quality.setInt(Quality.fps, "q.fps", int(event), "video")'>
          <option v-for="o in Quality.frameRates" :value="o._1.toString">{{ o._2 }}</option>
        </select></label>
      <label class="field"><span>画質（送る量）</span>
        <select :value="Quality.video" @change='Quality.setStr(Quality.video, "q.video", Dom.value(event), "bitrate")'>
          <option v-for="o in Quality.videoModes" :value="o._1">{{ o._2 }}</option>
        </select></label>
      <label class="field"><span>画面共有</span>
        <select :value="Quality.screen" @change='Quality.setStr(Quality.screen, "q.screen", Dom.value(event), "screen")'>
          <option v-for="o in Quality.screenModes" :value="o._1">{{ o._2 }}</option>
        </select></label>
    </div>
    <p class="small muted">{{ bitrate }}<span v-if="info.mapC(_.nonEmpty)">・いまのカメラ: {{ info }}</span></p>
    <p class="small muted">カメラが対応していない解像度・フレームレートは、近いものになります。</p>
    <h3>送る音声</h3>
    <label class="row"><input type="checkbox" :checked="Quality.echo" @change='Quality.setBool(Quality.echo, "q.echo", Dom.checked(event), "audio")'> エコーを除く（スピーカーで聞くとき）</label>
    <label class="row"><input type="checkbox" :checked="Quality.noise" @change='Quality.setBool(Quality.noise, "q.noise", Dom.checked(event), "audio")'> 雑音を抑える</label>
    <label class="row"><input type="checkbox" :checked="Quality.gain" @change='Quality.setBool(Quality.gain, "q.gain", Dom.checked(event), "audio")'> 音量を自動で調整する</label>
    <label class="row"><input type="checkbox" :checked="Quality.hifi" @change='Quality.setBool(Quality.hifi, "q.hifi", Dom.checked(event), "audio")'> 高音質（128kbps. 楽器・音楽を流すときは、上の3つを外すと原音に近くなります）</label>
    <h3>その他</h3>
    <label class="row"><input type="checkbox" :checked="Sound.enabled" @change="Sound.set(Dom.checked(event))"> 入退室・チャットの音を鳴らす</label>
    <div class="actions"><button class="primary" @click="props.onClose(())">閉じる</button></div>
  </Modal>
</template>

<style scoped>
h3 { font-size: .92rem; margin: 1rem 0 .4rem; padding-top: .6rem; border-top: 1px solid var(--line); }
.grid2 { display: grid; grid-template-columns: 1fr 1fr; gap: 0 .8rem; }
.bgs { display: grid; grid-template-columns: repeat(auto-fill, minmax(92px, 1fr)); gap: .45rem; }
.bg { position: relative; aspect-ratio: 16 / 9; border-radius: 8px; border: 2px solid var(--line); background: var(--hover) center / cover no-repeat;
  display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 0; cursor: pointer; overflow: hidden; color: var(--fg); }
.bg.on { border-color: var(--accent, #2563eb); box-shadow: 0 0 0 2px color-mix(in srgb, var(--accent, #2563eb) 35%, transparent); }
.bg .ic { font-size: 1.1rem; }
.bg .lb { position: absolute; left: 0; right: 0; bottom: 0; font-size: .68rem; background: rgba(0, 0, 0, .45); color: #fff; padding: .05rem 0; }
.bg.add { font-size: 1.3rem; color: var(--muted); }
.bg.add input { display: none; }
@media (max-width: 520px) { .grid2 { grid-template-columns: 1fr; } }
</style>
