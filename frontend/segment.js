// 人物の切り抜き（MediaPipe の ImageSegmenter）を作る. 背景のぼかし・差し替え（Scala.js の Background）から、使うときだけ読み込む.
// WASM とモデルは自分のサーバーから配信する（会議中に外部へは通信しない）.
import { FilesetResolver, ImageSegmenter } from '@mediapipe/tasks-vision'

/* global __MEDIAPIPE_BASE__ */
export async function createSegmenter() {
  const fileset = await FilesetResolver.forVisionTasks(__MEDIAPIPE_BASE__)
  let last
  // GPU（WebGL）が使えなければ CPU で
  for (const delegate of ['GPU', 'CPU']) {
    try {
      return await ImageSegmenter.createFromOptions(fileset, {
        baseOptions: { modelAssetPath: '/models/selfie_segmenter_landscape.tflite', delegate },
        runningMode: 'VIDEO', outputCategoryMask: false, outputConfidenceMasks: true,
      })
    } catch (e) { last = e }
  }
  throw last
}
