import './style.css'
// 人物の切り抜き（背景のぼかし・差し替え）は使うときだけ読み込む（WASM が大きいため）
window.skakaSegmenter = () => import('./segment.js').then(m => m.createSegmenter())
import 'scalajs:main.js'
