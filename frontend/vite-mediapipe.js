import fs from 'node:fs'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

/**
 * MediaPipe（人物の切り抜き. 背景のぼかし・差し替えに使う）の WASM を、自分のサーバーから配信する Vite プラグイン.
 * 開発中は node_modules から返し、ビルドでは dist に含める. 版をパスに入れて、版を上げたら読み直させる.
 * 背景の機能を使った人だけが初回に読み込む（frontend/segment.js を動的 import）.
 */
// package.json は exports に入っていないため、node_modules の場所から読む
const pkgDir = path.join(path.dirname(fileURLToPath(import.meta.url)), 'node_modules', '@mediapipe', 'tasks-vision')
const version = JSON.parse(fs.readFileSync(path.join(pkgDir, 'package.json'), 'utf8')).version
const files = ['vision_wasm_internal.js', 'vision_wasm_internal.wasm']
export const mediapipeBase = `/assets/mediapipe-${version}`

export default function mediapipe() {
  return {
    name: 'skaka:mediapipe',
    config: () => ({ define: { __MEDIAPIPE_BASE__: JSON.stringify(mediapipeBase) } }),
    configureServer(server) {
      server.middlewares.use(mediapipeBase, (req, res, next) => {
        const name = (req.url || '').split('?')[0].replace(/^\//, '')
        if (!files.includes(name)) return next()
        res.setHeader('Content-Type', name.endsWith('.wasm') ? 'application/wasm' : 'text/javascript')
        fs.createReadStream(path.join(pkgDir, 'wasm', name)).pipe(res)
      })
    },
    generateBundle() {
      for (const name of files)
        this.emitFile({ type: 'asset', fileName: `${mediapipeBase.slice(1)}/${name}`, source: fs.readFileSync(path.join(pkgDir, 'wasm', name)) })
    },
  }
}
