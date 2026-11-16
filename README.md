# skaka

<p align="center"><img src="frontend/public/logo.svg" width="112" height="112" alt="skaka のロゴ"></p>

skaka は、チームのための Web 会議システムです（ブラウザだけで使えます. アプリのインストールは要りません）。ログインは OpenID Connect（OIDC）。ライセンス: MIT

- **会議室**: URL（`/r/abc-defg-hij`）で入る. 1人に1つの「自分の会議室」（ずっと同じ URL）と、定例会議などのための会議室を作れる.
  ホームから「すぐに会議を始める」、コード・URL を入れて入る、最近入った会議室
- **会議**: カメラ・マイク（オン・オフ、機器の選択、スピーカーの選択）、画面共有（音声付きのタブも）、
  ギャラリー（全員を並べる）とスピーカー（話している人・画面共有を大きく）の表示、枠の固定・全画面、話している人の枠を光らせる、
  挙手、リアクション（👍 👏 😂 🎉 など）、チャット（全員宛・個別. URL はリンクに）、参加者の一覧、経過時間、入退室の音、
  入る前にカメラ・マイクを確かめる画面、キーボードの操作（`m` マイク・`v` カメラ・`h` 挙手・`c` チャット・`p` 参加者・`g` 表示）
- **送る映像・音声の品質**（カメラ・マイクの設定. 端末ごとに保存し、会議中もすぐ反映）: カメラの解像度（360p〜1080p）・フレームレート（15〜30fps）・
  画質（節約・自動・高画質）、画面共有（文字をくっきり・動きをなめらかに）、マイクのエコー除去・雑音抑制・音量の自動調整、高音質（128kbps）
- **背景のぼかし・差し替え**（カメラ・マイクの設定. 端末ごとに保存）: なし・ぼかし・強くぼかす・用意した画像（部屋・青空・紺・skaka）・自分の画像.
  人物の切り抜き（MediaPipe）は端末の中で行い、映像を外に送らない. 初回は切り抜きの WASM（約14MB）を読み込む. WebGL2 が使えるブラウザで使える
- **主催者**（会議室の持ち主と、主催者にした人）: マイクをオフにする（1人・全員）、挙手を下ろす、主催者にする・外す、退出させる、
  会議のロック（新しく入れなくする）、全員の会議を終了
- **待機室**: ゲスト（ログインしない人）は待機室で待ち、主催者（いなければチームのメンバー）が許可してから入る.
  会議室ごとに、メンバーも待機室で待たせる設定ができる
- **ゲスト**: 会議室ごとに、ログインしない人も URL から入れるようにできる（名前を入れて待機室へ）
- **会議の履歴**: いつ・誰が入って出たか、全員宛のチャットの記録（コピー・Markdown でダウンロード）
- **skuaka（カレンダー）との連携**: 設定で skuaka の API トークンを登録すると
  - ホームに今後7日の予定が並び、会議室のある予定は「参加」から入れる（予定の場所に skaka の URL がある予定）
  - 「会議の予定を立てる」で、会議室を作って skuaka に予定を作り、参加者を招待する（場所に会議室の URL が入る）
  - 既存の予定に「会議室を付ける」（予定の場所に会議室の URL を足す. 繰り返しの予定もそのまま）
  - 逆向き: skuaka で予定を登録するときに「Web 会議（skaka）を付ける」. skuaka が OAuth で skaka を使い、会議室を作って場所に URL を入れる
    （取り決めは [docs/skuaka-integration.md](docs/skuaka-integration.md)）
- **録画（保存先は skawau）**: 主催者が会議中に「その他」→「録画する」. 参加者全員に「録画中」と表示する.
  録画する人のブラウザが全員の映像（画面共有があれば大きく）と音を1つにまとめて録り、録り終えると、その人の skawau の「skaka 録画」フォルダに置いて、
  会議に参加したメンバーと共有する（閲覧のみ）. 会議の記録の画面から skawau で開ける. skawau とは設定の画面で OAuth で連携する
- **API トークンと MCP**: 設定の画面で作成・失効（読み書き / 参照のみ、有効期限）. `Authorization: Bearer skaka_…` で画面と同じ REST API を呼べる.
  `POST /mcp`（Streamable HTTP）で、MCP に対応した AI アシスタントから会議室・会議の記録・会議の予定を扱える
- 画面の色（自動・ライト・ダーク）、スマホの幅

技術: Scala 3.3 LTS、Cats Effect 3、http4s 1.0（マイルストーン版. WebSocket）、doobie、Flyway、PostgreSQL 17、
Scala.js + [Scalactive](https://github.com/Ondeoma/scalactive)、Vite、WebRTC（メッシュ）。設計は [docs/design.md](docs/design.md) を参照してください。

兄弟のシステム: skea（タスク管理）、skiwi（Wiki）、skawau（ファイル共有）、skuaka（カレンダー）。

## 構成

| モジュール | 内容 |
|---|---|
| `modules/shared` | バックエンドと画面（Scala.js）で共有する API の型と、会議中のやり取り（WebSocket）の型（circe） |
| `modules/core` | 会議の状態の変化（入る・待機室・主催者の操作・チャット. 純粋な関数）と、予定から会議室の URL を探す処理 |
| `modules/db` | doobie のストア、Flyway のマイグレーション |
| `modules/auth` | OIDC（Authorization Code + PKCE + nonce、ID トークンの署名の検証（JWKS）、IdP からのログアウト）、暗号化 |
| `modules/devidp` | 開発・テスト用の簡易 OIDC プロバイダ（本番では使わない） |
| `modules/api` | http4s のルート（認証・会議室・会議の WebSocket・履歴・skuaka・skawau との連携・録画・MCP） |
| `modules/app` | 起動・設定（環境変数）・画面の配信 |
| `modules/web` | 画面（Scalactive の `.st` 単一ファイルコンポーネント）と WebRTC（`Meeting.scala`） |
| `frontend/` | Vite（Scala.js の出力をまとめてビルドする） |

## 開発

必要なもの: JDK 21、sbt、Node.js、Docker。

```bash
docker compose -f docker-compose.dev.yml up -d --wait   # PostgreSQL（:54350. テスト用の DB skaka_test も作る）
scripts/dev-restart.sh                                   # バックエンドを :8096 で起動（DEV_LOGIN=true. 名前だけでログインできる）
(cd frontend && npm install && npm run dev)              # 画面の開発サーバー http://localhost:5177（/api・/auth・/oauth・/ws・/mcp は :8096 へ中継）
sbt ~web/fastLinkJS                                      # 画面の変更を再リンクし続ける（ブラウザを再読み込みすると反映）
```

カメラ・マイク・画面共有は、ブラウザが「安全な場所」とみなすページ（`https://` か `http://localhost`）でだけ使えます。
同じパソコンの2つのタブ（別のブラウザのプロファイル）で入ると、2人の会議を試せます。

**OIDC で試す**: 開発用の IdP（:9096）を起動し、バックエンドを OIDC の設定で起動し直します。

```bash
scripts/dev-idp.sh
OIDC=dev scripts/dev-restart.sh
```

**skuaka と試す**: skuaka の開発環境（画面 :5175）を起動しておくと、`SKUAKA_URL=http://localhost:5175`（`scripts/dev-env.sh` の既定）で連携できます。
skuaka の設定の画面で「読み書き」の API トークンを作り、skaka の設定の画面に貼り付けてください。

**skawau と試す（録画）**: skawau の開発環境（画面 :5176）を起動しておくと、`SKAWAU_URL=http://localhost:5176`（`scripts/dev-env.sh` の既定）で連携できます。
skawau 側の `scripts/dev-env.sh` の `OAUTH_CLIENTS` に skaka（`read write`）が登録してあります。skaka の設定の画面で「skawau と連携する」を押して許可してください。

テスト（会議の状態の変化・WebM の長さの補正・OIDC の検証・開発用 IdP と偽の skuaka・skawau を使った通しのテスト. DB は docker-compose.dev.yml）:

```bash
sbt test
```

**DB マイグレーション**（`modules/db/src/main/resources/db/migration`）: 公開の前に `V1__schema.sql` の1本にまとめてあります。
スキーマを変えるときは新しい番号の `V<n>__*.sql` を追加し、適用済みのファイルは書き換えないでください。

## 配布・起動

```bash
scripts/dist.sh                   # dist/lib/*.jar と dist/static（画面）を作る
java -cp 'dist/lib/*' skaka.app.Main
```

Docker: `scripts/dist.sh` のあと `docker build -f deploy/docker/app.Dockerfile -t skaka .`

環境変数（すべては `modules/app/src/main/scala/skaka/app/Main.scala` の `Config`）:

| 変数 | 内容 |
|---|---|
| `DATABASE_URL` / `DATABASE_USER` / `DATABASE_PASSWORD` | PostgreSQL（`jdbc:postgresql://…`） |
| `PUBLIC_URL` | 公開 URL（`https://` なら Cookie に Secure が付く. 会議室の URL にも使う） |
| `OIDC_ISSUER` / `OIDC_CLIENT_ID` / `OIDC_CLIENT_SECRET` | OIDC. IdP には Redirect URI `${PUBLIC_URL}/auth/callback`、ログアウト後の戻り先 `${PUBLIC_URL}/` を登録する |
| `OIDC_SCOPES` | 要求するスコープ（既定 `openid profile email`） |
| `OIDC_GROUPS_CLAIM` / `OIDC_ALLOWED_GROUPS` | 所属グループのクレーム名（既定 `groups`）と、ログインを許すグループ（カンマ区切り. 未設定ならだれでも） |
| `SESSION_DAYS` | ログインの有効日数（既定 14） |
| `ICE_SERVERS` | WebRTC の STUN・TURN（JSON. 既定 `[{"urls":["stun:stun.l.google.com:19302"]}]`） |
| `TURN_URLS` / `TURN_SECRET` / `TURN_TTL_HOURS` | TURN サーバー（coturn の `use-auth-secret`）. 参加者ごとに一時的な認証情報を作る（既定 12 時間有効） |
| `SKUAKA_URL` | 連携する skuaka の URL（未設定なら連携の機能を出さない） |
| `SECRET_KEY` | skuaka・skawau のトークンを DB に暗号化して保存する鍵（変えると連携し直しが必要） |
| `SKAWAU_URL` / `SKAWAU_CLIENT_ID` / `SKAWAU_CLIENT_SECRET` | 録画の保存先の skawau（3つとも設定したときだけ録画できる）. skawau の `OAUTH_CLIENTS` に、同じ id・secret と `redirectUri` = `${PUBLIC_URL}/api/integrations/skawau/callback`、`scopes` = `["read","write"]` を登録する |
| `SKAWAU_INTERNAL_URL` | このサーバーから見た skawau の URL（既定は `SKAWAU_URL`） |
| `RECORDING_DIR` | 録画中の一時ファイルを置くディレクトリ（既定は OS の一時ディレクトリの `skaka-recordings`. 1時間でおよそ 0.5〜1GB） |
| `RECORDING_MAX_MB` | 1つの録画の上限（既定 8192） |
| `OAUTH_CLIENTS` | skaka を使う外部のサービス（skuaka）. `[{"id":"skuaka","secret":"…（16文字以上）","redirectUri":"https://…/api/integrations/skaka/callback","name":"skuaka","scopes":["read","write"]}]` |
| `HTTP_PORT` | 待ち受けるポート（既定 8080） |
| `STATIC_DIR` | ビルド済みの画面（`dist/static`） |
| `DEV_LOGIN` | 開発用のログインを有効にする（OIDC を設定せず、`PUBLIC_URL` が localhost などのときだけ. それ以外では起動しない） |

### 会議の通信（WebRTC）について

- 映像・音声は参加者どうしが直接送り合います（メッシュ. サーバーは中継しません）. 人数が増えるほど各端末が送る量が増えるため、
  自動で映像の送る量を下げます. 目安は 2〜8 人、上限は 50 人（会議室ごと）です.
- 社内の LAN の中ではそのままつながりますが、会社の外・モバイル回線・厳しいファイアウォールの中からは、**TURN サーバー**が必要になることがあります.
  coturn を `use-auth-secret` で動かし、`TURN_URLS=turn:turn.example.com:3478,turns:turn.example.com:5349` と `TURN_SECRET` を設定してください.
- 会議中の状態はアプリのメモリにあります. 複数ノードで動かすときは、同じ会議室が同じノードに届くように（`/ws/` と `/api/rooms/{code}/join` を
  会議室のコードで振り分けるか、スティッキーセッションで）してください.
- リバースプロキシでは `/ws/` の WebSocket（`Upgrade` ヘッダー）を通してください.

### OIDC の IdP の設定

OpenID Connect の Discovery（`/.well-known/openid-configuration`）に対応した IdP で使えます。

- クライアントの種類: コンフィデンシャル（クライアントシークレットあり）、フロー: Authorization Code（PKCE の S256 を使う）
- Redirect URI: `${PUBLIC_URL}/auth/callback`
- ログアウト後の戻り先（post_logout_redirect_uri）: `${PUBLIC_URL}/`
- ID トークンに `email`・`name` があると、会議の予定に招待する相手（skuaka）や、録画を共有する相手（skawau）を探せます

## API トークンと MCP

設定の画面（右上のメニュー →「設定」）の「API トークンと MCP」でトークンを作ります。値は作成したときだけ表示され、DB には SHA-256 だけを保存します。
トークンの権限は作った人と同じで、「参照のみ」のトークンは GET（MCP では参照のツール）だけが使えます。トークンの作成・失効は画面からだけできます。

REST API（一覧は `GET /api/endpoints`）:

```bash
curl -H "Authorization: Bearer skaka_…" https://meet.example.com/api/rooms
```

MCP（URL は `${PUBLIC_URL}/mcp`、トランスポートは Streamable HTTP）. MCP クライアントの設定の例:

```json
{
  "mcpServers": {
    "skaka": {
      "type": "http",
      "url": "https://meet.example.com/mcp",
      "headers": { "Authorization": "Bearer skaka_…" }
    }
  }
}
```

| ツール | 内容 |
| --- | --- |
| `get_me` | 自分の情報・タイムゾーン・今日の日付 |
| `list_rooms` / `get_room` | 会議室の一覧（入っている人数）・会議室の情報といま入っている人 |
| `create_room` / `update_room` / `delete_room` | 会議室の作成・変更・削除 |
| `list_meetings` / `get_meeting` | 会議の履歴・参加者とチャットの記録（議事録や要約に） |
| `list_recordings` | 会議の録画と、skawau で開く URL |
| `list_upcoming` | skuaka の今後の予定と、その会議室 |
| `schedule_meeting` | 会議の予定を立てる（会議室を作り、skuaka に予定を作って招待） |
| `add_room_to_event` | skuaka の予定に会議室を付ける |
| `list_api_endpoints` / `call_api` | 専用のツールがない操作を REST API で |

MCP のツールでは、日時をトークンの持ち主のタイムゾーン（`yyyy-MM-ddTHH:mm`）で読み書きします。会議室はコードか URL で指定できます。

## 同梱しているもの

- 人物の切り抜きのモデル `frontend/public/models/selfie_segmenter_landscape.tflite`（Google MediaPipe. Apache License 2.0）
- ビルドのときに `@mediapipe/tasks-vision`（Apache License 2.0）の WASM を `dist/static/assets/mediapipe-{版}/` に入れる
- ロゴ・背景の画像（`frontend/public/`）は skaka のために作ったもの（MIT）

