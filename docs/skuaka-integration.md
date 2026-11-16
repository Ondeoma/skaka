# skuaka 連携の取り決め（skuaka から skaka を使う向き）

skuaka で予定を登録するときに「Web 会議（skaka）を付ける」を選ぶと、skaka に会議室を作り、予定の **場所** に会議室の URL を入れる。
（逆向きの「skaka から skuaka の予定を読む・会議の予定を立てる」は、skaka の設定で skuaka の API トークンを登録する別の仕組み. docs/design.md）

- 各ユーザーは skuaka の設定で「skaka と連携する」を押し、skaka の画面で許可する（OAuth 2.0 の認可コード + PKCE）. 兄弟の skea・skiwi が skawau とつなぐのと同じ形.
- skaka は、そのユーザーの **読み書き** の API トークン（`skaka_…`. 名前は「skuaka 連携」）を発行する. skuaka はそれを暗号化して保存し、サーバーから skaka の API を呼ぶ.
- 連携していないユーザーには、予定の編集画面で「skaka と連携すると Web 会議を付けられます」と案内する.

## skaka 側（このリポジトリ）

### 1. クライアントの登録（環境変数 `OAUTH_CLIENTS`）

```json
[{"id": "skuaka", "secret": "（16文字以上）", "redirectUri": "{skuaka の URL}/api/integrations/skaka/callback", "name": "skuaka", "scopes": ["read", "write"]}]
```

開発環境: `client_id=skuaka`、`client_secret=skuaka-dev-secret-0123456789`、`redirect_uri=http://localhost:5175/api/integrations/skaka/callback`
（skaka の `scripts/dev-env.sh` に登録済み）. skuaka から見た skaka の URL は `http://localhost:5177`（skaka の画面の開発サーバー. `/oauth`・`/api` を skaka のサーバーへ中継する）.

### 2. `GET /oauth/authorize`

`response_type=code`、`client_id`、`redirect_uri`（完全一致）、`scope=read write`、`state`、`code_challenge`（S256. 43文字）、`code_challenge_method=S256`.
ログインしていなければ skaka のログインを経て戻る. 許可の画面で「許可する」を押すと `redirect_uri?code=…&state=…`、拒否すると `?error=access_denied&state=…`.

### 3. `POST /oauth/token`（クライアント認証は Basic）

`application/x-www-form-urlencoded`: `grant_type=authorization_code`、`code`、`redirect_uri`、`code_verifier`（43〜128文字）.
応答: `{"access_token": "skaka_…", "token_type": "Bearer", "scope": "write"}`（コードは60秒・1回だけ）. 許可し直すと前のトークンは失効する.

### 4. `POST /oauth/revoke`（Basic）

`token=skaka_…`. 連携をやめるときに呼ぶ（なければ何もしない. いつも 200）.

### 5. API（`Authorization: Bearer skaka_…`）

- **会議室を作る**: `POST /api/rooms`
  ```json
  {"name": "設計レビュー", "description": "", "guestAccess": false, "lobby": false,
   "event": {"eventId": "<skuaka の予定の id>", "title": "設計レビュー", "start": "2026-11-20T01:00:00Z", "end": "2026-11-20T02:00:00Z", "allDay": false}}
  ```
  応答（201）は RoomDto. `url`（`{skaka の URL}/r/abc-defg-hij`）を予定の場所に入れる. 名前は100文字まで（長いタイトルは切る）. `event` は省略できる.
  終日の予定は `start`・`end` に `yyyy-MM-dd` も使える.
- **予定の写しを変える**: `PUT /api/rooms/{code}/event`（本文は上の `event` と同じ形. `null` で外す）. 予定のタイトル・日時を変えたとき（失敗しても予定の保存は止めない）.
  会議室の名前が前の予定のタイトルのまま（利用者が手で変えていない）なら、会議室の名前も新しいタイトル（100文字まで）に合わせる. 応答は変えたあとの RoomDto.
- **会議室を消す**: `DELETE /api/rooms/{code}`（予定を消しても、会議室は自動では消さない. 会議の履歴が残るため）.
- 401 はトークンの失効（skaka の設定で連携をやめた）. skuaka は連携が切れたと扱い、連携し直しを促す.

skaka は、予定の場所に `{skaka の URL}/r/{code}` があるか、会議室に結び付いた予定の id で、ホームの「今後の予定」に会議室を出す.

## skuaka 側（skuaka のリポジトリ）

- **環境変数**: `SKAKA_URL`（画面から開く skaka の URL）、`SKAKA_CLIENT_ID`、`SKAKA_CLIENT_SECRET`（3つとも設定したときだけ使える）. `SKAKA_INTERNAL_URL`（サーバーから呼ぶ URL. 省略時は `SKAKA_URL`）. トークンの暗号化の鍵として `APP_SECRET` も要る.
- **連携**: `/api/integrations/skaka/{connect,callback,connection}`（connect は state・PKCE の verifier を保存して許可の画面へ. callback でトークンを受け取り暗号化して保存.
  connection の DELETE で `/oauth/revoke` を呼んで消す）. 設定の画面に「skaka（Web 会議）と連携する」と状態.
- **予定の編集画面**: 「Web 会議（skaka）を付ける」. 新しく付けるときは、予定を保存したあとで（予定の id が決まってから）`POST /api/rooms` で会議室を作り、
  場所に URL を入れる（場所が空なら URL だけ、あれば `{場所} / {URL}`. skaka が予定に会議室を付けるときと同じ書き方）. 失敗したら予定は残し、「会議室を作れませんでした」と知らせる.
  既に場所に skaka の URL がある予定には付けない（チェックは付いた状態で表示）.
- **予定の変更**: タイトル・日時が変わったら `PUT /api/rooms/{code}/event`（場所の URL から code を取り出す）.
- **予定の詳細**: 場所に skaka の URL があれば「Web 会議に参加」ボタン.
- **MCP**（あれば）: 予定を作るツールに `withMeeting: true` のような引数.
