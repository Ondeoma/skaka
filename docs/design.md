# skaka の設計

## 認証（OIDC）

skuaka と同じ（兄弟のシステムで揃えている）.

- Authorization Code + PKCE（S256）+ nonce + state. ログイン途中の状態は DB の `login_state` に置き、15分・1回だけ使える.
- ID トークンはトークンエンドポイントから受け取ったうえで **署名も検証する**（JWKS. `alg=none` は拒否）. `iss`・`aud`・`exp`・`nonce` を確かめる.
- セッションはランダムな値の Cookie（HttpOnly・SameSite=Lax・https なら Secure）. DB にはその SHA-256 だけを保存する.
- CSRF: GET 以外の API は `X-Skaka` の見出しが必要（ほかのサイトのフォームからは付けられない）. ログインしていない人（ゲスト）の入室・ログアウト・開発用のログインの要求も同じ.
- ログイン・連携のあとの戻り先は、同じサイトのパスだけ（`SafePath`. `//`・`\`・制御文字を含むものは断る）.
- IdP が `email_verified: false` としたメールアドレスは使わない（録画の共有の相手をメールで探すため）.
- 開発用のログイン（`DEV_LOGIN`）は、`PUBLIC_URL` が手元（localhost など）のときだけ起動できる.

### 応答の守り（`Guard`）

- リクエストの本文は 1MB まで（録画の断片だけ 32MB まで、文字起こしの音声は 2MB まで）. Content-Length がなくても上限で読むのをやめる. 会議の WebSocket の1つのメッセージは 64KB まで.
- 会議中のメッセージは、1つの接続から10秒に3000件まで（チャット・リアクションは40件まで）. 超えた分は捨てる.
- すべての応答に `X-Content-Type-Options: nosniff`・`Referrer-Policy: same-origin`（会議室の URL を他のサイトに渡さない）.
  HTML には `X-Frame-Options: DENY` と CSP（インラインのスクリプトは禁止. MediaPipe の WASM のため `wasm-unsafe-eval`、Worker の時計のため `blob:` だけを許す）.
- ログインしていない人に見せる入口（`/api/rooms/{code}/entry`）では、持ち主のメールアドレスを出さない.
- 録画は動画（WebM・MP4）だけを受け付け（skawau に置くときの Content-Type になる）、1人が同時に録れるのは1つ.
- OAuth の認可コードが2回使われたら、漏れたものとして、そのコードで発行したトークンを失効させる.
- ユーザーは `sub` で識別する. 初めてのログインで「自分の会議室」を作る.

## 会議室と会議

- 会議室（`room`）は推測されにくいコード（`abc-defg-hij`. 25文字 × 10桁）で識別し、URL は `/r/{code}`. ログインしている人はコードを知っていれば入れる
  （チームの中の会議室. ゲストを受け付けない会議室は、ログインしていない人には名前も見せない）.
- 会議（`meeting`）は、会議室に最初の人が入ってから最後の人が出るまで. 参加者の出入り（`meeting_participant`）と全員宛のチャット（`chat_message`）を残す.
  個別のメッセージは残さない. 起動したときに、前に動いていたときの終わっていない会議を終える.
- 入る: `POST /api/rooms/{code}/join` で切符（ランダム・1回だけ・2分）をもらい、`GET /ws/{ticket}` の WebSocket につなぐ.
  切符にすることで、WebSocket の接続に Cookie の確認・CSRF の確認を持ち込まない（切符の発行は `X-Skaka` 付きの POST）. API トークンでは入れない.
- 会議中の状態は **メモリ**（`Hub`）. 状態の変化は `core` の `Conference`（純粋な関数: 今の状態とメッセージ → 新しい状態と `Effect` の一覧）で決め、
  `Hub` は `AtomicCell` で1つずつ適用し、送る（`Effect.Send`）・切る（`Disconnect`）を状態を変えたのと同じ順番で各接続のキューに入れる.
  DB への書き込み（会議の開始・終了・出入り・チャット）は裏のキューで順に行い、会議の進行を止めない.
- 返事のない接続（スリープ・回線断で TCP が残ったもの）は、75秒何も届かなければ切る（画面は20秒ごとに Ping を送る）.
- 複数ノード: 会議中の状態はノードごと. 同じ会議室を同じノードに振り分ける必要がある（README の「会議の通信」）.

### 主催者・待機室・ロック

- 主催者は会議室の持ち主と、会議中に主催者にされた人（ユーザーで覚えるので、入り直しても主催者. 会議が終わると消える）. 持ち主は外せない.
- 待機室: ゲストはいつも待機室へ. 会議室の `lobby` が有効ならメンバーも（主催者を除く）. 待機室の人を入れられるのは主催者で、
  主催者がいなければログインしているメンバー（チームの会議で主催者が遅れても困らないように）.
- ロック中は主催者のほかは入れない. 全員が出るとロックは外れる.
- マイクをオフにする操作は「頼む」だけ（各端末が自分でオフにする）. 映像・音声はサーバーを通らないため、強制はできない.

## 映像・音声（WebRTC）

- **メッシュ**: 参加者どうしが相手ごとに `RTCPeerConnection` を張る. サーバー（SFU）を持たないので運用が軽い代わりに、
  端末の送る量は人数に比例する. 人数に応じてカメラの映像の上限（`maxBitrate`）を下げる.
- **送る品質**（`Quality`. 端末ごとに localStorage）: カメラの解像度・フレームレートは getUserMedia の条件にし、変えたら `applyConstraints` で
  そのまま変える（できなければ取り直す）. 映像の上限は「解像度ごとの基準（360p 500k・540p 900k・720p 1.5M・1080p 2.8M）× 人数（1人 1.0・〜3人 0.55・
  〜6人 0.3・7人〜 0.17）× 画質（節約 0.5・自動 1.0・高画質 1.6）× フレームレート / 24」. 画面共有は「文字をくっきり」（〜1440p・15fps・
  `contentHint = detail`）か「動きをなめらかに」（〜720p・30fps・`motion`）. マイクのエコー除去・雑音抑制・音量の自動調整は取り直して変える
  （Chrome は同じマイクの開いているトラックと音声処理を共有するため、前のトラックを止めてから取る）. 高音質は音声の送る枠の `maxBitrate` を 128kbps に.
- **背景**（`Background`. 端末ごとに localStorage）: カメラの映像を幅256に縮めて MediaPipe の ImageSegmenter（`selfie_segmenter_landscape`. VIDEO モード）で
  人物らしさ（0〜1）を出し、0.25〜0.75 をなめらかにつないで透明度にしたマスクを作る. 背景（ぼかした映像か画像）の上に、マスクの形で抜いた映像を重ねた
  canvas を `captureStream` し、送る映像にする（カメラそのものは `Meeting.camera`. 解像度の変更・停止はこちら）. 描く時計は Worker（`Ticker`. 録画と共通）.
  背景を変えるときは、合成した映像ができるまで映像を送らない（元の背景が一瞬でも相手に見えないように）.
  - 処理はすべて端末の中. WASM（約14MB）は Vite のプラグイン（`frontend/vite-mediapipe.js`）が `/assets/mediapipe-{版}/` で自前のサーバーから配り、
    モデル（約250KB）は `frontend/public/models/`. どちらも背景を使った人だけが初回に読む（`frontend/segment.js` を動的 import）.
  - 切り抜きは CPU で処理するときも WebGL2 で映像を取り込むため、WebGL2 がなければ初めから断る. GPU（WebGL）で作れなければ CPU で作る.
    処理が30フレーム続けて失敗したら、知らせてカメラのままに戻す.
  - 自分の画像は 1280×720 に収まるよう切り取って JPEG（約0.85）にし、localStorage に1枚だけ保存する（入らなければその会議の間だけ使う）.
- 接続の手順は **Perfect Negotiation**: どちらの側からも `negotiationneeded` で offer を出してよく、ぶつかったら id の小さい側（polite）が譲る.
  SDP・ICE は WebSocket で相手に中継するだけ（サーバーは中身を見ない）. 相手からの SDP・ICE は Future をつないで順番に処理する.
- ただし **最初の offer はあとから入った側（initiator）だけ**が出す. 既にいる側は相手の offer に答えてから自分の送る枠を足す.
  両方が同時に最初の offer を出すと、譲った側の ICE の候補が捨てられて ICE が始まらないことがあった（3人の会議で3回に1回ほど）.
  念のため、8秒たってもつながらなければ、譲らない側が `restartIce()` する.
- カメラ・マイクは接続を作るときに `sendonly` の送る枠（transceiver）を作っておき、オン・オフはトラックの差し替え（`replaceTrack`）で行う
  （交渉し直さない. カメラをオフにするとトラックを止め、カメラのランプも消える）. マイクのオフは `enabled = false`.
- 画面共有は始めるたびに新しい送る枠を足し、やめると `transceiver.stop()`（交渉し直す）. 相手は MediaStream の id（`MediaState.screen`）で、
  カメラと画面共有を見分ける（msid は相手にもそのまま届く）.
- 音は映像の枠とは別の `audio` 要素で鳴らす（表示のしかたで枠が消えても聞こえる. スピーカーの選択は `setSinkId`）. `video` はいつも消音.
- 話している人: Web Audio の `AnalyserNode` で 150ms ごとに音の大きさ（RMS）を見る. マイクがオフの人は数えない.
- ICE サーバーは切符と一緒に渡す. TURN は coturn の `use-auth-secret`（`username = 期限:ユーザー`、`credential = HMAC-SHA1(secret, username)`）.

## skuaka（カレンダー）との連携

- skaka から skuaka を使う向き: skuaka は OAuth の許可の画面（提供側）を持たないため、ユーザーが skuaka の **API トークン**を設定の画面に登録する.
  トークンは `SECRET_KEY` から作った鍵で AES-256-GCM で暗号化して `skuaka_link` に保存する. 登録のときに `GET /api/me` で持ち主を確かめ、
  `PATCH /api/me {}`（何も変えない更新）が通るかで読み書きのトークンか確かめる.
- skuaka の REST API をそのトークンで呼ぶ（`SkuakaClient`）. skuaka が 401 を返したら「登録し直し」を促す.
- 予定と会議室の結び付き: 予定の **場所** に会議室の URL（`${PUBLIC_URL}/r/{code}`）を入れる（招待された人の skuaka にもそのまま出る）.
  会議室の側にも予定の写し（`room.event_*`）を持ち、会議室の一覧・入る前の画面に出す. 今後の予定の一覧では、場所の URL と `room.event_id` の両方から会議室を探す.
- 予定に会議室を付けるときは、予定の定義（`GET /api/events/{id}`）を変更の要求に組み直し、場所だけを変えて `PUT ?scope=all`（version 付き）で送る.
  知らない項目（繰り返しの決まりなど）はそのまま渡す. skuaka への書き込みに失敗したら、作った会議室を消す.
- 会議の予定: 自分のメインのカレンダーに、招待する人（メールアドレスで skuaka のユーザーを探す）と10分前の通知を付けて作る.
- skuaka から skaka を使う向き（skuaka で予定を登録するときに「Web 会議を付ける」）: skaka が OAuth の提供側になる（`OAuthRoutes`. skawau と同じ形）.
  `OAUTH_CLIENTS` に登録したサービスに、許可の画面で認めた人の API トークン（「skuaka 連携」. `api_token.oauth_client`）を発行する. 認可コードは `oauth_code`（60秒・1回だけ）.
  skuaka はそのトークンで `POST /api/rooms`（予定の写し `event` 付き）を呼び、予定の題名・日時が変わったら `PUT /api/rooms/{code}/event` で写しを直す
  （会議室の名前が前の題名のままなら、名前も合わせる）. 取り決めは [skuaka-integration.md](skuaka-integration.md).

## 録画（保存先は skawau）

- **連携**: skawau の OAuth（認可コード + PKCE. skea・skiwi が skawau を読むのと同じ仕組み）で、スコープ `read write` のトークンを受け取る.
  skawau 側でクライアントごとに許すスコープを決める（`OAUTH_CLIENTS` の `scopes`）. 書き込みを許されなかったら、受け取ったトークンを失効させて「連携できない」と伝える.
  トークンは `SECRET_KEY` の鍵で暗号化して `skawau_link` に保存. 許可の画面へ送り出すときの state・PKCE の verifier は `oauth_state`（10分・1回だけ）.
- **録る**: 映像はサーバーを通らない（メッシュ）ため、録画する人（会議中の主催者）のブラウザが録る（`Recorder`）.
  会議の枠を canvas（1280×720・15fps）に描き（画面共有があれば大きく、人は右に. なければ 16:9 の枠を並べる）、全員の音（自分のマイク・相手・画面共有の音）を
  Web Audio の `MediaStreamDestination` にまとめ、`MediaRecorder`（WebM の VP8・Opus. Safari は MP4）で録る. 描く時計は Worker（背景のタブでも遅くならない）.
  入ってきた人・出た人の音は1秒ごとにトラックの id で付け外しする.
- **送る**: 4秒ごとの断片を順に `PUT /api/recordings/{id}/chunks/{seq}` で送り、サーバーは一時ファイル（`RECORDING_DIR`）に追記する.
  送り直しで同じ番号が来たら受け流し、番号が飛んだら断る（409）. 通信の失敗は待ってから送り直す（6回まで）.
  skawau のアップロードは1回の PUT で全体を送る仕組み（分割して送れない）ため、会議の間は skaka にためる（会議の途中で切れても、それまでの分は残る）.
- **知らせる**: 録画中であることは `Live.recording` に持ち、`ServerMsg.Recording` で全員に、後から入った人には `Welcome` で知らせる. 1つの会議室で同時に1つ.
- **保存**: 画面が終えた（`finish`）・断片が90秒届かない（画面を閉じた・回線断）・起動したときに残っていた、のどれかで、裏で skawau に置く.
  ブラウザの WebM には長さ（Duration）がなくシークできないため、先頭の Segment の Info に Duration を足してから送る（`core` の `Webm`. 純粋な関数）.
  置き場所は録画した人のルートの `skaka 録画/yyyy-MM/{会議室の名前} {日時}.webm`（同じ名前があれば skawau が名前を変える）.
  会議に参加したメンバーのメールアドレスで skawau のユーザーを引き（`POST /api/users/lookup`. 完全一致）、閲覧のみで共有する.
  失敗したら（skawau が止まっている・連携が切れた）一時ファイルを残して `failed` にし、録画した人が「もう一度保存」できる.
- **見る**: 録画した人・会議室の持ち主・その会議に参加した人に、会議の記録の画面・`GET /api/recordings`・MCP の `list_recordings` で見せる.
  中身は skawau の `/view/{id}` で開く（skawau の権限で. 共有されていない人は開けない）. skaka の一覧から消しても skawau のファイルは消さない.

## 文字起こし（自前の Whisper）

- **始める**: 主催者が `ClientMsg.Transcribe(true)`. `Live.transcribing` に持ち、`ServerMsg.Transcribing` で全員に、後から入った人には `Welcome` で知らせる.
  全員が出たら止まる. サーバーに `WHISPER_URL` がなければ断る（画面にも出さない）.
- **音を拾う**: 文字起こし中は、各自のブラウザ（`Transcriber`）が自分のマイクのトラックだけを Web Audio で読み、話の切れ目ごとに切り出す.
  音の大きさ（RMS）が周りの雑音（話していない間の大きさを覚える）の3倍（最低 0.015）を2回続けて超えたら話し始め. 直前の約0.3秒も含める.
  0.9秒黙るか20秒続いたら区切り、話した時間が0.4秒より短いもの（咳など）は捨てる. 16kHz・16bit・モノラルの WAV にして送る（20秒で約 640KB）.
  マイクを切っている間は送らない（サーバーでも、マイクを切った人の音声は文字にしない）.
- **話者**: 映像・音声はメッシュで、音声は最初から人ごとに分かれている. 送った人がそのまま話した人になるので、話者の推定（diarization）は要らない.
  同時に話しても混ざらない. ほかの人の声がスピーカーから回り込む分は、ブラウザのエコー除去が消す.
- **送る**: `POST /api/speech/{key}?ago={話し始めてからの ms}`（本文は音声）. key は会議に入る切符（`POST /api/rooms/{code}/join`）と一緒に発行する鍵で、
  WebSocket の URL には載せない（アクセスログに残らない）. 接続している間だけ使え、ゲストも送れる.
- **文字にする**: Whisper の OpenAI 互換の API（`POST {WHISPER_URL}/audio/transcriptions`. model・language=ja・response_format=json・temperature=0）に
  マルチパートで送る（大きさを付ける. chunked を読めないサーバーがある）. speaches・faster-whisper-server・LocalAI・whisper.cpp の server などが使える.
  同時に送る数は `WHISPER_CONCURRENCY`（既定 2）. 待ちがその4倍を超えたら断る（遅れて届く字幕は役に立たない）. 接続の失敗は1回だけ送り直す.
  無音や雑音から Whisper が作りがちな文（「ご視聴ありがとうございました」など）と、同じ音の繰り返しは捨てる（`Transcriber.clean`）.
- **配る・残す**: 文字にできたら `ServerMsg.Caption`（話した人の名前・話し始めた時刻）で全員に送り、`transcript_line` に保存する.
  画面は届いてから12秒の間、最後の3行を字幕として出す. 会議の記録の画面・Markdown の書き出しに「文字起こし」として出る.

## 議事録・要約（skakapo）

- **連携**: skakapo の API トークン（`skakapo_…`. 読み書き）を設定の画面で登録する（skuaka と同じ `TokenLinks`. `skakapo_link` に暗号化して保存）.
- **頼む**: 会議の記録の画面の「skakapo で議事録を作る」（会議を見られる人なら誰でも）. 参加者・文字起こし・チャットを Markdown にして（`MeetingText`）
  skakapo に添付し（`POST /api/attachments`）、依頼の種類 `SKAKAPO_REQUEST_TYPE`（既定「その他の依頼」）で会話を作る（`POST /api/sessions`）.
  依頼の内容には、議事録の形式（要約・決まったこと・宿題・話し合ったこと・次回までに確かめること）と、ほかのサービスに書き込まないことを書く.
- **取り込む**: 裏で skakapo の会話の出来事を読み（`GET /api/sessions/{id}/events`）、状態が `idle` になったら最後のエージェントの返事を `minutes` に保存する.
  失敗（`failed`）なら最後のエラーの文を残す. 60分待っても終わらなければ、取り込み直しを促して終える. 起動したときに待っていたものは続きを見る.
- **直す**: 返事に満足できなければ、skakapo の会話で直してもらい、skaka で「取り込み直す」（頼んだ人だけ. 最新の返事を取り込む）.
- **skawau に置く**: 取り込んだら、頼んだ人の skawau（録画と同じ OAuth の連携）のルートの `skaka 議事録/yyyy-MM/議事録 {会議室の名前}（{日時}）.md` に置き、
  会議に参加したメンバーと閲覧のみで共有する（録画と共通の `SkawauFiles`）. ファイルには議事録の本文と、skaka の会議の記録・skakapo の会話への案内を書く.
  取り込み直したら、置いたファイルの中身を書き換える（`PUT /api/files/{id}/content`. skawau に版が残る. 移動・改名されていても同じファイル. 消されていたら置き直す）.
  置けなかったら（skawau と連携していない・連携が切れた）理由を残し、頼んだ人が「skawau に保存」でもう一度置ける. skawau を設定していなければ置かない.

## API トークンと MCP

skuaka と同じ.

- API トークンは `skaka_` + 30バイトの乱数. DB には SHA-256 と先頭の数文字だけ. スコープは `read`（GET だけ）と `write`.
- `Authorization: Bearer` の要求は CSRF の確認をしない. トークンの作成・失効・skuaka のトークンの登録は Cookie のセッションからだけ.
- MCP は `POST /mcp` の JSON-RPC（Streamable HTTP. 応答は JSON）. 各ツールは同じトークンで REST API を内部から呼ぶので、
  権限・入力の確認は REST API と同じ. 参照のみのトークンには、変更するツールを `tools/list` に出さない.

## 画面

- Scalactive（Vue 風の `.st` 単一ファイルコンポーネント）. `Meeting`（Scala.js の object）が会議の状態（RV）と WebRTC を持ち、
  画面の部品（`MeetingStage`・`VideoTile`・`ChatPanel`・`PeoplePanel`）はそれを読むだけ. 映像の割り当てが変わったら `Meeting.streams` を増やし、
  `VideoTile` が `video.srcObject` を入れ直す.
- `RouterLink` はクラスを受け取らないため、クラスを付けたいリンクは `parts/Link.st` を使う. スコープ付きのスタイルは子のコンポーネントに届かない
  （子の中の要素のスタイルは `frontend/style.css` に置く）.
