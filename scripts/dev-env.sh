# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB を使う
export DATABASE_URL=jdbc:postgresql://localhost:54350/skaka DATABASE_USER=skaka DATABASE_PASSWORD=skaka
# 画面は Vite の開発サーバー（:5177. /api・/auth・/ws・/mcp を :8096 へ中継する）から開くので、公開 URL もそちらにする
export PUBLIC_URL=${PUBLIC_URL:-http://localhost:5177} HTTP_PORT=8096
# skuaka の API トークンを暗号化する鍵（開発用の固定値）と、連携する skuaka（開発中の skuaka の画面 :5175 から API にも届く）
export SECRET_KEY=${SECRET_KEY:-skaka-dev-secret-key-0123456789} SKUAKA_URL=${SKUAKA_URL:-http://localhost:5175}
# 録画の保存先の skawau（開発中の skawau の画面 :5176. skawau の scripts/dev-env.sh の OAUTH_CLIENTS に skaka を登録してある）
export SKAWAU_URL=${SKAWAU_URL:-http://localhost:5176} SKAWAU_CLIENT_ID=skaka SKAWAU_CLIENT_SECRET=skaka-dev-secret-0123456789
# skuaka が skaka に会議室を作る（予定に Web 会議を付ける）ための OAuth のクライアント（docs/skuaka-integration.md）
export OAUTH_CLIENTS='[{"id":"skuaka","secret":"skuaka-dev-secret-0123456789","redirectUri":"http://localhost:5175/api/integrations/skaka/callback","name":"skuaka","scopes":["read","write"]}]'
# 認証: OIDC=dev なら開発用の IdP（scripts/dev-idp.sh、:9096）で OIDC のログインを試す。それ以外は名前だけの開発用ログイン
if [ "${OIDC:-}" = "dev" ]; then
  export OIDC_ISSUER=http://localhost:9096 OIDC_CLIENT_ID=skaka OIDC_CLIENT_SECRET=skaka-dev-secret-0123456789
  unset DEV_LOGIN
else
  export DEV_LOGIN=true
  unset OIDC_ISSUER OIDC_CLIENT_ID OIDC_CLIENT_SECRET
fi
# 議事録・要約を頼む skakapo（開発中の skakapo の画面 :5199 から API にも届く）
export SKAKAPO_URL=${SKAKAPO_URL:-http://localhost:5199}
# 文字起こしの Whisper（OpenAI 互換の API. 設定したときだけ使う. docs/design.md の「文字起こし」を参照）
#   export WHISPER_URL=http://localhost:8000/v1 WHISPER_MODEL=Systran/faster-whisper-small
