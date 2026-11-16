#!/usr/bin/env bash
# 開発用の OIDC プロバイダ（:9096）を起動する。本番では使わないこと（ログは $LOG、既定 /tmp/skaka-idp.log）
cd "$(dirname "$0")/.."
. scripts/lib.sh
LOG=${LOG:-/tmp/skaka-idp.log}
free_port 9096
run_sbt devidp/run "$LOG" '\[devidp\] 開発用' 120
