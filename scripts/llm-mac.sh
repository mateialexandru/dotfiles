#!/usr/bin/env bash
# Manage the on-demand LM Studio/MLX runtime on macOS.
#
# LM Studio's server listens on localhost:1234.

set -uo pipefail

action="${1:-status}"
model="${2:-qwen3.6-35b-a3b}"
lmstudio_endpoint="http://127.0.0.1:1234"
lms_bin="$(command -v lms 2>/dev/null || true)"
[[ -x "$HOME/.lmstudio/bin/lms" ]] && lms_bin="$HOME/.lmstudio/bin/lms"

lmstudio_up() { curl -fsS "$lmstudio_endpoint/v1/models" >/dev/null 2>&1; }

require_lms() {
    if [[ -z "$lms_bin" || ! -x "$lms_bin" ]]; then
        echo "✗ LM Studio CLI not found — open LM Studio and run \`lms bootstrap\`" >&2
        exit 1
    fi
}

start_lmstudio() {
    require_lms
    if ! lmstudio_up; then
        "$lms_bin" server start --port 1234 >/dev/null 2>&1 || true
        printf "  waiting for LM Studio"
        for _ in $(seq 30); do lmstudio_up && break; printf '.'; sleep 1; done
        echo
        lmstudio_up || {
            echo "✗ LM Studio did not come up — try: lms server start --port 1234" >&2
            exit 1
        }
    fi
    echo "✓ LM Studio up ($lmstudio_endpoint)"
    loaded="$("$lms_bin" ps 2>/dev/null || true)"
    if printf '%s\n' "$loaded" | grep -Fq "$model"; then
        echo "✓ $model already resident"
    else
        if printf '%s\n' "$loaded" | grep -Fq "Local"; then
            echo "  unloading the previously resident model"
            "$lms_bin" unload --all >/dev/null 2>&1 || true
        fi
        printf "  loading %s… " "$model"
        if "$lms_bin" load "$model" --gpu max --parallel 1 --ttl 3600 -y; then
            echo "✓ resident"
        else
            echo "✗ could not load $model — run \`lms ls\` to check the model key" >&2
            exit 1
        fi
    fi
}

case "$action" in
    start)
        start_lmstudio
        ;;
    stop)
        require_lms
        "$lms_bin" unload --all >/dev/null 2>&1 || true
        "$lms_bin" server stop >/dev/null 2>&1 || true
        echo "✓ LM Studio stopped (models unloaded)"
        ;;
    status)
        require_lms
        if lmstudio_up; then
            echo "✓ LM Studio up ($lmstudio_endpoint)"
            echo "loaded:"
            "$lms_bin" ps
        else
            echo "✗ LM Studio down — run \`sys llm start\`"
            exit 1
        fi
        ;;
    *)
        echo "usage: sys llm [start|stop|status] [model]"
        exit 2
        ;;
esac
