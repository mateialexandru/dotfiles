#!/usr/bin/env bash
# Manage the on-demand LM Studio/MLX runtime on macOS.
#
# Ollama remains available as an explicit GGUF fallback, but is not the default
# runtime. LM Studio's server listens on localhost:1234.

set -uo pipefail

action="${1:-status}"
model="${2:-qwen3.6-35b-a3b}"
lmstudio_endpoint="http://127.0.0.1:1234"
ollama_endpoint="http://localhost:11434"
lms_bin="$(command -v lms 2>/dev/null || true)"
[[ -x "$HOME/.lmstudio/bin/lms" ]] && lms_bin="$HOME/.lmstudio/bin/lms"

lmstudio_up() { curl -fsS "$lmstudio_endpoint/v1/models" >/dev/null 2>&1; }
ollama_up() { curl -fsS "$ollama_endpoint/api/tags" >/dev/null 2>&1; }

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
    if printf '%s\\n' "$loaded" | grep -Fq "$model"; then
        echo "✓ $model already resident"
    else
        if printf '%s\\n' "$loaded" | grep -Fq "Local"; then
            echo "  unloading the previously resident model"
            "$lms_bin" unload --all >/dev/null 2>&1 || true
        fi
        printf "  loading %s… " "$model"
        if "$lms_bin" load "$model" --gpu max -y; then
            echo "✓ resident"
        else
            echo "✗ could not load $model — run \`lms ls\` to check the model key" >&2
            exit 1
        fi
    fi
}

start_ollama() {
    local ollama_model="${1:-gpt-oss:20b}"
    if ! ollama_up; then
        brew services run ollama >/dev/null 2>&1 || true
        printf "  waiting for Ollama"
        for _ in $(seq 30); do ollama_up && break; printf '.'; sleep 1; done
        echo
        ollama_up || { echo "✗ Ollama did not come up" >&2; exit 1; }
    fi
    echo "✓ Ollama up ($ollama_endpoint)"
    printf "  warming %s… " "$ollama_model"
    if curl -fsS "$ollama_endpoint/api/generate" \
        -d "{\"model\":\"$ollama_model\",\"prompt\":\"\",\"keep_alive\":-1}" >/dev/null 2>&1; then
        echo "✓ resident"
    else
        echo "✗ (is $ollama_model pulled? \`sys llm ollama status\`)"
    fi
}

status_ollama() {
    if ollama_up; then
        echo "Ollama fallback up ($ollama_endpoint)"
        ollama ps
    else
        echo "Ollama fallback down"
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
            status_ollama
            exit 1
        fi
        ;;
    ollama)
        case "${2:-status}" in
            start) start_ollama "${3:-gpt-oss:20b}" ;;
            stop) brew services stop ollama ;;
            status) status_ollama ;;
            *) echo "usage: sys llm ollama [start|stop|status] [model]"; exit 2 ;;
        esac
        ;;
    mirror)
        base="$HOME/.lmstudio/models/ollama"
        blobs="$HOME/.ollama/models/blobs"
        manifests="$HOME/.ollama/models/manifests"
        mkdir -p "$base"
        for directory in "$base"/*/; do
            [ -d "$directory" ] || continue
            file=$(find "$directory" -maxdepth 1 -name '*.gguf' -print -quit 2>/dev/null)
            { [ -z "$file" ] || [ ! -e "$file" ]; } && {
                rm -rf "$directory"
                echo "  pruned $(basename "$directory")"
            }
        done
        [ -d "$manifests" ] || { echo "note: no Ollama models yet"; exit 0; }
        find "$manifests" -type f 2>/dev/null | while read -r manifest; do
            tag=$(basename "$manifest")
            name=$(basename "$(dirname "$manifest")")
            digest=$(jq -r '.layers[]|select(.mediaType=="application/vnd.ollama.image.model").digest' \
                "$manifest" 2>/dev/null | sed 's/sha256://')
            [ -z "$digest" ] && continue
            [ -f "$blobs/sha256-$digest" ] || continue
            safe="$name-$tag"
            mkdir -p "$base/$safe"
            ln -sf "$blobs/sha256-$digest" "$base/$safe/$safe.gguf"
            echo "  mirrored $name:$tag"
        done
        echo "✓ Ollama GGUF mirror synced → $base (rescan in LM Studio to see them)"
        ;;
    *)
        echo "usage: sys llm [start|stop|status|ollama|mirror] [model]"
        echo "       sys llm ollama [start|stop|status] [model]"
        exit 2
        ;;
esac
