#!/usr/bin/env bash
# Manage the on-demand Ollama runtime and its LM Studio mirror on macOS.

set -uo pipefail

action="${1:-status}"
model="${2:-gpt-oss:20b}"
endpoint="http://localhost:11434"

up() { curl -fsS "$endpoint/api/tags" >/dev/null 2>&1; }

case "$action" in
    start)
        if ! up; then
            brew services run ollama >/dev/null 2>&1 || true
            printf "  waiting for endpoint"
            for _ in $(seq 30); do up && break; printf '.'; sleep 1; done
            echo
            up || { echo "✗ Ollama did not come up — check: brew services list | grep ollama"; exit 1; }
        fi
        echo "✓ Ollama up ($endpoint)"
        printf "  warming %s…" "$model"
        if curl -fsS "$endpoint/api/generate" \
            -d "{\"model\":\"$model\",\"prompt\":\"\",\"keep_alive\":-1}" >/dev/null 2>&1; then
            echo " ✓ resident"
        else
            echo " ✗ (is $model pulled? \`sys llm status\`)"
        fi
        ;;
    stop)
        brew services stop ollama
        ;;
    status)
        if up; then
            echo "✓ Ollama up ($endpoint)"
            ollama list
            echo
            echo "loaded:"
            ollama ps
        else
            echo "✗ Ollama down — run \`sys llm start\`"
            exit 1
        fi
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
        echo "✓ mirror synced → $base (rescan in LM Studio to see them)"
        ;;
    *)
        echo "usage: sys llm [start|stop|status|mirror] [model]"
        exit 2
        ;;
esac
