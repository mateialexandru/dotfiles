#!/usr/bin/env bash
# Local LLM layer for macOS.
#
#   - LM Studio (cask) — the PRIMARY runtime: MLX on Apple Silicon, with an
#                        OpenAI-compatible endpoint on :1234; models are user-selected.
#   - Ollama (formula) — an OPTIONAL GGUF fallback, reproducible from
#                        scripts/ollama-models.txt and available through `sys llm ollama`.
#
# Ollama and LM Studio can't share model files (Ollama always copies into its own
# content-addressed store; GGUF≠MLX). As a convenience, `sys llm mirror` symlinks
# Ollama's GGUF blobs into LM Studio's tree so they also show up there labelled `ollama`.
# LM Studio is user-managed for model selection, but `lms` is provisioned so `sys llm
# start` can reliably load the preferred local model. Ollama remains a fallback, not
# the default runtime.
#
# See docs/decisions/013-llm-ollama-lmstudio.md.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST="$REPO/scripts/ollama-models.txt"
ENDPOINT="http://localhost:11434"

# --- LM Studio / MLX (primary runtime) ---
echo "==> LM Studio / MLX (primary local runtime)"
brew install --cask lm-studio
# Put the `lms` CLI on PATH so `sys llm` can start the server and load models.
[[ -x "$HOME/.lmstudio/bin/lms" ]] && "$HOME/.lmstudio/bin/lms" bootstrap >/dev/null 2>&1 || true

# --- Ollama (formula — optional GGUF fallback) ---
echo "==> Ollama (optional GGUF fallback)"
brew install ollama

# --- Bring the daemon up just long enough to pull models ---
# `brew services run` starts it now WITHOUT registering it at login (unlike `start`),
# matching the on-demand `sys llm ollama *` model.
ollama_started=false
if ! curl -fsS "$ENDPOINT/api/tags" >/dev/null 2>&1; then
    echo "Starting Ollama (on-demand, no login registration) to pull fallback models..."
    brew services run ollama >/dev/null 2>&1 || true
    ollama_started=true
    printf "  waiting for endpoint"
    for _ in $(seq 30); do
        curl -fsS "$ENDPOINT/api/tags" >/dev/null 2>&1 && break
        printf '.'; sleep 1
    done
    echo
    if ! curl -fsS "$ENDPOINT/api/tags" >/dev/null 2>&1; then
        echo "✗ Ollama endpoint did not come up — run \`sys llm ollama start\` and re-run." >&2
        exit 1
    fi
fi

# --- Pull the curated model set (idempotent — pull no-ops when digest is current) ---
echo "==> Pulling models from $(basename "$MANIFEST")"
while IFS= read -r line; do
    model="${line%%#*}"                        # strip inline comment
    model="$(echo "$model" | xargs)"           # trim whitespace
    [ -z "$model" ] && continue
    echo "  ollama pull $model"
    ollama pull "$model"
done < "$MANIFEST"

# Do not leave the fallback daemon resident just because installation needed it.
if $ollama_started; then
    brew services stop ollama >/dev/null 2>&1 || true
fi

# --- Mirror GGUF models into LM Studio (best-effort convenience) ---
bash "$REPO/scripts/llm-mac.sh" mirror 2>/dev/null || true

echo
echo "==> Local LLM ready. LM Studio/MLX is primary (control with \`sys llm start|stop|status\`)."
echo "    Ollama remains an optional fallback (\`sys llm ollama start|stop|status\`)."
