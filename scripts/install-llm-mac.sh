#!/usr/bin/env bash
# Local LLM layer for macOS.
#
#   - Ollama (formula) — the MANAGED runtime: GGUF, scriptable, reproducible from
#                        scripts/ollama-models.txt, OpenAI-compatible endpoint :11434.
#   - LM Studio (cask) — kept, but USER-MANAGED: browse/download MLX models in the GUI
#                        yourself. The installer only ensures it's present.
#
# Ollama and LM Studio can't share model files (Ollama always copies into its own
# content-addressed store; GGUF≠MLX). As a convenience, `sys llm mirror` symlinks
# Ollama's GGUF blobs into LM Studio's tree so they also show up there labelled `ollama`
# — run here at the end (best-effort; self-healing).
#
# The Ollama daemon is ON-DEMAND, not a login service: control with `sys llm start`
# / `sys llm stop` / `sys llm status`. This installer spins it up transiently (via
# `brew services run`, which does NOT register for login) only to pull the model set.
#
# See docs/decisions/013-llm-ollama-lmstudio.md.

set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MANIFEST="$REPO/scripts/ollama-models.txt"
ENDPOINT="http://localhost:11434"

# --- LM Studio (GUI, user-managed) ---
echo "==> LM Studio (GUI — you manage MLX models yourself)"
brew install --cask lm-studio
# `lms` CLI onto PATH for the optional mirror + your own scripting.
[[ -x "$HOME/.lmstudio/bin/lms" ]] && "$HOME/.lmstudio/bin/lms" bootstrap >/dev/null 2>&1 || true

# --- Ollama (formula — headless CLI + daemon, the managed runtime) ---
echo "==> Ollama (managed GGUF endpoint)"
brew install ollama

# --- Bring the daemon up just long enough to pull models ---
# `brew services run` starts it now WITHOUT registering it at login (unlike `start`),
# matching the on-demand `sys llm *` model.
if ! curl -fsS "$ENDPOINT/api/tags" >/dev/null 2>&1; then
    echo "Starting Ollama (on-demand, no login registration) to pull models..."
    brew services run ollama >/dev/null 2>&1 || true
    printf "  waiting for endpoint"
    for _ in $(seq 30); do
        curl -fsS "$ENDPOINT/api/tags" >/dev/null 2>&1 && break
        printf '.'; sleep 1
    done
    echo
    if ! curl -fsS "$ENDPOINT/api/tags" >/dev/null 2>&1; then
        echo "✗ Ollama endpoint did not come up — run \`sys llm start\` and re-run." >&2
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

# --- Mirror GGUF models into LM Studio (best-effort convenience) ---
bash "$REPO/scripts/llm-mac.sh" mirror 2>/dev/null || true

echo
echo "==> Local LLM ready. Ollama on $ENDPOINT (control with \`sys llm start|stop|status\`)."
echo "    LM Studio: open the app and download MLX models yourself; Ollama models are mirrored in."
