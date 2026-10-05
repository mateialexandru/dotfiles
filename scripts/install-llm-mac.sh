#!/usr/bin/env bash
# Local LM Studio/MLX runtime for macOS. Model choice is managed by private layers.

set -euo pipefail

echo "==> LM Studio / MLX (local runtime)"
brew install --cask lm-studio
# Put the CLI on PATH so `sys llm` can start the server and load a model.
[[ -x "$HOME/.lmstudio/bin/lms" ]] && "$HOME/.lmstudio/bin/lms" bootstrap >/dev/null 2>&1 || true

echo "==> Local LLM ready. Control LM Studio with \`sys llm start|stop|status\`."
