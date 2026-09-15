#!/usr/bin/env bash
# Build and install the compiled cross-platform dotfiles operations CLI.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! command -v cargo >/dev/null 2>&1; then
    echo "Error: cargo is required to build sys." >&2
    exit 1
fi

cargo install --locked --force --root "$HOME/.local" --path "$DOTFILES_DIR/tools/sys"
echo "Installed sys to $HOME/.local/bin/sys"
