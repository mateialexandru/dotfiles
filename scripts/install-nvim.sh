#!/usr/bin/env bash
# Install Neovim (LazyVim) on Linux
# Creates symlink and installs dependencies

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
NVIM_SOURCE="$DOTFILES_DIR/nvim"
NVIM_TARGET="$HOME/.config/nvim"

# --- Symlink nvim config ---

if [ -L "$NVIM_TARGET" ]; then
    echo "Neovim symlink already exists: $(readlink "$NVIM_TARGET")"
elif [ -d "$NVIM_TARGET" ]; then
    echo "Backing up existing nvim config to $NVIM_TARGET.backup"
    mv "$NVIM_TARGET" "$NVIM_TARGET.backup"
    ln -s "$NVIM_SOURCE" "$NVIM_TARGET"
    echo "Created symlink: $NVIM_TARGET -> $NVIM_SOURCE"
else
    mkdir -p "$(dirname "$NVIM_TARGET")"
    ln -s "$NVIM_SOURCE" "$NVIM_TARGET"
    echo "Created symlink: $NVIM_TARGET -> $NVIM_SOURCE"
fi

# --- Install dependencies via Homebrew ---

if command -v brew &>/dev/null; then
    brew install neovim ripgrep fd node lazygit
else
    echo "Homebrew not found — skipping dependency installation."
fi

echo "Neovim (LazyVim) installation complete!"
