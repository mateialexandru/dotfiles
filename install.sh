#!/usr/bin/env bash
# Dotfiles Install Script for Linux (Bluefin/Fedora)
# Installs all editors and tools

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"

# --- Homebrew packages (shared tools) ---

if command -v brew &>/dev/null; then
    echo "Installing shared packages via Homebrew..."

    # Everyday tools
    brew install --formula kanata
    brew install --cask claude-code codex copilot-cli \
        font-0xproto-nerd-font \
        font-blex-mono-nerd-font \
        font-caskaydia-mono-nerd-font \
        font-comic-shanns-mono-nerd-font \
        font-droid-sans-mono-nerd-font \
        font-fira-code-nerd-font \
        font-go-mono-nerd-font \
        font-sauce-code-pro-nerd-font \
        font-source-code-pro \
        font-ubuntu-nerd-font

    # Programming tools
    brew install cmake ninja gh grip devcontainer
else
    echo "Homebrew not found — skipping shared package installation."
    echo "Install Homebrew first: https://brew.sh"
fi

# Install Doom Emacs
bash "$DOTFILES_DIR/scripts/install-doom.sh"

# Install Neovim (LazyVim)
bash "$DOTFILES_DIR/scripts/install-nvim.sh"

# Install Hack worktree tooling
bash "$DOTFILES_DIR/scripts/install-hack.sh"

echo "Done! Dotfiles installed."
