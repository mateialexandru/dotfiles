#!/usr/bin/env bash
# Dotfiles Install Script for Linux (Bluefin/Fedora)
# Creates symlinks, installs packages via Homebrew, and sets up Doom Emacs

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
DOOM_SOURCE="$DOTFILES_DIR/doom"
DOOM_TARGET="$HOME/.config/doom"
DOOM_EMACS="$HOME/.config/emacs"

# --- Homebrew packages ---

if command -v brew &>/dev/null; then
    echo "Installing packages via Homebrew..."

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

    # Doom Emacs dependencies
    brew install ripgrep fd shellcheck pandoc
else
    echo "Homebrew not found — skipping package installation."
    echo "Install Homebrew first: https://brew.sh"
fi

# --- Symlink doom config ---

if [ -L "$DOOM_TARGET" ]; then
    echo "Doom symlink already exists: $(readlink "$DOOM_TARGET")"
elif [ -d "$DOOM_TARGET" ]; then
    echo "Backing up existing doom config to $DOOM_TARGET.backup"
    mv "$DOOM_TARGET" "$DOOM_TARGET.backup"
    ln -s "$DOOM_SOURCE" "$DOOM_TARGET"
    echo "Created symlink: $DOOM_TARGET -> $DOOM_SOURCE"
else
    mkdir -p "$(dirname "$DOOM_TARGET")"
    ln -s "$DOOM_SOURCE" "$DOOM_TARGET"
    echo "Created symlink: $DOOM_TARGET -> $DOOM_SOURCE"
fi

# --- Install Doom Emacs ---

if [ ! -d "$DOOM_EMACS" ]; then
    echo "Cloning Doom Emacs..."
    git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_EMACS"
    "$DOOM_EMACS/bin/doom" install
else
    echo "Doom Emacs already installed at $DOOM_EMACS"
fi

# --- Sync doom config ---

echo "Running doom sync..."
"$DOOM_EMACS/bin/doom" sync

echo "Done! Dotfiles installed."
