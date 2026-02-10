#!/usr/bin/env bash
# Install Doom Emacs on Linux
# Creates symlinks, installs dependencies, and sets up Doom

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
DOOM_SOURCE="$DOTFILES_DIR/doom"
DOOM_TARGET="$HOME/.config/doom"
DOOM_EMACS="$HOME/.config/emacs"

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

# --- Install dependencies via Homebrew ---

if command -v brew &>/dev/null; then
    brew install ripgrep fd shellcheck pandoc
else
    echo "Homebrew not found — skipping dependency installation."
fi

# --- Install Doom Emacs ---

if [ ! -d "$DOOM_EMACS" ]; then
    echo "Cloning Doom Emacs..."
    git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_EMACS"
    "$DOOM_EMACS/bin/doom" install
else
    echo "Doom Emacs already installed at $DOOM_EMACS"
fi

# --- Install Roslyn LSP ---
echo "Installing Roslyn LSP (C# language server)..."
bash "$DOTFILES_DIR/scripts/install-roslyn-lsp.sh"

# --- Sync doom config ---
echo "Running doom sync..."
"$DOOM_EMACS/bin/doom" sync

echo "Doom Emacs installation complete!"
