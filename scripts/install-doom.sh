#!/usr/bin/env bash
# Doom Emacs setup: symlink config, install fonts (Linux), bootstrap Doom, sync.
# Called by install.sh — packages and C# tooling are handled there.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
DOOM_SOURCE="$DOTFILES_DIR/doom"
DOOM_TARGET="$HOME/.config/doom"
DOOM_EMACS="$HOME/.config/emacs"

# --- Symlink doom config ---

if [ -L "$DOOM_TARGET" ]; then
    CURRENT="$(readlink "$DOOM_TARGET")"
    if [ "$CURRENT" = "$DOOM_SOURCE" ]; then
        echo "Doom symlink correct: $DOOM_TARGET -> $DOOM_SOURCE"
    else
        echo "Repairing Doom symlink ($DOOM_TARGET -> $CURRENT, expected $DOOM_SOURCE)..."
        rm "$DOOM_TARGET"
        ln -s "$DOOM_SOURCE" "$DOOM_TARGET"
    fi
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

# --- Install fonts (Linux only — macOS handled via brew cask in install.sh) ---

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    FONT_DIR="$HOME/.local/share/fonts/JetBrainsMono"
    if [ -d "$FONT_DIR" ] && ls "$FONT_DIR"/*.ttf &>/dev/null; then
        echo "JetBrains Mono NF already installed."
    else
        echo "Installing JetBrains Mono Nerd Font..."
        FONT_VERSION="v3.3.0"
        FONT_URL="https://github.com/ryanoasis/nerd-fonts/releases/download/${FONT_VERSION}/JetBrainsMono.tar.xz"
        mkdir -p "$FONT_DIR"
        curl -fsSL "$FONT_URL" | tar -xJ -C "$FONT_DIR"
        if command -v fc-cache &>/dev/null; then fc-cache -f "$FONT_DIR"; fi
        echo "JetBrains Mono NF installed."
    fi

    INTER_FONT_DIR="$HOME/.local/share/fonts/Inter"
    if [ -d "$INTER_FONT_DIR" ] && ls "$INTER_FONT_DIR"/*.ttc &>/dev/null; then
        echo "Inter font already installed."
    else
        echo "Installing Inter font..."
        INTER_VERSION="v4.1"
        INTER_URL="https://github.com/rsms/inter/releases/download/${INTER_VERSION}/Inter-4.1.zip"
        mkdir -p "$INTER_FONT_DIR"
        TMP_ZIP=$(mktemp /tmp/inter-XXXXXX.zip)
        curl -fsSL "$INTER_URL" -o "$TMP_ZIP"
        unzip -qo "$TMP_ZIP" "*.ttf" -d "$INTER_FONT_DIR" || unzip -qo "$TMP_ZIP" -d "$INTER_FONT_DIR"
        rm "$TMP_ZIP"
        if command -v fc-cache &>/dev/null; then fc-cache -f "$INTER_FONT_DIR"; fi
        echo "Inter font installed."
    fi
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

echo "Doom Emacs setup complete!"
