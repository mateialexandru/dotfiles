#!/usr/bin/env bash
# Excalidraw prerequisites for the org-excalidraw (wdavew) Doom integration.
#
# `excalidraw:' org links open the drawing JSON in the Chrome Excalidraw PWA
# (File Handling API); on save, fswatch runs the exporter to regenerate an SVG
# that org displays inline. This installs the toolchain that lane needs:
#   - fswatch            the filewatcher org-excalidraw-initialize starts
#   - excalidraw-cli     JSON → SVG converter (@swiftlysingh/excalidraw-cli): the
#                        real @excalidraw/utils exportToSvg() + bundled fonts, so
#                        bound/multi-line text lays out faithfully. No browser, no
#                        node-canvas. See ADR-008 for why we dropped excalidraw_export.
#   - ~/Documents/org/excalidraw  default org-excalidraw-directory (config.el)
#
# Chrome itself + registering it as the .excalidraw handler are GUI-only steps,
# printed at the end. macOS only. Idempotent. See docs/decisions/008-excalidraw-integration.md.

set -euo pipefail

if [[ "$OSTYPE" != darwin* ]]; then
    echo "install-excalidraw-mac.sh: macOS only — skipping."
    exit 0
fi

EXCALIDRAW_DIR="$HOME/Documents/org/excalidraw"

install_excalidraw_prereqs() {
    # Filewatcher for org-excalidraw's file-notify watch.
    brew install fswatch

    # Faithful JSON → SVG converter (real Excalidraw renderer, no native build).
    # Node CLI; `npm i -g` is idempotent. Needs Node >= 20.19 (see .nvmrc/brew node).
    npm install -g @swiftlysingh/excalidraw-cli

    # Retire the old excalidraw_export toolchain if a previous install left it —
    # the node-canvas host dir, its PATH symlink, and the Virgil/Cascadia fonts
    # (excalidraw-cli bundles its own). Keeps a stale binary from winning on PATH.
    rm -f "$(brew --prefix)/bin/excalidraw_export"
    rm -rf "$HOME/.local/share/excalidraw-export"
    rm -f "$HOME/Library/Fonts/Virgil.woff2" "$HOME/Library/Fonts/Cascadia.woff2"

    # Default org-excalidraw-directory (config.el).
    mkdir -p "$EXCALIDRAW_DIR"
}

install_excalidraw_prereqs

cat <<'NEXT'
Excalidraw prereqs installed. Manual (GUI, one-time) steps left:

  1. Install Chrome, open https://excalidraw.com → install as PWA
     (address-bar install icon). Creates ~/Applications/Chrome Apps/Excalidraw.app.
  2. First .excalidraw open → Chrome prompts "open .excalidraw files" → Allow
     (or chrome://apps → Excalidraw → App info → enable file types).
  3. Finder: any .excalidraw → Get Info → Open with: Excalidraw.app → Change All.
  4. Verify: `open <file>.excalidraw` launches the PWA with the file loaded.

Then in Emacs: M-x org-excalidraw-create-drawing.
NEXT
