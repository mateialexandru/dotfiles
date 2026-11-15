#!/usr/bin/env bash
# Install Libron's desktop TTF release for the current macOS user.
set -euo pipefail

LIBRON_VERSION="${LIBRON_VERSION:-v0.25}"
LIBRON_FONT_DIR="$HOME/Library/Fonts"

if [[ -f "$LIBRON_FONT_DIR/Libron-Regular.ttf" \
   && -f "$LIBRON_FONT_DIR/Libron-Italic.ttf" \
   && -f "$LIBRON_FONT_DIR/Libron-Bold.ttf" \
   && -f "$LIBRON_FONT_DIR/Libron-BoldItalic.ttf" ]]; then
    echo "=> Libron already installed — skipping."
    exit 0
fi

LIBRON_WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$LIBRON_WORK_DIR"' EXIT

curl -fsSL -o "$LIBRON_WORK_DIR/Libron.zip" \
    "https://github.com/nicoverbruggen/libron/releases/download/$LIBRON_VERSION/Libron.zip"
mkdir -p "$LIBRON_FONT_DIR"
unzip -j -o "$LIBRON_WORK_DIR/Libron.zip" '*.ttf' -d "$LIBRON_FONT_DIR" >/dev/null
echo "=> Installed Libron $LIBRON_VERSION in $LIBRON_FONT_DIR."
