#!/usr/bin/env bash
# Install the PlantUML jar where Doom's :lang plantuml module expects it.

set -euo pipefail

TARGET="$HOME/.config/emacs/.local/etc/plantuml.jar"
URL="https://github.com/plantuml/plantuml/releases/latest/download/plantuml.jar"

if [[ -f "$TARGET" ]]; then
    echo "PlantUML jar already installed at $TARGET"
    exit 0
fi

echo "Installing PlantUML jar..."
mkdir -p "$(dirname "$TARGET")"
curl -fL "$URL" -o "$TARGET"
echo "Installed PlantUML jar: $TARGET"
