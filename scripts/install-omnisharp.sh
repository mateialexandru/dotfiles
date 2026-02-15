#!/usr/bin/env bash
set -euo pipefail
VERSION="${1:-1.39.13}"
INSTALL_DIR="$HOME/.local/share/omnisharp"
BINARY="$INSTALL_DIR/OmniSharp"

if [ -f "$BINARY" ]; then
    echo "OmniSharp already installed at $BINARY"; exit 0
fi
URL="https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v${VERSION}/omnisharp-linux-x64-net6.0.tar.gz"
TEMP=$(mktemp -d)
echo "Installing OmniSharp v$VERSION..."
curl -fSL "$URL" -o "$TEMP/omnisharp.tar.gz"
mkdir -p "$INSTALL_DIR"
tar -xzf "$TEMP/omnisharp.tar.gz" -C "$INSTALL_DIR"
chmod +x "$BINARY"
rm -rf "$TEMP"
echo "Installed to $BINARY  —  launch: $BINARY -lsp"
