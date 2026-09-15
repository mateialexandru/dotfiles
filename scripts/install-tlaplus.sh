#!/usr/bin/env bash
# Installs TLA+ tools: tla2tools.jar (TLC, SANY, PlusCal, TLA2TeX) + pmer/tla-bin wrappers
set -euo pipefail

VERSION="${1:-1.8.0}"
PREFIX="$HOME/.local"
JAR_PATH="$PREFIX/lib/tla2tools.jar"
TLC_BIN="$PREFIX/bin/tlc"

if [ -f "$JAR_PATH" ] && [ -x "$TLC_BIN" ]; then
    echo "TLA+ tools already installed at $PREFIX (jar: $JAR_PATH)"
    echo "To reinstall, delete $JAR_PATH and $PREFIX/bin/{tlc,pcal,tlatex,sany,tlarepl} and run again."
    exit 0
fi

if ! command -v java &>/dev/null; then
    echo "ERROR: java not found on PATH — install.sh's brew step should have provided it."
    exit 1
fi

TEMP_DIR=$(mktemp -d)
trap 'rm -rf "$TEMP_DIR"' EXIT

echo "Installing TLA+ tools v$VERSION..."
echo "  Downloading tla2tools.jar..."
curl -fSL "https://github.com/tlaplus/tlaplus/releases/download/v${VERSION}/tla2tools.jar" \
    -o "$TEMP_DIR/tla2tools.jar"

echo "  Cloning pmer/tla-bin wrappers..."
git clone --depth 1 https://github.com/pmer/tla-bin.git "$TEMP_DIR/tla-bin"
cp "$TEMP_DIR/tla2tools.jar" "$TEMP_DIR/tla-bin/"

mkdir -p "$PREFIX/bin" "$PREFIX/lib"
( cd "$TEMP_DIR/tla-bin" && ./install.sh "$PREFIX" )

if [ ! -x "$TLC_BIN" ] || [ ! -f "$JAR_PATH" ]; then
    echo "  ERROR: install verification failed (missing $TLC_BIN or $JAR_PATH)"
    exit 1
fi

echo "  Installed: tlc, pcal, tlatex, sany, tlarepl → $PREFIX/bin"
echo "  Jar: $JAR_PATH"

if ! command -v tlc &>/dev/null; then
    echo
    echo "  NOTE: $PREFIX/bin is not on \$PATH. Add to your shell rc:"
    echo "    export PATH=\"\$HOME/.local/bin:\$PATH\""
fi
