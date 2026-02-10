#!/usr/bin/env bash
# Downloads and installs Microsoft.CodeAnalysis.LanguageServer (Roslyn LSP) for Linux
# Same C# language server that powers VS Code
set -euo pipefail

VERSION="${1:-5.0.0-1.25277.114}"
INSTALL_DIR="$HOME/.local/share/roslyn-lsp"
DLL_NAME="Microsoft.CodeAnalysis.LanguageServer.dll"
TARGET_DLL="$INSTALL_DIR/$DLL_NAME"

if [ -f "$TARGET_DLL" ]; then
    echo "Roslyn LSP already installed at $TARGET_DLL"
    echo "To reinstall, delete $INSTALL_DIR and run again."
    exit 0
fi

NUGET_URL="https://www.nuget.org/api/v2/package/Microsoft.CodeAnalysis.LanguageServer.linux-x64/$VERSION"
TEMP_DIR=$(mktemp -d)
TEMP_ZIP="$TEMP_DIR/roslyn-lsp.zip"

echo "Installing Microsoft.CodeAnalysis.LanguageServer v$VERSION..."
echo "  Downloading from NuGet..."
curl -fSL "$NUGET_URL" -o "$TEMP_ZIP"

echo "  Extracting..."
unzip -q "$TEMP_ZIP" -d "$TEMP_DIR/extract"

# Find the DLL - path varies between versions
FOUND_DLL=$(find "$TEMP_DIR/extract" -name "$DLL_NAME" -print -quit)
if [ -z "$FOUND_DLL" ]; then
    echo "  ERROR: $DLL_NAME not found in package"
    rm -rf "$TEMP_DIR"
    exit 1
fi

SERVER_DIR=$(dirname "$FOUND_DLL")
mkdir -p "$INSTALL_DIR"
cp -r "$SERVER_DIR"/* "$INSTALL_DIR/"

rm -rf "$TEMP_DIR"

if [ -f "$TARGET_DLL" ]; then
    echo "  Installed to $INSTALL_DIR"
    echo "  Launch: dotnet $TARGET_DLL --stdio"
else
    echo "  ERROR: Installation failed"
    exit 1
fi
