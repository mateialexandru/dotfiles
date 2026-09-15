#!/usr/bin/env bash
# Build and install the compiled hack worktree manager.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TOOL_DIR="$DOTFILES_DIR/tools/hack"

if ! command -v cargo >/dev/null 2>&1; then
    echo "Error: cargo is required to build hack." >&2
    exit 1
fi

cargo install --locked --force --root "$HOME/.local" --path "$TOOL_DIR"
echo "Installed hack to $HOME/.local/bin/hack"

# v3 was dot-sourced into the PowerShell profile. Remove that now-dead source line.
LEGACY_PROFILE="$HOME/.config/powershell/Microsoft.PowerShell_profile.ps1"
if [[ -f "$LEGACY_PROFILE" ]] && grep -qF "$DOTFILES_DIR/shell/hack.ps1" "$LEGACY_PROFILE"; then
    CLEAN_PROFILE="$(mktemp "${LEGACY_PROFILE}.XXXXXX")"
    grep -vF "$DOTFILES_DIR/shell/hack.ps1" "$LEGACY_PROFILE" > "$CLEAN_PROFILE" || true
    mv "$CLEAN_PROFILE" "$LEGACY_PROFILE"
    echo "Removed the legacy hack.ps1 profile source."
fi
