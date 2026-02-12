#!/usr/bin/env bash
# install-hack.sh — Adds hack.ps1 dot-source to pwsh profile on Linux

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
HACK_SCRIPT="$DOTFILES_DIR/shell/hack.ps1"

if [[ ! -f "$HACK_SCRIPT" ]]; then
    echo "Error: hack.ps1 not found at $HACK_SCRIPT" >&2
    exit 1
fi

# pwsh profile location on Linux
PWSH_PROFILE="$HOME/.config/powershell/Microsoft.PowerShell_profile.ps1"
PWSH_PROFILE_DIR="$(dirname "$PWSH_PROFILE")"

if ! command -v pwsh &>/dev/null; then
    echo "pwsh not found — skipping hack install (install PowerShell first)"
    exit 0
fi

mkdir -p "$PWSH_PROFILE_DIR"

if [[ ! -f "$PWSH_PROFILE" ]]; then
    touch "$PWSH_PROFILE"
    echo "Created pwsh profile at $PWSH_PROFILE"
fi

DOT_SOURCE_LINE=". \"$HACK_SCRIPT\""

if grep -qF "$HACK_SCRIPT" "$PWSH_PROFILE" 2>/dev/null; then
    echo "hack.ps1 already sourced in pwsh profile"
    exit 0
fi

printf '\n# Hack worktree tooling\n%s\n' "$DOT_SOURCE_LINE" >> "$PWSH_PROFILE"
echo "Added hack.ps1 to pwsh profile: $PWSH_PROFILE"
echo "Restart your shell or run: $DOT_SOURCE_LINE"
