#!/usr/bin/env bash
# Upgrade the packages and tools managed by this dotfiles checkout.

set -euo pipefail

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
DOOM="$HOME/.config/emacs/bin/doom"

if [[ $# -ne 0 ]]; then
    echo "Usage: sys update" >&2
    exit 2
fi

step() {
    echo
    echo "==> $*"
}

step "Homebrew packages"
brew update
brew upgrade

step "Global npm tools"
npm update --global

step "uv tools"
uv tool upgrade --all

if command -v rustup >/dev/null 2>&1; then
    step "Rust toolchain"
    rustup update
fi

step "C# formatter"
dotnet tool update --global csharpier || dotnet tool install --global csharpier

step "Doom Emacs and packages"
"$DOOM" upgrade

step "Compiled worktree tool"
bash "$DOTFILES_DIR/scripts/install-hack.sh"

step "Nushell integrations"
bash "$DOTFILES_DIR/scripts/install-nushell.sh"

if [[ "$OSTYPE" == darwin* ]]; then
    step "Emacs daemon"
    bash "$DOTFILES_DIR/scripts/restart-emacs-mac.sh" 0

    step "Ghostel native module"
    bash "$DOTFILES_DIR/scripts/install-ghostel-module.sh"
fi

step "Environment check"
bash "$DOTFILES_DIR/scripts/sys-health.sh"

echo
echo "==> Update complete."
