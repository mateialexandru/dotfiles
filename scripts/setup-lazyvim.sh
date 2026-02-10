#!/usr/bin/env bash
# Setup script for LazyVim (Neovim) on Linux
# Installs Neovim and dependencies via Homebrew

set -euo pipefail

if ! command -v brew &>/dev/null; then
    echo "Homebrew not found. Install it first: https://brew.sh"
    exit 1
fi

echo "Installing Neovim and dependencies via Homebrew..."
brew install neovim ripgrep fd node lazygit

echo "LazyVim dependencies installed."
