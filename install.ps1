# Dotfiles Install Script
# Installs all editors and tools

$dotfilesDir = $PSScriptRoot

Write-Host "Installing dotfiles from: $dotfilesDir" -ForegroundColor Cyan

# Install Doom Emacs
& (Join-Path $dotfilesDir "scripts\install-doom.ps1")

# Install Neovim (LazyVim)
& (Join-Path $dotfilesDir "scripts\install-nvim.ps1")

# Install Hack worktree tooling
& (Join-Path $dotfilesDir "scripts\install-hack.ps1")

# Git performance settings (Windows-specific: fscache, parallel index, etc.)
Write-Host "`nConfiguring git for Windows performance..." -ForegroundColor Cyan
git config --global core.preloadindex true
git config --global core.fscache true
git config --global core.untrackedCache true
git config --global feature.manyFiles true
Write-Host "Git performance settings applied." -ForegroundColor Green

Write-Host "`nDotfiles installation complete!" -ForegroundColor Green
