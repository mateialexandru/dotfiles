# Dotfiles Install Script
# Installs all editors and tools

$dotfilesDir = $PSScriptRoot

Write-Host "Installing dotfiles from: $dotfilesDir" -ForegroundColor Cyan

# Install Doom Emacs
& (Join-Path $dotfilesDir "scripts\install-doom.ps1")

# Install Neovim (LazyVim)
& (Join-Path $dotfilesDir "scripts\install-nvim.ps1")

Write-Host "`nDotfiles installation complete!" -ForegroundColor Green
