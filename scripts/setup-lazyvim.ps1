# Setup script for LazyVim (Neovim) on Windows
# Installs Neovim and dependencies via winget

Write-Host "Installing Neovim and dependencies..." -ForegroundColor Cyan

$packages = @(
    "Neovim.Neovim",
    "Git.Git",
    "OpenJS.NodeJS.LTS",
    "JesseDuffield.lazygit",
    "BurntSushi.ripgrep.MSVC",
    "sharkdp.fd",
    "LLVM.LLVM",
    "DEVCOM.JetBrainsMonoNerdFont"
)

foreach ($pkg in $packages) {
    Write-Host "  Installing $pkg..." -ForegroundColor Gray
    winget install --id $pkg -e --accept-source-agreements --accept-package-agreements
}

Write-Host "LazyVim dependencies installed." -ForegroundColor Green
