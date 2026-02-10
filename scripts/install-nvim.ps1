# Install Neovim (LazyVim) on Windows
# Creates symlink and installs dependencies

$dotfilesDir = Split-Path $PSScriptRoot -Parent
$nvimSource = Join-Path $dotfilesDir "nvim"
$nvimTarget = Join-Path $env:LOCALAPPDATA "nvim"

Write-Host "Installing Neovim (LazyVim)..." -ForegroundColor Cyan

# Create Neovim symlink
if (Test-Path $nvimTarget) {
    $item = Get-Item $nvimTarget -Force
    if ($item.LinkType -eq "SymbolicLink") {
        Write-Host "Neovim symlink already exists." -ForegroundColor Green
    } else {
        Write-Host "Backing up existing nvim config to nvim.backup..." -ForegroundColor Yellow
        Rename-Item $nvimTarget "$nvimTarget.backup"
        try {
            New-Item -ItemType SymbolicLink -Path $nvimTarget -Target $nvimSource -ErrorAction Stop | Out-Null
            Write-Host "Created symlink: $nvimTarget -> $nvimSource" -ForegroundColor Green
        } catch {
            Write-Host "Symlink failed (need admin or Developer Mode)." -ForegroundColor Yellow
        }
    }
} else {
    try {
        New-Item -ItemType SymbolicLink -Path $nvimTarget -Target $nvimSource -ErrorAction Stop | Out-Null
        Write-Host "Created symlink: $nvimTarget -> $nvimSource" -ForegroundColor Green
    } catch {
        Write-Host "Symlink failed (need admin or Developer Mode)." -ForegroundColor Yellow
    }
}

# Install dependencies
$setupScript = Join-Path $PSScriptRoot "setup-lazyvim.ps1"
if (Test-Path $setupScript) {
    & $setupScript
} else {
    Write-Host "setup-lazyvim.ps1 not found at $setupScript" -ForegroundColor Yellow
}

Write-Host "Neovim (LazyVim) installation complete!" -ForegroundColor Green
