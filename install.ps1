# Dotfiles Install Script
# Creates symlinks and runs setup

$dotfilesDir = $PSScriptRoot
$doomSource = Join-Path $dotfilesDir "doom"
$doomTarget = Join-Path $env:USERPROFILE ".config\doom"

Write-Host "Installing dotfiles from: $dotfilesDir" -ForegroundColor Cyan

# Ensure .config directory exists
$configDir = Join-Path $env:USERPROFILE ".config"
if (-not (Test-Path $configDir)) {
    New-Item -ItemType Directory -Path $configDir -Force | Out-Null
    Write-Host "Created $configDir"
}

# Create Doom symlink
if (Test-Path $doomTarget) {
    $item = Get-Item $doomTarget -Force
    if ($item.LinkType -eq "SymbolicLink") {
        Write-Host "Doom symlink already exists." -ForegroundColor Green
    } else {
        Write-Host "Backing up existing doom config to doom.backup..." -ForegroundColor Yellow
        Rename-Item $doomTarget "$doomTarget.backup"
        New-Item -ItemType SymbolicLink -Path $doomTarget -Target $doomSource | Out-Null
        Write-Host "Created symlink: $doomTarget -> $doomSource" -ForegroundColor Green
    }
} else {
    New-Item -ItemType SymbolicLink -Path $doomTarget -Target $doomSource | Out-Null
    Write-Host "Created symlink: $doomTarget -> $doomSource" -ForegroundColor Green
}

# Run setup script
$setupScript = Join-Path $dotfilesDir "scripts\setup-doom.ps1"
if (Test-Path $setupScript) {
    Write-Host "`nRunning setup-doom.ps1..." -ForegroundColor Cyan
    & $setupScript
} else {
    Write-Host "Setup script not found at $setupScript" -ForegroundColor Yellow
}

Write-Host "`nDotfiles installation complete!" -ForegroundColor Green
