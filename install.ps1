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

# Ctags configuration symlink (Universal CTags uses ctags.d, no dot)
# Note: Symlink is optional - Citre uses explicit --options= path
$ctagsSource = Join-Path $dotfilesDir "ctags.d"
$ctagsTarget = Join-Path $env:USERPROFILE "ctags.d"
if (Test-Path $ctagsSource) {
    if (Test-Path $ctagsTarget) {
        $item = Get-Item $ctagsTarget -Force
        if ($item.LinkType -eq "SymbolicLink") {
            Write-Host "Ctags symlink already exists." -ForegroundColor Green
        } else {
            Write-Host "Backing up existing ctags config to ctags.d.backup..." -ForegroundColor Yellow
            Rename-Item $ctagsTarget "$ctagsTarget.backup"
            try {
                New-Item -ItemType SymbolicLink -Path $ctagsTarget -Target $ctagsSource -ErrorAction Stop | Out-Null
                Write-Host "Created symlink: $ctagsTarget -> $ctagsSource" -ForegroundColor Green
            } catch {
                Write-Host "Symlink failed (need admin or Developer Mode). Citre will still work." -ForegroundColor Yellow
            }
        }
    } else {
        try {
            New-Item -ItemType SymbolicLink -Path $ctagsTarget -Target $ctagsSource -ErrorAction Stop | Out-Null
            Write-Host "Created symlink: $ctagsTarget -> $ctagsSource" -ForegroundColor Green
        } catch {
            Write-Host "Symlink failed (need admin or Developer Mode). Citre will still work." -ForegroundColor Yellow
        }
    }
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
