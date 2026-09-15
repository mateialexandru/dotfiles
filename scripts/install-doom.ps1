# Install Doom Emacs on Windows
# Creates symlinks and installs dependencies

$dotfilesDir = Split-Path $PSScriptRoot -Parent
$doomSource = Join-Path (Join-Path $dotfilesDir "config") "doom"
$doomTarget = Join-Path $env:USERPROFILE ".config\doom"

Write-Host "Installing Doom Emacs..." -ForegroundColor Cyan

# Ensure .config directory exists
$configDir = Join-Path $env:USERPROFILE ".config"
if (-not (Test-Path $configDir)) {
    New-Item -ItemType Directory -Path $configDir -Force | Out-Null
    Write-Host "Created $configDir"
}

function Set-DotfilesLink {
    param(
        [Parameter(Mandatory)][string]$Source,
        [Parameter(Mandatory)][string]$Target,
        [Parameter(Mandatory)][string]$Label,
        [switch]$Optional
    )

    $item = Get-Item -LiteralPath $Target -Force -ErrorAction SilentlyContinue
    if ($item -and $item.LinkType -eq "SymbolicLink") {
        $current = [string]$item.Target
        if ($current -eq $Source) {
            Write-Host "$Label symlink already correct." -ForegroundColor Green
            return
        }
        Write-Host "Repairing $Label symlink ($current -> $Source)..." -ForegroundColor Yellow
        Remove-Item -LiteralPath $Target -Force
    } elseif ($item) {
        Write-Host "Backing up existing $Label config to $Target.backup..." -ForegroundColor Yellow
        Move-Item -LiteralPath $Target -Destination "$Target.backup"
    }

    try {
        New-Item -ItemType SymbolicLink -Path $Target -Target $Source -ErrorAction Stop | Out-Null
        Write-Host "Created symlink: $Target -> $Source" -ForegroundColor Green
    } catch {
        if (-not $Optional) { throw }
        Write-Host "$Label symlink failed (need admin or Developer Mode)." -ForegroundColor Yellow
    }
}

Set-DotfilesLink -Source $doomSource -Target $doomTarget -Label "Doom"

# Ctags configuration symlink (Universal CTags uses ctags.d, no dot)
$ctagsSource = Join-Path (Join-Path $dotfilesDir "config") "ctags"
$ctagsTarget = Join-Path $env:USERPROFILE "ctags.d"
if (Test-Path $ctagsSource) {
    Set-DotfilesLink -Source $ctagsSource -Target $ctagsTarget -Label "Ctags" -Optional
}

# Install dependencies and set up Doom
$setupScript = Join-Path $PSScriptRoot "setup-doom.ps1"
if (Test-Path $setupScript) {
    & $setupScript
} else {
    Write-Host "setup-doom.ps1 not found at $setupScript" -ForegroundColor Yellow
}

Write-Host "Doom Emacs installation complete!" -ForegroundColor Green
