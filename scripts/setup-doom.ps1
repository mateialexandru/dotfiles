#Requires -Version 7.3
# Doom Emacs Windows Setup Script
# Installs required dependencies via winget (idempotent)

param(
    [switch]$AddDefenderExclusions  # Add Windows Defender exclusions (requires admin)
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\emacs-paths.ps1"

# Windows Defender Exclusions for Emacs Performance
# Emacs compiles many .elc files which triggers constant AV scanning
function Add-EmacsDefenderExclusions {
    $env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';' +
        [Environment]::GetEnvironmentVariable('PATH', 'User')
    $paths = @(Get-DotfilesEmacsPaths | Select-Object -Unique)
    if (-not $paths.Count) { throw 'No active Emacs paths found; no exclusions added.' }

    Write-Host "`nConfiguring Windows Defender exclusions..." -ForegroundColor Cyan

    # Check if running as admin
    $isAdmin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
    if (-not $isAdmin) {
        throw 'Adding approved Defender exclusions requires Administrator privileges.'
    }

    foreach ($path in $paths) {
        if (Test-Path $path) {
            try {
                $existing = @((Get-MpPreference -ErrorAction Stop).ExclusionPath)
                if ($path -notin $existing) {
                    Add-MpPreference -ExclusionPath $path -ErrorAction Stop
                }
                $confirmed = @((Get-MpPreference -ErrorAction Stop).ExclusionPath)
                if ($path -notin $confirmed) { throw "Defender did not retain exclusion: $path" }
                Write-Host "  Verified exclusion: $path" -ForegroundColor Green
            } catch {
                throw "Could not configure exclusion for $path. Stop and consult IT; do not bypass policy. $($_.Exception.Message)"
            }
        } else {
            Write-Host "  Skipped (not found): $path" -ForegroundColor DarkGray
        }
    }

    Write-Host 'Approved exclusions verified. Network protection is unchanged.' -ForegroundColor Green
}

# If -AddDefenderExclusions flag is set, just do that and exit
if ($AddDefenderExclusions) {
    Add-EmacsDefenderExclusions
    exit 0
}

# Set HOME environment variable (required for Emacs to find .emacs.d/.config/emacs)
$homePath = $env:USERPROFILE
if ([System.Environment]::GetEnvironmentVariable("HOME", "User") -ne $homePath) {
    [System.Environment]::SetEnvironmentVariable("HOME", $homePath, "User")
    Write-Host "HOME environment variable set to: $homePath"
} else {
    Write-Host "HOME environment variable already set."
}

# Dependencies are installed by install-prerequisites.ps1 (run first via install.ps1)

Write-Host "`nChecking and synchronizing Doom Emacs..."
$doomSetupInvoker = Join-Path $PSScriptRoot 'invoke-doom.ps1'
$emacsWindowsLauncher = Join-Path $PSScriptRoot 'emacs-windows.ps1'

# Run in the caller so output and failures reach the top-level installer.
$scriptBlock = @'
$ErrorActionPreference = 'Stop'
$env:HOME = $env:USERPROFILE
$env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';' +
    [Environment]::GetEnvironmentVariable('PATH', 'User')
$doomDir = Join-Path $env:HOME '.config\emacs'
$doomBin = Join-Path $doomDir 'bin'

# Clone only if not already present
if (-not (Test-Path $doomDir)) {
    Write-Host 'Cloning Doom Emacs...' -ForegroundColor Cyan
    git clone --depth 1 https://github.com/doomemacs/doomemacs $doomDir
    if ($LASTEXITCODE -ne 0) { throw 'Doom clone failed.' }
} else {
    Write-Host 'Doom Emacs already cloned.' -ForegroundColor Green
}

# Add to PATH if not already present
$currentPath = [System.Environment]::GetEnvironmentVariable('PATH', 'User')
if ($currentPath -notlike "*$doomBin*") {
    Write-Host 'Adding Doom to PATH...' -ForegroundColor Cyan
    [System.Environment]::SetEnvironmentVariable('PATH', "$currentPath;$doomBin", 'User')
    $env:PATH = "$env:PATH;$doomBin"
} else {
    Write-Host 'Doom already in PATH.' -ForegroundColor Green
}

$selectedEmacs = Get-DotfilesEmacsRuntime
$emacsExe = $selectedEmacs.Executable
if ($emacsExe) {
    $emacsBin = Split-Path $emacsExe
    if ($env:PATH -notlike "*$emacsBin*") {
        Write-Host "Adding Emacs to PATH: $emacsBin" -ForegroundColor Cyan
        $env:PATH = "$emacsBin;$env:PATH"
    }
} else {
    throw 'emacs.exe not found; cannot synchronize Doom.'
}

function Invoke-DoomSetupCommand([string]$Command) {
    & $doomSetupInvoker -Command $Command -DoomDirectory $doomDir -Force
}

# Install Doom if needed, then synchronize the declared configuration. Upgrades
# remain an explicit operation so a rebuild does not unexpectedly move versions.
$doomLocal = Join-Path $env:HOME '.config\emacs\.local'
if (-not (Test-Path $doomLocal)) {
    Write-Host 'Running doom install...' -ForegroundColor Cyan
    Invoke-DoomSetupCommand 'install'
}
Write-Host 'Syncing Doom config...' -ForegroundColor Cyan
Invoke-DoomSetupCommand 'sync'

# Doom's PlantUML module checks its profile data directory, while the shared
# prerequisite installer keeps the downloaded jar under LOCALAPPDATA.
$plantumlSource = Join-Path $env:LOCALAPPDATA 'plantuml\plantuml.jar'
$plantumlTarget = Join-Path $doomLocal 'etc\plantuml.jar'
if (Test-Path $plantumlSource) {
    New-Item -ItemType Directory -Path (Split-Path $plantumlTarget) -Force | Out-Null
    Copy-Item $plantumlSource $plantumlTarget -Force
    Write-Host "Installed Doom PlantUML jar: $plantumlTarget" -ForegroundColor Green
}

# Install CSharpier (C# formatter for apheleia)
$csharpierInstalled = dotnet tool list --global 2>$null | Select-String 'csharpier'
if ($LASTEXITCODE -ne 0) { throw 'Could not inspect global .NET tools.' }
if ($csharpierInstalled) {
    Write-Host 'CSharpier already installed.' -ForegroundColor Green
} else {
    Write-Host 'Installing CSharpier...' -ForegroundColor Cyan
    dotnet tool install --global csharpier
    if ($LASTEXITCODE -ne 0) { throw 'CSharpier installation failed.' }
}

# Platform settings live in the repository's config-windows.el. Never append
# machine setup to the symlinked public config, which would dirty the checkout.

# Add Windows Explorer context menu entries using PowerShell registry commands
Write-Host 'Setting up Windows Explorer context menu...' -ForegroundColor Cyan
$emacsClient = $selectedEmacs.GuiClient
if (-not (Test-Path -LiteralPath $emacsClient)) {
    throw 'The built Emacs GUI client is missing. Run scripts\build-emacs-windows.ps1.'
}
Write-Host "  Using: $emacsClient" -ForegroundColor Gray

# Helper to create registry key and set values
function Set-ContextMenu($keyPath, $label, $command) {
    $fullPath = "HKCU:\$keyPath"
    $cmdPath = "$fullPath\command"

    # Create keys if needed (use -LiteralPath for paths with *)
    if (-not (Test-Path -LiteralPath $fullPath)) { New-Item -Path $fullPath -Force | Out-Null }
    if (-not (Test-Path -LiteralPath $cmdPath)) { New-Item -Path $cmdPath -Force | Out-Null }

    # Set values
    Set-ItemProperty -LiteralPath $fullPath -Name '(Default)' -Value $label
    Set-ItemProperty -LiteralPath $fullPath -Name 'Icon' -Value "$emacsClient,0"
    Set-ItemProperty -LiteralPath $cmdPath -Name '(Default)' -Value $command
}

# Files - Open with Emacs
$powershell = (Get-Command pwsh.exe -ErrorAction Stop).Source
$clientCommand = "`"$powershell`" -NoProfile -WindowStyle Hidden -File `"$emacsWindowsLauncher`" -Client -Gui"
Set-ContextMenu 'Software\Classes\*\shell\OpenWithEmacs' 'Open with Emacs' "$clientCommand -n `"%1`""
Write-Host '  Added: Open with Emacs (files)' -ForegroundColor Green

# Directories - Open in Emacs
Set-ContextMenu 'Software\Classes\Directory\shell\OpenInEmacs' 'Open in Emacs' "$clientCommand -n `"%V`""
Write-Host '  Added: Open in Emacs (directories)' -ForegroundColor Green

# Directory background - Open in Emacs
Set-ContextMenu 'Software\Classes\Directory\Background\shell\OpenInEmacs' 'Open in Emacs' "$clientCommand -n `"%V`""
Write-Host '  Added: Open in Emacs (folder background)' -ForegroundColor Green
Write-Host "`nDoom setup complete." -ForegroundColor Cyan
'@

& ([scriptblock]::Create($scriptBlock))
