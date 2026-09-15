# Doom Emacs Windows Setup Script
# Installs required dependencies via winget (idempotent)

param(
    [switch]$AddDefenderExclusions  # Add Windows Defender exclusions (requires admin)
)

# Windows Defender Exclusions for Emacs Performance
# Emacs compiles many .elc files which triggers constant AV scanning
function Add-EmacsDefenderExclusions {
    $paths = @(
        (Join-Path $env:USERPROFILE ".config\emacs")
        (Join-Path $env:USERPROFILE ".emacs.d")
        "C:\Program Files\Emacs"
    )

    Write-Host "`nConfiguring Windows Defender exclusions..." -ForegroundColor Cyan

    # Check if running as admin
    $isAdmin = ([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
    if (-not $isAdmin) {
        Write-Host "ERROR: Adding Defender exclusions requires Administrator privileges." -ForegroundColor Red
        Write-Host "Re-run this script as Administrator with -AddDefenderExclusions" -ForegroundColor Yellow
        return
    }

    foreach ($path in $paths) {
        if (Test-Path $path) {
            try {
                Add-MpPreference -ExclusionPath $path -ErrorAction Stop
                Write-Host "  Added exclusion: $path" -ForegroundColor Green
            } catch {
                Write-Host "  Failed to add: $path - $($_.Exception.Message)" -ForegroundColor Red
            }
        } else {
            Write-Host "  Skipped (not found): $path" -ForegroundColor DarkGray
        }
    }

    Write-Host "Done! Emacs should now be faster." -ForegroundColor Green
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

Write-Host "`nStarting new shell to clone and install Doom Emacs..."

# Start new PowerShell instance with updated environment to clone and install Doom
$scriptBlock = @'
$doomDir = Join-Path $env:HOME '.config\emacs'
$doomBin = Join-Path $doomDir 'bin'

# Clone only if not already present
if (-not (Test-Path $doomDir)) {
    Write-Host 'Cloning Doom Emacs...' -ForegroundColor Cyan
    git clone --depth 1 https://github.com/doomemacs/doomemacs $doomDir
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

# Ensure Emacs is discoverable — check PATH first, then scan common locations
$emacsExe = Get-Command emacs.exe -ErrorAction SilentlyContinue | Select-Object -First 1 -ExpandProperty Source
if (-not $emacsExe) {
    $searchPaths = @('C:\Program Files\Emacs') + @(Get-PSDrive -PSProvider FileSystem |
        Where-Object { $_.Name.Length -eq 1 -and $_.Name -ne 'C' } |
        ForEach-Object { "$($_.Root)tools" }) | Where-Object { Test-Path $_ }
    foreach ($sp in $searchPaths) {
        $emacsExe = Get-ChildItem $sp -Recurse -Filter 'emacs.exe' -ErrorAction SilentlyContinue |
            Sort-Object FullName -Descending | Select-Object -First 1 -ExpandProperty FullName
        if ($emacsExe) { break }
    }
}
if ($emacsExe) {
    $emacsBin = Split-Path $emacsExe
    if ($env:PATH -notlike "*$emacsBin*") {
        Write-Host "Adding Emacs to PATH: $emacsBin" -ForegroundColor Cyan
        $env:PATH = "$emacsBin;$env:PATH"
    }
} else {
    Write-Host 'WARNING: emacs.exe not found — doom install may fail' -ForegroundColor Yellow
}

# Install Doom if needed, then synchronize the declared configuration. Upgrades
# remain an explicit operation so a rebuild does not unexpectedly move versions.
$doomLocal = Join-Path $env:HOME '.config\emacs\.local'
if (-not (Test-Path $doomLocal)) {
    Write-Host 'Running doom install...' -ForegroundColor Cyan
    powershell -ExecutionPolicy Bypass -File "$doomBin\doom.ps1" install
}
Write-Host 'Syncing Doom config...' -ForegroundColor Cyan
powershell -ExecutionPolicy Bypass -File "$doomBin\doom.ps1" sync

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
if ($csharpierInstalled) {
    Write-Host 'CSharpier already installed.' -ForegroundColor Green
} else {
    Write-Host 'Installing CSharpier...' -ForegroundColor Cyan
    dotnet tool install --global csharpier
}

# Platform settings live in the repository's config-windows.el. Never append
# machine setup to the symlinked public config, which would dirty the checkout.
$configFile = Join-Path $env:HOME '.config\doom\config.el'

# Configure fonts
$fontConfig = @"

;; Font configuration
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 14)
      doom-variable-pitch-font (font-spec :family "Segoe UI" :size 15)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))
"@
if (Test-Path $configFile) {
    $content = Get-Content $configFile -Raw
    if ($content -notlike '*doom-font*') {
        Write-Host 'Configuring fonts for Emacs...' -ForegroundColor Cyan
        Add-Content $configFile $fontConfig
    } else {
        Write-Host 'Fonts already configured in config.el.' -ForegroundColor Green
    }
}

# Configure Emacs server for emacsclient
$serverConfig = @"

;; Start Emacs server for emacsclient support (context menu integration)
(unless (and (boundp 'server-process) server-process)
  (server-start))
"@
if (Test-Path $configFile) {
    $content = Get-Content $configFile -Raw
    if ($content -notlike '*server-start*') {
        Write-Host 'Enabling Emacs server...' -ForegroundColor Cyan
        Add-Content $configFile $serverConfig
    } else {
        Write-Host 'Emacs server already configured.' -ForegroundColor Green
    }
}

# Add Windows Explorer context menu entries using PowerShell registry commands
Write-Host 'Setting up Windows Explorer context menu...' -ForegroundColor Cyan
# Discover emacsclientw.exe — same directory as emacs.exe if found, otherwise scan
$emacsClient = $null
if ($emacsBin) {
    $emacsClient = Join-Path $emacsBin 'emacsclientw.exe'
    if (-not (Test-Path $emacsClient)) { $emacsClient = $null }
}
if (-not $emacsClient) {
    $emacsClient = Get-Command emacsclientw.exe -ErrorAction SilentlyContinue | Select-Object -First 1 -ExpandProperty Source
}
if (-not $emacsClient) {
    Write-Host 'WARNING: emacsclientw.exe not found - skipping context menu setup' -ForegroundColor Yellow
} else {
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
Set-ContextMenu 'Software\Classes\*\shell\OpenWithEmacs' 'Open with Emacs' "`"$emacsClient`" -n `"%1`""
Write-Host '  Added: Open with Emacs (files)' -ForegroundColor Green

# Directories - Open in Emacs
Set-ContextMenu 'Software\Classes\Directory\shell\OpenInEmacs' 'Open in Emacs' "`"$emacsClient`" -n `"%V`""
Write-Host '  Added: Open in Emacs (directories)' -ForegroundColor Green

# Directory background - Open in Emacs
Set-ContextMenu 'Software\Classes\Directory\Background\shell\OpenInEmacs' 'Open in Emacs' "`"$emacsClient`" -n `"%V`""
Write-Host '  Added: Open in Emacs (folder background)' -ForegroundColor Green
}

Write-Host "`nSetup complete! Closing in 10 seconds (press any key to close now)..." -ForegroundColor Cyan
$timeout = 10
for ($i = $timeout; $i -gt 0; $i--) {
    if ([Console]::KeyAvailable) { $null = [Console]::ReadKey($true); break }
    Start-Sleep -Seconds 1
}
'@

$encodedCommand = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($scriptBlock))
Start-Process powershell -ArgumentList "-ExecutionPolicy", "Bypass", "-EncodedCommand", $encodedCommand -Wait
