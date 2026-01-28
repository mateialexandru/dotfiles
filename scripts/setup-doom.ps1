# Doom Emacs Windows Setup Script
# Installs required dependencies via winget (idempotent)

# Set HOME environment variable (required for Emacs to find .emacs.d/.config/emacs)
$homePath = $env:USERPROFILE
if ([System.Environment]::GetEnvironmentVariable("HOME", "User") -ne $homePath) {
    [System.Environment]::SetEnvironmentVariable("HOME", $homePath, "User")
    Write-Host "HOME environment variable set to: $homePath"
} else {
    Write-Host "HOME environment variable already set."
}

# Install dependencies (winget is idempotent - skips if already installed)
Write-Host "`nInstalling dependencies..."
winget install --id GNU.Emacs -e --accept-source-agreements --accept-package-agreements
winget install --id BurntSushi.ripgrep.MSVC -e --accept-source-agreements --accept-package-agreements
winget install --id sharkdp.fd -e --accept-source-agreements --accept-package-agreements
winget install --id LLVM.LLVM -e --accept-source-agreements --accept-package-agreements
winget install --id JohnMacFarlane.Pandoc -e --accept-source-agreements --accept-package-agreements
winget install --id koalaman.shellcheck -e --accept-source-agreements --accept-package-agreements
winget install --id DEVCOM.JetBrainsMonoNerdFont -e --accept-source-agreements --accept-package-agreements

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

# Always run doom install (it handles syncing if already installed)
Write-Host 'Running doom install...' -ForegroundColor Cyan
powershell -ExecutionPolicy Bypass -File "$doomBin\doom.ps1" install

# Configure Git bash for POSIX compatibility
$configFile = Join-Path $env:HOME '.config\doom\config.el'
$bashConfig = "`n;; Windows: Use Git bash for POSIX compatibility (works with Windows paths)`n(setq shell-file-name `"C:/Program Files/Git/bin/bash.exe`")"
if (Test-Path $configFile) {
    $content = Get-Content $configFile -Raw
    if ($content -notlike '*shell-file-name*') {
        Write-Host 'Configuring Git bash for Emacs...' -ForegroundColor Cyan
        Add-Content $configFile $bashConfig
    } else {
        Write-Host 'Shell already configured in config.el.' -ForegroundColor Green
    }
}

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

# Add Windows Explorer context menu entries
Write-Host 'Setting up Windows Explorer context menu...' -ForegroundColor Cyan
$emacsClient = 'C:\Program Files\Emacs\emacs-30.2\bin\emacsclientw.exe'

# Files - Open with Emacs
reg add "HKCU\Software\Classes\*\shell\OpenWithEmacs" /ve /d "Open with Emacs" /f | Out-Null
reg add "HKCU\Software\Classes\*\shell\OpenWithEmacs" /v "Icon" /d "$emacsClient,0" /f | Out-Null
reg add "HKCU\Software\Classes\*\shell\OpenWithEmacs\command" /ve /d "`"$emacsClient`" -n `"%1`"" /f | Out-Null
Write-Host '  Added: Open with Emacs (files)' -ForegroundColor Green

# Directories - Open in Emacs
reg add "HKCU\Software\Classes\Directory\shell\OpenInEmacs" /ve /d "Open in Emacs" /f | Out-Null
reg add "HKCU\Software\Classes\Directory\shell\OpenInEmacs" /v "Icon" /d "$emacsClient,0" /f | Out-Null
reg add "HKCU\Software\Classes\Directory\shell\OpenInEmacs\command" /ve /d "`"$emacsClient`" -n `"%V`"" /f | Out-Null
Write-Host '  Added: Open in Emacs (directories)' -ForegroundColor Green

# Directory background - Open in Emacs
reg add "HKCU\Software\Classes\Directory\Background\shell\OpenInEmacs" /ve /d "Open in Emacs" /f | Out-Null
reg add "HKCU\Software\Classes\Directory\Background\shell\OpenInEmacs" /v "Icon" /d "$emacsClient,0" /f | Out-Null
reg add "HKCU\Software\Classes\Directory\Background\shell\OpenInEmacs\command" /ve /d "`"$emacsClient`" -n `"%V`"" /f | Out-Null
Write-Host '  Added: Open in Emacs (folder background)' -ForegroundColor Green
'@

$encodedCommand = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($scriptBlock))
Start-Process powershell -ArgumentList "-NoExit", "-EncodedCommand", $encodedCommand
