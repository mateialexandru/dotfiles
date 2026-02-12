# install-claude.ps1 - Configure Claude Code settings (Windows toast notifications)
# Requires: Install-PSResource (ships with pwsh 7.4+)

$dotfilesDir = Split-Path -Parent $PSScriptRoot

# Install BurntToast if missing
if (-not (Get-Module -ListAvailable BurntToast)) {
    Write-Host "Installing BurntToast module..." -ForegroundColor Cyan
    Install-PSResource BurntToast -Scope CurrentUser -TrustRepository
    Write-Host "BurntToast installed." -ForegroundColor Green
} else {
    Write-Host "BurntToast already installed" -ForegroundColor Gray
}
$notifyScript = Join-Path (Join-Path $dotfilesDir "shell") "notify.ps1"
$claudeDir = Join-Path $env:USERPROFILE ".claude"
$settingsFile = Join-Path $claudeDir "settings.json"

if (-not (Test-Path $notifyScript)) {
    Write-Host "Error: notify.ps1 not found at $notifyScript" -ForegroundColor Red
    exit 1
}

# Create .claude directory if needed
if (-not (Test-Path $claudeDir)) {
    New-Item -ItemType Directory -Path $claudeDir -Force | Out-Null
    Write-Host "Created $claudeDir" -ForegroundColor Gray
}

# Build the hook command with forward slashes for reliability
$notifyScriptUri = $notifyScript -replace '\\','/'
$hookCommand = "powershell.exe -NoProfile -Command `". '$notifyScriptUri'; Invoke-ClaudeNotification`""

$settings = @{
    hooks = @{
        Notification = @(
            @{
                matcher = ""
                hooks = @(
                    @{
                        type = "command"
                        command = $hookCommand
                    }
                )
            }
        )
    }
}

if (Test-Path $settingsFile) {
    Write-Host "Claude settings already exists at $settingsFile - skipping" -ForegroundColor Gray
    Write-Host "To overwrite, delete the file and re-run this script" -ForegroundColor Gray
    exit 0
}

$settings | ConvertTo-Json -Depth 5 | Set-Content -Path $settingsFile -Encoding UTF8
Write-Host "Created Claude Code settings: $settingsFile" -ForegroundColor Green
Write-Host "Notification hook will send Windows toasts when Claude needs attention" -ForegroundColor Gray
