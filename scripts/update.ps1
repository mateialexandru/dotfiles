# Upgrade the packages and tools managed by this dotfiles checkout.

$ErrorActionPreference = "Stop"
$dotfilesDir = Split-Path -Parent $PSScriptRoot
$doom = Join-Path $env:USERPROFILE ".config\emacs\bin\doom.ps1"

function Invoke-Native {
    param(
        [Parameter(Mandatory)][string]$Command,
        [string[]]$Arguments = @()
    )

    & $Command @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "$Command failed with exit code $LASTEXITCODE"
    }
}

Write-Host "`n==> WinGet packages" -ForegroundColor Cyan
Invoke-Native "winget" @("upgrade", "--all", "--accept-source-agreements", "--accept-package-agreements")

# Pick up paths written by MSI/WinGet installers before updating their tools.
$machinePath = [Environment]::GetEnvironmentVariable("PATH", "Machine")
$userPath = [Environment]::GetEnvironmentVariable("PATH", "User")
$env:PATH = "$machinePath;$userPath"

Write-Host "`n==> Global npm tools" -ForegroundColor Cyan
Invoke-Native "npm" @("update", "--global")

Write-Host "`n==> Pi coding agent and layered configuration" -ForegroundColor Cyan
& (Join-Path $PSScriptRoot "install-pi.ps1")

Write-Host "`n==> uv tools" -ForegroundColor Cyan
Invoke-Native "uv" @("tool", "upgrade", "--all")

Write-Host "`n==> Rust toolchain" -ForegroundColor Cyan
Invoke-Native "rustup" @("update")

Write-Host "`n==> C# formatter" -ForegroundColor Cyan
& dotnet tool update --global csharpier
if ($LASTEXITCODE -ne 0) {
    Invoke-Native "dotnet" @("tool", "install", "--global", "csharpier")
}

Write-Host "`n==> PowerShell tools" -ForegroundColor Cyan
if (Get-Command Get-InstalledPSResource -ErrorAction SilentlyContinue) {
    if (Get-InstalledPSResource BurntToast -ErrorAction SilentlyContinue) {
        Update-PSResource BurntToast -TrustRepository
    }
} elseif (Get-Module -ListAvailable BurntToast) {
    Update-Module BurntToast -Force
}

Write-Host "`n==> Doom Emacs and packages" -ForegroundColor Cyan
Invoke-Native $doom @("upgrade")

Write-Host "`n==> Compiled worktree tool" -ForegroundColor Cyan
& (Join-Path $PSScriptRoot "install-hack.ps1")

Write-Host "`n==> Nushell integrations" -ForegroundColor Cyan
& (Join-Path $PSScriptRoot "install-nushell.ps1")

Write-Host "`n==> Environment check" -ForegroundColor Cyan
& (Join-Path $PSScriptRoot "doctor.ps1")

Write-Host "`n==> Update complete." -ForegroundColor Green
