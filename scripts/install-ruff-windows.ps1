param(
    [switch]$AllowDownloads,
    [string]$Version = '0.17.2'
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\emacs-windows.ps1"

function Add-EditorToolPath($Directory) {
    if (-not (Test-Path -LiteralPath $Directory -PathType Container)) {
        throw "Tool directory does not exist: $Directory"
    }
    $userPath = [Environment]::GetEnvironmentVariable('PATH', 'User')
    $machinePath = [Environment]::GetEnvironmentVariable('PATH', 'Machine')
    $existingPaths = "$machinePath;$userPath" -split ';' | ForEach-Object { $_.TrimEnd('\') }
    if ($Directory.TrimEnd('\') -notin $existingPaths) {
        $newUserPath = (@($userPath, $Directory) | Where-Object { $_ }) -join ';'
        [Environment]::SetEnvironmentVariable('PATH', $newUserPath, 'User')
        Write-Host "Added editor tools to user PATH: $Directory"
    }
    $env:PATH = Get-DotfilesProcessPath
}

function Install-Ruff([switch]$AllowDownloads, [string]$Version) {
    $binDirectory = Join-Path $env:USERPROFILE '.local\bin'
    $executable = Join-Path $binDirectory 'ruff.exe'
    $command = Get-Command ruff -ErrorAction SilentlyContinue
    if (-not $command -and (Test-Path -LiteralPath $executable)) {
        Add-EditorToolPath $binDirectory
        $command = Get-Command ruff -ErrorAction SilentlyContinue
        if (-not $command) { throw 'Existing Ruff is still unavailable after PATH repair.' }
    }
    if ($command) {
        & $command --version
        if ($LASTEXITCODE -ne 0) { throw 'Existing Ruff failed verification; repair it before continuing.' }
        Write-Host 'Already satisfied: Ruff (not upgrading or changing its package manager).'
        return
    }
    if (-not $AllowDownloads -or $Version -notmatch '^\d+\.\d+\.\d+$') {
        throw 'Ruff is missing. Authorize its WinGet package with -AllowDownloads and an exact version. No download fallback is used.'
    }

    & winget install --id astral-sh.ruff --version $Version -e --source winget --no-upgrade `
        --accept-source-agreements --accept-package-agreements
    if ($LASTEXITCODE -notin @(0, -1978335189, -1978335135)) {
        throw "WinGet Ruff installation failed or was cancelled (exit $LASTEXITCODE)."
    }
    $env:PATH = Get-DotfilesProcessPath
    $command = Get-Command ruff -ErrorAction SilentlyContinue
    if (-not $command) { throw 'WinGet completed but Ruff is unavailable on PATH.' }
    $installedVersion = & $command --version
    if ($LASTEXITCODE -ne 0 -or "$installedVersion".Trim() -ne "ruff $Version") {
        throw "Ruff verification failed: expected ruff $Version; got $installedVersion"
    }
    Write-Host "Installed and verified through WinGet: $installedVersion"
}

Install-Ruff -AllowDownloads:$AllowDownloads -Version $Version
