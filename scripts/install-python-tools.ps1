# Reuse working global tools regardless of their package manager.
param(
    [switch]$AllowUvDownloads,
    [switch]$AllowRuffDownloads,
    [string]$PyrightVersion,
    [string]$RuffVersion
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\uv-security.ps1"
Assert-UvSecurity

foreach ($tool in @('pyright', 'ruff')) {
    $command = Get-Command $tool -ErrorAction SilentlyContinue
    if ($command) {
        $versionOutput = & $command --version
        if ($LASTEXITCODE -ne 0) {
            throw "$tool is installed but its version check failed; repair it before continuing."
        }
        Write-Host "Already satisfied: $versionOutput ($($command.Source))"
        continue
    }

    if ($tool -eq 'ruff') {
        $ruffArguments = @{ AllowDownloads = $AllowRuffDownloads }
        if ($RuffVersion) { $ruffArguments.Version = $RuffVersion }
        & "$PSScriptRoot\install-ruff-windows.ps1" @ruffArguments
        continue
    }

    $version = $PyrightVersion
    if (-not $AllowUvDownloads -or $version -notmatch '^\d+\.\d+\.\d+$') {
        throw "$tool is missing. Provision it through an approved source, or explicitly authorize uv downloads with -AllowUvDownloads and an exact -${tool}Version. Do not override an organizational block."
    }
    & uv tool install "$tool==$version"
    if ($LASTEXITCODE -ne 0) {
        throw "uv installation of $tool failed or was blocked. No retry or alternate source will be attempted."
    }
    & $tool --version
    if ($LASTEXITCODE -ne 0) { throw "$tool failed verification after installation." }
}
