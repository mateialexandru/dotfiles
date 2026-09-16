# Reuse working global tools regardless of their package manager.
param(
    [switch]$AllowUvDownloads,
    [string]$PyrightVersion,
    [string]$RuffVersion
)

$ErrorActionPreference = 'Stop'

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

    $version = if ($tool -eq 'pyright') { $PyrightVersion } else { $RuffVersion }
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
