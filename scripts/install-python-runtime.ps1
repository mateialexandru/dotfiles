#Requires -Version 7.3
[CmdletBinding()]
param(
    [switch]$Check,
    [switch]$AllowDownloads,
    [ValidatePattern('^3\.(1[2-9]|[2-9][0-9])$')][string]$PythonSeries = '3.13'
)

$ErrorActionPreference = 'Stop'
if (-not $IsWindows -or [Runtime.InteropServices.RuntimeInformation]::OSArchitecture -notin @('X64', 'Arm64')) {
    throw 'This bootstrap requires 64-bit Windows.'
}
if ($Check -and $AllowDownloads) { throw '-Check cannot be combined with -AllowDownloads.' }
. "$PSScriptRoot\emacs-paths.ps1"
. "$PSScriptRoot\uv-security.ps1"
. "$PSScriptRoot\python-windows.ps1"
$env:PATH = Get-DotfilesProcessPath
$managerConfig = Join-Path $PSScriptRoot '..\config\python\windows-manager.json'

function Install-OfficialPackage {
    param([string]$Id, [string]$Source)
    if (-not $AllowDownloads) {
        throw "$Id is missing. Rerun scripts\install-python-runtime.ps1 -AllowDownloads to authorize provisioning."
    }
    & winget install --id $Id --exact --source $Source --no-upgrade `
        --accept-source-agreements --accept-package-agreements --disable-interactivity | Out-Host
    if ($LASTEXITCODE -notin @(0, -1978335189, -1978335135)) {
        throw "WinGet failed for $Id (exit $LASTEXITCODE). No alternate source was attempted."
    }
    $env:PATH = Get-DotfilesProcessPath
}

$python = Get-DotfilesPython -AllowMissing:(-not $Check)
if (-not $python) {
    $manager = Get-Command pymanager.exe -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if (-not $manager) {
        Install-OfficialPackage '9NQ7512CXL7T' 'msstore'
        $manager = Get-Command pymanager.exe -CommandType Application -ErrorAction Stop |
            Select-Object -First 1
    }
    if (-not $AllowDownloads) { throw 'Installing a Python runtime requires -AllowDownloads.' }
    # pymanager is unambiguous even when the legacy py launcher is installed.
    $tag = if ([Runtime.InteropServices.RuntimeInformation]::OSArchitecture -eq 'Arm64') {
        "$PythonSeries-arm64"
    } else { "$PythonSeries-64" }
    & $manager.Source install --source https://www.python.org/ftp/python/index-windows.json `
        --config $managerConfig -y $tag | Out-Host
    if ($LASTEXITCODE -ne 0) { throw "Official Python runtime installation failed (exit $LASTEXITCODE)." }
    $pythonPath = & $manager.Source list --one --format=exe $tag
    if ($LASTEXITCODE -ne 0 -or @($pythonPath).Count -ne 1) {
        throw 'Python Install Manager did not identify one installed interpreter.'
    }
    $python = Get-DotfilesPython -Python $pythonPath.Trim()
}

if (-not (Get-Command uv -CommandType Application -ErrorAction SilentlyContinue)) {
    if ($Check) { throw 'Astral uv is missing. Run scripts\install-python-runtime.ps1 -AllowDownloads.' }
    Install-OfficialPackage 'astral-sh.uv' 'winget'
}
if (-not $Check) { Repair-UvWindowsSelection }
Assert-DotfilesUv
Set-DotfilesPythonPath -Python $python.executable -Check:$Check
$env:PATH = Get-DotfilesProcessPath
Set-DotfilesPythonPolicy -Check:$Check
Assert-DotfilesUvPython
Write-Host "Official Python $($python.version): $($python.executable)"
Write-Host 'Astral uv/uvx verified; uv uses installed Python without automatic runtime downloads.'
