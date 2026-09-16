# The WinGet Gnuplot package declares machine scope and can request elevation.
param(
    [Parameter(Mandatory)]
    [ValidateNotNullOrEmpty()]
    [string]$Version,
    [switch]$AllowElevation
)

$ErrorActionPreference = 'Stop'

function Add-GnuplotPath {
    # WinGet's machine-scoped installer does not always register its bin directory.
    $bin = Join-Path $env:ProgramFiles 'gnuplot\bin'
    if (Test-Path (Join-Path $bin 'gnuplot.exe')) {
        $userPath = [Environment]::GetEnvironmentVariable('PATH', 'User')
        if (($userPath -split ';') -notcontains $bin) {
            [Environment]::SetEnvironmentVariable('PATH', "$userPath;$bin", 'User')
        }
        if (($env:PATH -split ';') -notcontains $bin) { $env:PATH = "$env:PATH;$bin" }
    }
}

Add-GnuplotPath
$command = Get-Command gnuplot -ErrorAction SilentlyContinue
if ($command) {
    $versionOutput = & $command --version
    if ($LASTEXITCODE -ne 0) { throw 'Installed Gnuplot failed its version check.' }
    Write-Host "Already satisfied: $versionOutput. Existing version preserved; no upgrade requested."
    return
}
if (-not $AllowElevation) {
    throw 'Gnuplot requires a machine-wide WinGet install. Review the version and rerun with -AllowElevation only if permitted.'
}

Write-Host "Installing Gnuplot $Version through WinGet (machine scope; elevation expected)."
& winget install --id gnuplot.gnuplot --version $Version --source winget --exact `
    --scope machine --no-upgrade --accept-source-agreements --accept-package-agreements
if ($LASTEXITCODE -notin @(0, -1978335135)) {
    throw "Gnuplot installation failed or was cancelled (exit $LASTEXITCODE). No retry will be attempted."
}

$env:PATH = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';' +
    [Environment]::GetEnvironmentVariable('PATH', 'User')
Add-GnuplotPath
& gnuplot --version
if ($LASTEXITCODE -ne 0) { throw 'Gnuplot failed verification after installation.' }
