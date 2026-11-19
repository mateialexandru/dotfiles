#Requires -Version 7.3
param(
    [string]$ProgramsDirectory = [Environment]::GetFolderPath('Programs')
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\emacs-windows.ps1"

if (-not $ProgramsDirectory) {
    throw 'The current-user Start menu Programs directory is unavailable.'
}
$runtime = Get-DotfilesEmacsRuntime
if (-not (Test-Path -LiteralPath $runtime.GuiExecutable -PathType Leaf)) {
    throw 'The accepted Emacs GUI launcher is missing. Run scripts\build-emacs-windows.ps1.'
}
foreach ($clientPath in @($runtime.Client, $runtime.GuiClient)) {
    if (-not (Test-Path -LiteralPath $clientPath -PathType Leaf)) {
        throw "The accepted Emacs client is missing: $clientPath. Run scripts\build-emacs-windows.ps1."
    }
}
$doomDirectory = Join-Path $env:USERPROFILE '.config\emacs'
if (-not (Test-Path -LiteralPath (Join-Path $doomDirectory 'early-init.el') -PathType Leaf)) {
    throw "Doom is not set up at $doomDirectory. Run install.ps1 first."
}

$powershell = Join-Path $PSHOME 'pwsh.exe'
$launcher = Join-Path $PSScriptRoot 'emacs-windows.ps1'
New-Item -ItemType Directory -Path $ProgramsDirectory -Force | Out-Null
$shell = New-Object -ComObject WScript.Shell
$shortcutPath = Join-Path $ProgramsDirectory 'Doom Emacs.lnk'
$shortcut = $shell.CreateShortcut($shortcutPath)
$shortcut.TargetPath = $powershell
$shortcut.Arguments = "-NoLogo -NoProfile -WindowStyle Hidden -File `"$launcher`" -DoomClient"
$shortcut.WorkingDirectory = $env:USERPROFILE
$shortcut.IconLocation = "$($runtime.GuiExecutable),0"
$shortcut.Description = 'Connect to Doom Emacs, starting a daemon if needed'
$shortcut.Save()
Write-Host "Start menu shortcut ready: $shortcutPath" -ForegroundColor Green
