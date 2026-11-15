#Requires -Version 7.3
# doom.ps1 — Doom Emacs helpers (dot-source in your PowerShell profile)
#   . "<dotfiles>/config/shell/doom.ps1"

. "$PSScriptRoot/notify.ps1"

$script:DoomBin = Join-Path $PSScriptRoot '..\..\scripts\invoke-doom.ps1'
$script:DotfilesEmacsLauncher = Join-Path $PSScriptRoot '..\..\scripts\emacs-windows.ps1'

function emacs { & $script:DotfilesEmacsLauncher -EmacsArguments @($args) }
function emacsclient { & $script:DotfilesEmacsLauncher -Client -EmacsArguments @($args) }
function emacsclientw { & $script:DotfilesEmacsLauncher -Client -Gui -EmacsArguments @($args) }

function dsync {
    <#
    .SYNOPSIS
    Run doom sync with visible progress output.
    #>
    if (-not (Test-Path $script:DoomBin)) {
        Write-Error "Doom CLI not found at $script:DoomBin"
        return
    }
    Write-Host "Running doom sync..." -ForegroundColor Cyan
    & $script:DoomBin -Command sync @args
    if ($LASTEXITCODE -eq 0) {
        Write-Host "doom sync complete." -ForegroundColor Green
        Send-Toast "Doom Sync" "Sync complete"
    } else {
        Write-Host "doom sync failed (exit $LASTEXITCODE)." -ForegroundColor Red
        Send-Toast "Doom Sync" "Sync FAILED (exit $LASTEXITCODE)"
    }
}
