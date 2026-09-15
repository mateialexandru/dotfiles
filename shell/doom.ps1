# doom.ps1 — Doom Emacs helpers (dot-source in your PowerShell profile)
#   . "<dotfiles>/shell/doom.ps1"

. "$PSScriptRoot/notify.ps1"

$script:DoomBin = Join-Path $env:HOME '.config\emacs\bin\doom.ps1'

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
    & powershell -ExecutionPolicy Bypass -File $script:DoomBin sync @args
    if ($LASTEXITCODE -eq 0) {
        Write-Host "doom sync complete." -ForegroundColor Green
        Send-Toast "Doom Sync" "Sync complete"
    } else {
        Write-Host "doom sync failed (exit $LASTEXITCODE)." -ForegroundColor Red
        Send-Toast "Doom Sync" "Sync FAILED (exit $LASTEXITCODE)"
    }
}
