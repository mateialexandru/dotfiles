# Build and install the compiled hack worktree manager.

$dotfilesDir = Split-Path -Parent $PSScriptRoot
$toolDir = Join-Path (Join-Path $dotfilesDir "tools") "hack"

if (-not (Get-Command cargo -ErrorAction SilentlyContinue)) {
    Write-Error "cargo is required to build hack"
    exit 1
}

cargo install --locked --force --path $toolDir
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "Installed hack." -ForegroundColor Green

# v3 was dot-sourced into the PowerShell profile. Remove that now-dead source line.
$profileCandidates = @(
    (Join-Path $env:LOCALAPPDATA 'PowerShell\7\profile.ps1'),
    $PROFILE.CurrentUserCurrentHost
) | Select-Object -Unique

foreach ($profilePath in $profileCandidates) {
    if (-not (Test-Path $profilePath)) { continue }
    $lines = @(Get-Content $profilePath)
    $clean = @($lines | Where-Object { $_ -notmatch 'shell[\\/]hack\.ps1' })
    if ($clean.Count -ne $lines.Count) {
        Set-Content -Path $profilePath -Value $clean
        Write-Host "Removed the legacy hack.ps1 source from $profilePath" -ForegroundColor Gray
    }
}
