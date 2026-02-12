# install-hack.ps1 — Adds hack.ps1 dot-source to PowerShell profile

$dotfilesDir = Split-Path -Parent $PSScriptRoot
$hackScript = Join-Path $dotfilesDir "shell" "hack.ps1"

if (-not (Test-Path $hackScript)) {
    Write-Host "Error: hack.ps1 not found at $hackScript" -ForegroundColor Red
    exit 1
}

# Determine profile path
$profilePath = $PROFILE.CurrentUserCurrentHost

if (-not (Test-Path $profilePath)) {
    New-Item -ItemType File -Path $profilePath -Force | Out-Null
    Write-Host "Created profile at $profilePath" -ForegroundColor Gray
}

$dotSourceLine = ". `"$hackScript`""
$profileContent = Get-Content $profilePath -Raw -ErrorAction SilentlyContinue

if ($profileContent -and $profileContent.Contains($hackScript)) {
    Write-Host "hack.ps1 already sourced in profile" -ForegroundColor Gray
    return
}

# Append dot-source
Add-Content -Path $profilePath -Value "`n# Hack worktree tooling`n$dotSourceLine"
Write-Host "Added hack.ps1 to PowerShell profile: $profilePath" -ForegroundColor Green
Write-Host "Restart your shell or run: $dotSourceLine" -ForegroundColor Gray
