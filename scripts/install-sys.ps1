# Build and install the compiled cross-platform dotfiles operations CLI.

$dotfilesDir = Split-Path -Parent $PSScriptRoot
$toolDir = Join-Path (Join-Path $dotfilesDir "tools") "sys"

if (-not (Get-Command cargo -ErrorAction SilentlyContinue)) {
    Write-Error "cargo is required to build sys"
    exit 1
}

cargo install --locked --force --path $toolDir
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "Installed sys." -ForegroundColor Green
