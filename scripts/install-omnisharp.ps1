# scripts/install-omnisharp.ps1
# Downloads and installs OmniSharp-Roslyn LSP server for Windows
# Installed to %LOCALAPPDATA%\omnisharp\OmniSharp.exe

param(
    [string]$Version = "1.39.13"
)

$installDir = Join-Path $env:LOCALAPPDATA "omnisharp"
$binary     = Join-Path $installDir "OmniSharp.exe"

# Skip if already installed
if (Test-Path $binary) {
    Write-Host "OmniSharp already installed at $binary" -ForegroundColor Green
    Write-Host "To reinstall, delete $installDir and run again."
    return
}

$url      = "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v$Version/omnisharp-win-x64-net6.0.zip"
$tempZip  = Join-Path $env:TEMP "omnisharp-$Version.zip"
$tempDir  = Join-Path $env:TEMP "omnisharp-extract"

Write-Host "Installing OmniSharp v$Version..." -ForegroundColor Cyan

# Download
Write-Host "  Downloading from GitHub..." -ForegroundColor Yellow
try {
    Invoke-WebRequest -Uri $url -OutFile $tempZip -UseBasicParsing
} catch {
    Write-Host "  Failed to download: $_" -ForegroundColor Red
    Write-Host "  URL: $url" -ForegroundColor DarkGray
    exit 1
}

# Extract
Write-Host "  Extracting..." -ForegroundColor Yellow
if (Test-Path $tempDir) { Remove-Item $tempDir -Recurse -Force }
Expand-Archive -Path $tempZip -DestinationPath $tempDir -Force

# Find OmniSharp.exe — location can vary between releases
$found = Get-ChildItem -Path $tempDir -Recurse -Filter "OmniSharp.exe" | Select-Object -First 1
if (-not $found) {
    Write-Host "  ERROR: OmniSharp.exe not found in archive" -ForegroundColor Red
    Write-Host "  Archive contents:" -ForegroundColor DarkGray
    Get-ChildItem -Path $tempDir -Recurse -File | ForEach-Object {
        Write-Host "    $($_.FullName.Replace($tempDir, ''))" -ForegroundColor DarkGray
    }
    Remove-Item $tempZip -Force -ErrorAction SilentlyContinue
    Remove-Item $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    exit 1
}

# Copy the whole server directory (exe + all dependencies)
$serverDir = $found.DirectoryName
Write-Host "  Found server at: $($serverDir.Replace($tempDir, '...'))" -ForegroundColor DarkGray

if (Test-Path $installDir) { Remove-Item $installDir -Recurse -Force }
New-Item -ItemType Directory -Force -Path $installDir | Out-Null
Copy-Item -Path "$serverDir\*" -Destination $installDir -Recurse -Force

# Cleanup
Remove-Item $tempZip  -Force -ErrorAction SilentlyContinue
Remove-Item $tempDir  -Recurse -Force -ErrorAction SilentlyContinue

# Verify
if (Test-Path $binary) {
    Write-Host "  Installed to $installDir" -ForegroundColor Green
    Write-Host "  Launch:  $binary -lsp" -ForegroundColor DarkGray
    Write-Host "  In Doom: SPC t L to toggle C# backend to OmniSharp" -ForegroundColor DarkGray
} else {
    Write-Host "  ERROR: Installation failed - binary not at expected path" -ForegroundColor Red
    exit 1
}
