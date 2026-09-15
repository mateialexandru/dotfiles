# scripts/install-roslyn-lsp.ps1
# Downloads and installs Microsoft.CodeAnalysis.LanguageServer (Roslyn LSP)
# Same C# language server that powers VS Code

param(
    [string]$Version = "5.0.0-1.25277.114"
)

$installDir = Join-Path $env:LOCALAPPDATA "roslyn-lsp"
$dllName = "Microsoft.CodeAnalysis.LanguageServer.dll"
$targetDll = Join-Path $installDir $dllName

# Skip if already installed
if (Test-Path $targetDll) {
    Write-Host "Roslyn LSP already installed at $targetDll" -ForegroundColor Green
    Write-Host "To reinstall, delete $installDir and run again."
    return
}

$nugetUrl = "https://www.nuget.org/api/v2/package/Microsoft.CodeAnalysis.LanguageServer.win-x64/$Version"
$tempZip = Join-Path $env:TEMP "roslyn-lsp-$Version.zip"
$tempExtract = Join-Path $env:TEMP "roslyn-lsp-extract"

Write-Host "Installing Microsoft.CodeAnalysis.LanguageServer v$Version..." -ForegroundColor Cyan

# Download NuGet package (it's a ZIP file)
Write-Host "  Downloading from NuGet..." -ForegroundColor Yellow
try {
    Invoke-WebRequest -Uri $nugetUrl -OutFile $tempZip -UseBasicParsing
} catch {
    Write-Host "  Failed to download: $_" -ForegroundColor Red
    Write-Host "  URL: $nugetUrl" -ForegroundColor DarkGray
    exit 1
}

# Extract
Write-Host "  Extracting..." -ForegroundColor Yellow
if (Test-Path $tempExtract) { Remove-Item $tempExtract -Recurse -Force }
Expand-Archive -Path $tempZip -DestinationPath $tempExtract -Force

# Find the DLL - path varies between versions
$foundDll = Get-ChildItem -Path $tempExtract -Recurse -Filter $dllName | Select-Object -First 1
if (-not $foundDll) {
    Write-Host "  ERROR: $dllName not found in package" -ForegroundColor Red
    Write-Host "  Package contents:" -ForegroundColor DarkGray
    Get-ChildItem -Path $tempExtract -Recurse -File | ForEach-Object {
        Write-Host "    $($_.FullName.Replace($tempExtract, ''))" -ForegroundColor DarkGray
    }
    Remove-Item $tempZip -Force -ErrorAction SilentlyContinue
    Remove-Item $tempExtract -Recurse -Force -ErrorAction SilentlyContinue
    exit 1
}

# Copy the server directory (DLL + all dependencies) to install location
$serverDir = $foundDll.DirectoryName
Write-Host "  Found server at: $($serverDir.Replace($tempExtract, '...'))" -ForegroundColor DarkGray

if (Test-Path $installDir) { Remove-Item $installDir -Recurse -Force }
New-Item -ItemType Directory -Force -Path $installDir | Out-Null
Copy-Item -Path "$serverDir\*" -Destination $installDir -Recurse -Force

# Cleanup
Remove-Item $tempZip -Force -ErrorAction SilentlyContinue
Remove-Item $tempExtract -Recurse -Force -ErrorAction SilentlyContinue

# Verify
if (Test-Path $targetDll) {
    Write-Host "  Installed to $installDir" -ForegroundColor Green
    Write-Host "  Launch: dotnet $targetDll --logLevel Information" -ForegroundColor DarkGray
} else {
    Write-Host "  ERROR: Installation failed - DLL not at expected path" -ForegroundColor Red
    exit 1
}
