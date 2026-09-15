# scripts/install-inter-font.ps1
# Installs Inter font from GitHub releases (idempotent, no admin required)

$fontsDir = Join-Path $env:LOCALAPPDATA "Microsoft\Windows\Fonts"
$regPath  = "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts"
$sentinel = Join-Path $fontsDir "Inter-Regular.ttf"

if (Test-Path $sentinel) {
    Write-Host "Inter font already installed." -ForegroundColor Green
    exit 0
}

Write-Host "Installing Inter font..." -ForegroundColor Cyan

$tempZip = Join-Path $env:TEMP "Inter.zip"
$tempDir = Join-Path $env:TEMP "InterFont"

try {
    # Resolve latest release asset via GitHub API
    $release  = Invoke-RestMethod "https://api.github.com/repos/rsms/inter/releases/latest" -UseBasicParsing
    $zipAsset = $release.assets | Where-Object { $_.name -like "Inter*.zip" } | Select-Object -First 1
    $zipUrl   = if ($zipAsset) { $zipAsset.browser_download_url } else { "https://github.com/rsms/inter/releases/latest/download/Inter.zip" }

    Write-Host "  Downloading $($release.tag_name) from $zipUrl ..."
    Invoke-WebRequest -Uri $zipUrl -OutFile $tempZip -UseBasicParsing

    Remove-Item $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    Expand-Archive -Path $tempZip -DestinationPath $tempDir -Force
    New-Item -ItemType Directory -Path $fontsDir -Force | Out-Null

    $fonts = Get-ChildItem -Path $tempDir -Filter "*.ttf" -Recurse |
        Where-Object { $_.DirectoryName -notlike "*Web*" }

    if (-not $fonts) { throw "No TTF files found in archive — zip layout may have changed" }

    foreach ($font in $fonts) {
        $dest = Join-Path $fontsDir $font.Name
        Copy-Item $font.FullName $dest -Force
        $fontName = [System.IO.Path]::GetFileNameWithoutExtension($font.Name)
        Set-ItemProperty -Path $regPath -Name "$fontName (TrueType)" -Value $dest -ErrorAction SilentlyContinue
    }

    Write-Host "  Installed $($fonts.Count) font files to $fontsDir" -ForegroundColor Green
} catch {
    Write-Host "ERROR: $_" -ForegroundColor Red
    exit 1
} finally {
    Remove-Item $tempZip -Force -ErrorAction SilentlyContinue
    Remove-Item $tempDir -Recurse -Force -ErrorAction SilentlyContinue
}
