# Install the optional Doom font presets for the current Windows user.
# JetBrains Mono and Inter are installed by install-prerequisites.ps1 already.
$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.Drawing
Add-Type @'
using System;
using System.Runtime.InteropServices;
public static class DotfilesFonts {
    [DllImport("gdi32.dll", CharSet = CharSet.Unicode)]
    public static extern int AddFontResource(string path);
    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern IntPtr SendMessageTimeout(IntPtr window, uint message,
        IntPtr wParam, IntPtr lParam, uint flags, uint timeout, out IntPtr result);
}
'@

$fontDir = Join-Path $env:LOCALAPPDATA 'Microsoft\Windows\Fonts'
$fontRegistry = 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts'
New-Item -Path $fontRegistry -Force | Out-Null
$installedFamilies = @((New-Object System.Drawing.Text.InstalledFontCollection).Families |
    ForEach-Object { $_.Name })
$nerdFontsVersion = 'v3.4.0'
$mapleVersion = 'v7.9'

function Get-IosevkaAssetUrl {
    param([Parameter(Mandatory)][string]$Variant)
    $release = Invoke-RestMethod 'https://api.github.com/repos/be5invis/Iosevka/releases/latest' -UseBasicParsing
    $asset = $release.assets | Where-Object {
        $_.name -match "^PkgTTF-Iosevka$Variant-[0-9].*\.zip$"
    } | Select-Object -First 1
    if (-not $asset) { throw "No Iosevka $Variant font archive found in the latest release." }
    return $asset.browser_download_url
}

function Install-FontArchive {
    param(
        [Parameter(Mandatory)][string]$Name,
        [Parameter(Mandatory)][string[]]$Families,
        [Parameter(Mandatory)][string]$Url,
        [string]$FileNamePattern = '*'
    )

    if (@($Families | Where-Object { $_ -notin $installedFamilies }).Count -eq 0) {
        Write-Host "  $Name already installed; skipping." -ForegroundColor Gray
        return
    }

    Write-Host "  Downloading $Name from $Url ..." -ForegroundColor Yellow
    $work = Join-Path $env:TEMP ("dotfiles-font-" + [guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $work -Force | Out-Null
    try {
        $archive = Join-Path $work 'font.zip'
        $extracted = Join-Path $work 'extracted'
        Invoke-WebRequest -Uri $Url -OutFile $archive -UseBasicParsing
        Expand-Archive -LiteralPath $archive -DestinationPath $extracted -Force
        $fonts = @(Get-ChildItem -LiteralPath $extracted -File -Recurse |
            Where-Object { $_.Extension -in @('.ttf', '.otf') -and $_.Name -like $FileNamePattern })
        if ($fonts.Count -eq 0) { throw "No usable font files found in $Name archive." }

        New-Item -ItemType Directory -Path $fontDir -Force | Out-Null
        foreach ($font in $fonts) {
            # Prefix names so archives with similarly named faces do not overwrite each other.
            $relative = $font.FullName.Substring($extracted.Length + 1)
            $safeName = ($relative -replace '[\\/:*?"<>|]', '-')
            $destination = Join-Path $fontDir "$Name-$safeName"
            Copy-Item -LiteralPath $font.FullName -Destination $destination -Force
            $kind = if ($font.Extension -eq '.otf') { 'OpenType' } else { 'TrueType' }
            Set-ItemProperty -Path $fontRegistry -Name "$Name-$safeName ($kind)" -Value $destination
            [void][DotfilesFonts]::AddFontResource($destination)
        }
        Write-Host "  Installed $($fonts.Count) $Name font files for this user." -ForegroundColor Green
    } finally {
        Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
    }
}

$nerdBase = "https://github.com/ryanoasis/nerd-fonts/releases/download/$nerdFontsVersion"
Install-FontArchive -Name 'Iosevka-NF' -Families @('Iosevka Nerd Font') -Url "$nerdBase/Iosevka.zip"
Install-FontArchive -Name 'FiraCode-NF' -Families @('FiraCode Nerd Font') -Url "$nerdBase/FiraCode.zip"
Install-FontArchive -Name 'CascadiaCode-NF' -Families @('CaskaydiaCove Nerd Font') -Url "$nerdBase/CascadiaCode.zip"
Install-FontArchive -Name 'VictorMono-NF' -Families @('VictorMono Nerd Font') -Url "$nerdBase/VictorMono.zip"
Install-FontArchive -Name 'Monaspace-NF' -Families @('MonaspiceNe Nerd Font', 'MonaspiceAr Nerd Font') -Url "$nerdBase/Monaspace.zip"
Install-FontArchive -Name 'MapleMono-NF' -Families @('Maple Mono NF') -Url "https://github.com/subframe7536/maple-font/releases/download/$mapleVersion/MapleMono-NF.zip"

$plexBase = 'https://github.com/IBM/plex/releases/download'
Install-FontArchive -Name 'IBMPlexMono' -Families @('IBM Plex Mono') -Url "$plexBase/%40ibm/plex-mono%401.1.0/ibm-plex-mono.zip"
Install-FontArchive -Name 'IBMPlexSans' -Families @('IBM Plex Sans') -Url "$plexBase/%40ibm/plex-sans%401.1.0/ibm-plex-sans.zip"

foreach ($variant in @('Aile', 'Etoile')) {
    $family = "Iosevka $variant"
    if ($family -notin $installedFamilies) {
        Install-FontArchive -Name "Iosevka$variant" -Families @($family) -Url (Get-IosevkaAssetUrl -Variant $variant)
    }
}

Install-FontArchive -Name 'FiraSans' -Families @('Fira Sans') `
    -Url 'https://github.com/mozilla/Fira/archive/refs/tags/4.202.zip' -FileNamePattern 'FiraSans*.otf'

# Tell running apps about newly registered fonts; Emacs may still need a restart.
$result = [IntPtr]::Zero
[void][DotfilesFonts]::SendMessageTimeout([IntPtr]0xffff, 0x001D,
    [IntPtr]::Zero, [IntPtr]::Zero, 2, 1000, [ref]$result)
