# Offline font-archive tests: no downloads or real font registration.
$ErrorActionPreference = 'Stop'
Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem
Add-Type 'public static class DotfilesFonts { public static int AddFontResource(string path) { return 1; } }'

$tokens = $null
$errors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile(
    (Join-Path $PSScriptRoot 'install-fonts-windows.ps1'), [ref]$tokens, [ref]$errors)
if ($errors.Count) { throw "Font installer parse errors: $errors" }
$definition = $ast.Find({
    param($node)
    $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and
    $node.Name -eq 'Install-FontArchive'
}, $true)
. ([scriptblock]::Create($definition.Extent.Text))

$temporary = Join-Path ([IO.Path]::GetTempPath()) ("dotfiles-font-test-" + [guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $temporary | Out-Null
$previousTemp = $env:TEMP
try {
    $env:TEMP = $temporary
    $fixture = Join-Path $temporary 'fixture.zip'
    $zip = [IO.Compression.ZipFile]::Open($fixture, [IO.Compression.ZipArchiveMode]::Create)
    try {
        foreach ($name in @('source/otf/FiraSans-Regular.otf', 'source/Other.ttf', 'source/glyphs/unused.glif')) {
            $writer = [IO.StreamWriter]::new($zip.CreateEntry($name).Open())
            try { $writer.Write('fixture font content') } finally { $writer.Dispose() }
        }
    } finally { $zip.Dispose() }

    $fontDir = Join-Path $temporary 'fonts'
    $fontRegistry = 'mock-font-registry'
    $installedFamilies = @()
    $script:registrations = @()
    $script:downloads = 0
    function Invoke-WebRequest($Uri, $OutFile, [switch]$UseBasicParsing) {
        $script:downloads++
        Copy-Item -LiteralPath $fixture -Destination $OutFile
    }
    function Expand-Archive { throw 'Whole source archives must not be extracted.' }
    function Set-ItemProperty($Path, $Name, $Value) {
        $script:registrations += @{ Name = $Name; Value = $Value }
    }
    Install-FontArchive -Name 'FiraSans' -Families @('Fira Sans') `
        -Url 'https://example.invalid/font.zip' -FileNamePattern 'FiraSans*.otf'
    $fonts = @(Get-ChildItem -LiteralPath $fontDir -File)
    if ($fonts.Count -ne 1 -or $fonts[0].Name -ne 'FiraSans-source-otf-FiraSans-Regular.otf') {
        throw 'Font selection or archive-relative naming changed.'
    }
    if ($script:registrations.Count -ne 1 -or
        $script:registrations[0].Name -ne 'FiraSans-source-otf-FiraSans-Regular.otf (OpenType)') {
        throw 'Selected font was not registered correctly.'
    }
    Write-Host 'PASS: extracts only matching fonts and preserves registration names'

    $loadedFont = [IO.File]::OpenRead($fonts[0].FullName)
    try {
        Install-FontArchive -Name 'FiraSans' -Families @('Fira Sans') `
            -Url 'https://example.invalid/font.zip' -FileNamePattern 'FiraSans*.otf'
    } finally { $loadedFont.Dispose() }
    Write-Host 'PASS: identical loaded fonts are reused without overwriting'

    Set-Content -LiteralPath $fonts[0].FullName -Value 'different font content'
    $failed = $false
    try {
        Install-FontArchive -Name 'FiraSans' -Families @('Fira Sans') `
            -Url 'https://example.invalid/font.zip' -FileNamePattern 'FiraSans*.otf'
    } catch {
        if ($_.ToString() -notmatch 'Existing font differs') { throw }
        $failed = $true
    }
    if (-not $failed) { throw 'Existing fonts with different bytes must not be overwritten.' }
    Write-Host 'PASS: differing existing fonts require explicit remediation'

    $failed = $false
    try {
        Install-FontArchive -Name 'Missing' -Families @('Missing') `
            -Url 'https://example.invalid/font.zip' -FileNamePattern 'missing*.otf'
    } catch {
        if ($_.ToString() -notmatch 'No usable font files') { throw }
        $failed = $true
    }
    if (-not $failed) { throw 'An archive without matching fonts must fail.' }
    if (Get-ChildItem -LiteralPath $temporary -Directory -Filter 'dotfiles-font-*') {
        throw 'Temporary font archives were not cleaned up.'
    }
    Write-Host 'PASS: missing font selections fail and clean up their archives'

    $installedFamilies = @('Iosevka NF', 'MonaspiceNe NF')
    $previousDownloads = $script:downloads
    Install-FontArchive -Name 'Aliases' -Families @('Iosevka Nerd Font', 'MonaspiceNe Nerd Font') `
        -Url 'https://example.invalid/font.zip'
    if ($script:downloads -ne $previousDownloads) {
        throw 'Installed Windows NF aliases must not trigger another download.'
    }
    Write-Host 'PASS: Windows short Nerd Font names are recognized'
} finally {
    $env:TEMP = $previousTemp
    Remove-Item -LiteralPath $temporary -Recurse -Force
}
