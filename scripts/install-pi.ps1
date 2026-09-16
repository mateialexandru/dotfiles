$ErrorActionPreference = "Stop"

$dotfilesDir = Split-Path -Parent $PSScriptRoot
$userHome = [Environment]::GetFolderPath("UserProfile")
$piHome = if ($env:PI_CODING_AGENT_DIR) { $env:PI_CODING_AGENT_DIR } else { Join-Path $userHome ".pi\agent" }
$layersDir = Join-Path $userHome ".config\dotfiles\layers.d"
$piPackage = "@earendil-works/pi-coding-agent"
$piVersion = "0.85.1"
$piAcpPackage = "pi-acp"
$piAcpVersion = "0.0.33"

$current = try { ((& pi --version 2>$null) -replace "[v\s]", "") } catch { "" }
if ($current -eq $piVersion) {
    Write-Host "Pi $current is already installed."
} else {
    npm install --global --ignore-scripts "$piPackage@$piVersion"
    if ($LASTEXITCODE -ne 0) { throw "Pi installation failed." }
}

$piAcpCurrent = ""
try {
    $npmListing = (& npm list --global --depth=0 --json $piAcpPackage 2>$null | Out-String | ConvertFrom-Json)
    $dependency = $npmListing.dependencies.PSObject.Properties[$piAcpPackage]
    if ($dependency) { $piAcpCurrent = $dependency.Value.version }
} catch {}

if ($piAcpCurrent -eq $piAcpVersion) {
    Write-Host "Pi ACP adapter $piAcpCurrent is already installed."
} else {
    npm install --global --ignore-scripts "$piAcpPackage@$piAcpVersion"
    if ($LASTEXITCODE -ne 0) { throw "Pi ACP adapter installation failed." }
}

New-Item -ItemType Directory -Force -Path $piHome | Out-Null
$settings = [System.Collections.Generic.List[string]]::new()
$models = [System.Collections.Generic.List[string]]::new()
$contexts = [System.Collections.Generic.List[string]]::new()
$hooks = [System.Collections.Generic.List[string]]::new()
$settings.Add((Join-Path $dotfilesDir "config\pi\settings.json"))
$models.Add((Join-Path $dotfilesDir "config\pi\models.json"))
$contexts.Add((Join-Path $dotfilesDir "config\pi\AGENTS.md"))

if (Test-Path $layersDir) {
    Get-ChildItem $layersDir -Directory | Sort-Object Name | ForEach-Object {
        $settingsPath = Join-Path $_.FullName "pi\settings.json"
        $modelsPath = Join-Path $_.FullName "pi\models.json"
        $contextPath = Join-Path $_.FullName "pi\AGENTS.md"
        $hookPath = Join-Path $_.FullName "pi\install.ps1"
        if (Test-Path $settingsPath) { $settings.Add($settingsPath) }
        if (Test-Path $modelsPath) { $models.Add($modelsPath) }
        if (Test-Path $contextPath) { $contexts.Add($contextPath) }
        if (Test-Path $hookPath) { $hooks.Add($hookPath) }
    }
}

& node (Join-Path $dotfilesDir "scripts\merge-json.mjs") (Join-Path $piHome "settings.json") @settings
if ($LASTEXITCODE -ne 0) { throw "Could not compose Pi settings." }
& node (Join-Path $dotfilesDir "scripts\merge-json.mjs") (Join-Path $piHome "models.json") @models
if ($LASTEXITCODE -ne 0) { throw "Could not compose Pi models." }

$contextText = ($contexts | ForEach-Object { Get-Content $_ -Raw }) -join "`n`n"
[IO.File]::WriteAllText((Join-Path $piHome "AGENTS.md"), "$contextText`n")

foreach ($hook in $hooks) {
    Write-Host "Applying Pi layer: $hook"
    & $hook
}

Write-Host "Pi configuration composed from $($settings.Count) layer(s) in $piHome."
