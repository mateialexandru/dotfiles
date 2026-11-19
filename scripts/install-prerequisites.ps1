#Requires -Version 7.3
# scripts/install-prerequisites.ps1
# Installs development tool prerequisites (idempotent)

param(
    [string]$GnuplotVersion,
    [switch]$AllowGnuplotElevation,
    [switch]$AllowRuffDownloads,
    [string]$RuffVersion
)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\emacs-paths.ps1"
$env:PATH = Get-DotfilesProcessPath
if ($GnuplotVersion -and -not $AllowGnuplotElevation) {
    throw 'An explicit Gnuplot install requires -AllowGnuplotElevation; its package uses machine scope.'
}

Write-Host "Installing development prerequisites..." -ForegroundColor Cyan

# Winget packages
$wingetPackages = @(
    # Build / dev tools
    "OpenJS.NodeJS.LTS",
    "Microsoft.DotNet.SDK.8",
    "Microsoft.OpenJDK.21",
    "Kitware.CMake",
    "Rustlang.Rustup",
    "Nushell.Nushell",
    # Search / file tools
    "BurntSushi.ripgrep.MSVC",
    "sharkdp.fd",
    "junegunn.fzf",
    "ajeetdsouza.zoxide",
    "jqlang.jq",
    "UniversalCtags.Ctags",
    "LLVM.LLVM",
    # Clipboard image extraction for Doom's Org +dragndrop integration
    "ImageMagick.ImageMagick",
    # Docs / linting
    "JohnMacFarlane.Pandoc",
    "koalaman.shellcheck",
    "Graphviz.Graphviz",
    "mvdan.shfmt",
    # Fonts
    "DEVCOM.JetBrainsMonoNerdFont"
)

foreach ($pkg in $wingetPackages) {
    Write-Host "  Installing $pkg..." -ForegroundColor Yellow
    winget install --id $pkg -e --source winget --no-upgrade --accept-source-agreements --accept-package-agreements
    # WinGet reports no applicable upgrade / already installed as nonzero exits.
    if ($LASTEXITCODE -notin @(0, -1978335189, -1978335135)) {
        throw "WinGet failed or was cancelled for $pkg (exit $LASTEXITCODE)."
    }
}

& "$PSScriptRoot\install-python-runtime.ps1" -AllowDownloads

& "$PSScriptRoot\build-emacs-windows.ps1"

if ($GnuplotVersion) {
    & "$PSScriptRoot\install-gnuplot.ps1" -Version $GnuplotVersion -AllowElevation
} else {
    Write-Host 'Skipped: optional Gnuplot installation (no version/elevation approval supplied).'
}

# PlantUML (Java JAR - not on winget)
$plantumlDir = Join-Path $env:LOCALAPPDATA "plantuml"
$plantumlJar = Join-Path $plantumlDir "plantuml.jar"
if (Test-Path $plantumlJar) {
    Write-Host "  PlantUML already installed at $plantumlJar" -ForegroundColor Gray
} else {
    Write-Host "  Installing PlantUML..." -ForegroundColor Yellow
    New-Item -ItemType Directory -Path $plantumlDir -Force | Out-Null
    $plantumlUrl = "https://github.com/plantuml/plantuml/releases/latest/download/plantuml.jar"
    Invoke-WebRequest -Uri $plantumlUrl -OutFile $plantumlJar -UseBasicParsing
    # Create a wrapper script so `plantuml` works from PATH
    $wrapper = Join-Path $plantumlDir "plantuml.cmd"
    Set-Content -Path $wrapper -Value "@java -jar `"$plantumlJar`" %*"
    # Add to user PATH if not already there
    $userPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    if ($userPath -notlike "*$plantumlDir*") {
        [Environment]::SetEnvironmentVariable("PATH", "$userPath;$plantumlDir", "User")
        $env:PATH = "$env:PATH;$plantumlDir"
    }
    Write-Host "  PlantUML installed to $plantumlDir" -ForegroundColor Green
}

Write-Host "`nInstalling Inter font..." -ForegroundColor Cyan
& "$PSScriptRoot\install-inter-font.ps1"

Write-Host "`nInstalling optional Doom font presets..." -ForegroundColor Cyan
& "$PSScriptRoot\install-fonts-windows.ps1"

Write-Host "`nInstalling Roslyn LSP (C# language server)..." -ForegroundColor Cyan
& "$PSScriptRoot\install-roslyn-lsp.ps1"

# npm global packages
Write-Host "`nInstalling npm global packages..." -ForegroundColor Cyan
$npmTools = [ordered]@{
    'yaml-language-server' = 'yaml-language-server'
    'mmdc' = '@mermaid-js/mermaid-cli'
    'excalidraw-cli' = '@swiftlysingh/excalidraw-cli'
    'copilot' = '@github/copilot'
    'typescript-language-server' = 'typescript-language-server'
    'vscode-html-language-server' = 'vscode-langservers-extracted'
}
foreach ($tool in $npmTools.Keys) {
    if (Get-Command $tool -ErrorAction SilentlyContinue) {
        Write-Host "Already present: $tool (not upgrading)."
        continue
    }
    npm install -g $npmTools[$tool]
    if ($LASTEXITCODE -ne 0) {
        throw "npm installation failed or was blocked for $tool (exit $LASTEXITCODE)."
    }
}

# Shared Doom config stores editable drawings here on every platform.
$excalidrawDir = Join-Path $env:USERPROFILE "Documents\org\excalidraw"
New-Item -ItemType Directory -Path $excalidrawDir -Force | Out-Null
Write-Host "  Excalidraw: associate .excalidraw files with the Chrome PWA once installed." -ForegroundColor Gray

# PowerShell notifications used by config/shell/doom.ps1
if (-not (Get-Module -ListAvailable BurntToast)) {
    Write-Host "`nInstalling BurntToast..." -ForegroundColor Cyan
    if (Get-Command Install-PSResource -ErrorAction SilentlyContinue) {
        $moduleRepositories = @(Get-PSResourceRepository |
            Where-Object { $_.Trusted -and $_.ApiVersion -in @('V2', 'V3') } |
            Sort-Object Priority)
        if (-not $moduleRepositories.Count) {
            throw 'BurntToast is missing and no trusted NuGet PowerShell repository is configured. Configure an approved module feed before retrying.'
        }
        Install-PSResource -Name BurntToast -Repository $moduleRepositories.Name `
            -Scope CurrentUser -ErrorAction Stop
    } else {
        $moduleRepositories = @(Get-PSRepository |
            Where-Object { $_.InstallationPolicy -eq 'Trusted' })
        if (-not $moduleRepositories.Count) {
            throw 'BurntToast is missing and no trusted PowerShell repository is configured. Configure an approved module feed before retrying.'
        }
        Install-Module -Name BurntToast -Repository $moduleRepositories.Name `
            -Scope CurrentUser -ErrorAction Stop
    }
    if (-not (Get-Module -ListAvailable BurntToast)) {
        throw 'BurntToast installation returned without a discoverable module. Check PSModulePath and the approved feed before retrying.'
    }
}

# Refresh PATH from registry (picks up changes from winget/MSI installers)
# Some machine installers do not register their bin directories.
foreach ($toolPath in @('LLVM\bin\clang-format.exe', 'Graphviz\bin\dot.exe')) {
    $executable = Join-Path $env:ProgramFiles $toolPath
    if (-not (Test-Path -LiteralPath $executable)) { continue }
    $bin = Split-Path $executable
    $userPath = [Environment]::GetEnvironmentVariable('PATH', 'User')
    $registered = [Environment]::GetEnvironmentVariable('PATH', 'Machine') + ';' + $userPath
    if ($bin -notin ($registered -split ';' | ForEach-Object { $_.TrimEnd('\') })) {
        [Environment]::SetEnvironmentVariable('PATH', "$userPath;$bin", 'User')
        Write-Host "Added existing tools to user PATH: $bin"
    }
}
$machinePath = [Environment]::GetEnvironmentVariable("PATH", "Machine")
$userPath = [Environment]::GetEnvironmentVariable("PATH", "User")
$env:PATH = "$machinePath;$userPath"

# Python and Rust tools required by the enabled shared Doom modules.
& "$PSScriptRoot\install-python-tools.ps1" -AllowRuffDownloads:$AllowRuffDownloads `
    -RuffVersion $RuffVersion
rustup default stable
if ($LASTEXITCODE -ne 0) { throw 'Could not select the stable Rust toolchain.' }
rustup component add rust-analyzer
if ($LASTEXITCODE -ne 0) { throw 'Could not install rust-analyzer.' }

# Verification
Write-Host "`nVerifying installations..." -ForegroundColor Cyan
$tools = @{
    "emacs" = "emacs --version"
    "rg" = "rg --version"
    "fd" = "fd --version"
    "pandoc" = "pandoc --version"
    "shellcheck" = "shellcheck --version"
    "shfmt" = "shfmt --version"
    "dot" = "dot -V"
    "ctags" = "ctags --version"
    "dotnet" = "dotnet --version"
    "node" = "node --version"
    "npm" = "npm --version"
    "jq" = "jq --version"
    "cmake" = "cmake --version"
    "clang-format" = "clang-format --version"
    "magick" = "magick --version"
    "java" = "java --version"
    "plantuml" = "Test-Path `"$env:LOCALAPPDATA\plantuml\plantuml.jar`""
    "roslyn-lsp" = "Test-Path `"$env:LOCALAPPDATA\roslyn-lsp\Microsoft.CodeAnalysis.LanguageServer.dll`""
    "yaml-language-server" = "yaml-language-server --version"
    "mmdc" = "mmdc --version"
    "excalidraw-cli" = "excalidraw-cli --version"
    "uv" = "uv --version"
    "pyright" = "pyright --version"
    "ruff" = "ruff --version"
    "rust-analyzer" = "rust-analyzer --version"
    "nu" = "nu --version"
    "fzf" = "fzf --version"
    "zoxide" = "zoxide --version"
    "typescript-language-server" = "typescript-language-server --version"
    "gnuplot" = "gnuplot --version"
    "copilot" = "copilot --version"
}

$missingTools = @()
foreach ($tool in $tools.Keys) {
    if ($tool -eq 'emacs') {
        Invoke-DotfilesEmacs -Arguments @('--version')
        Write-Host '  OK: selected Emacs runtime' -ForegroundColor Green
        continue
    }
    if ($tool -eq 'gnuplot' -and -not $GnuplotVersion -and
        -not (Get-Command gnuplot -ErrorAction SilentlyContinue)) {
        Write-Host '  SKIPPED: gnuplot (optional)' -ForegroundColor Gray
        continue
    }
    $found = Get-Command $tool -ErrorAction SilentlyContinue
    if (-not $found) {
        # Fall back to tool-specific check expression (e.g. roslyn-lsp is a DLL, not a command)
        try { $found = Invoke-Expression $tools[$tool] 2>$null } catch { $found = $null }
    }
    if ($found) {
        Write-Host "  OK: $tool" -ForegroundColor Green
    } else {
        $missingTools += $tool
        Write-Host "  MISSING: $tool (not found - may need shell restart)" -ForegroundColor Red
    }
}

if ($missingTools.Count) {
    throw "Prerequisites remain missing: $($missingTools -join ', '). Installation is incomplete."
}
Write-Host "`nDone! Restart your shell if any tools were not found." -ForegroundColor Cyan
