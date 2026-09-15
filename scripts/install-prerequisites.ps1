# scripts/install-prerequisites.ps1
# Installs development tool prerequisites (idempotent)

Write-Host "Installing development prerequisites..." -ForegroundColor Cyan

# Winget packages
$wingetPackages = @(
    # Editors
    "GNU.Emacs",
    # Build / dev tools
    "OpenJS.NodeJS.LTS",
    "Microsoft.DotNet.SDK.8",
    "Microsoft.OpenJDK.21",
    "Kitware.CMake",
    "Python.Python.3.13",
    "astral-sh.uv",
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
    # Docs / linting
    "JohnMacFarlane.Pandoc",
    "koalaman.shellcheck",
    # Plotting (org-babel gnuplot)
    "gnuplot.gnuplot",
    # Fonts
    "DEVCOM.JetBrainsMonoNerdFont"
)

foreach ($pkg in $wingetPackages) {
    Write-Host "  Installing $pkg..." -ForegroundColor Yellow
    winget install --id $pkg -e --accept-source-agreements --accept-package-agreements
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

Write-Host "`nInstalling Roslyn LSP (C# language server)..." -ForegroundColor Cyan
& "$PSScriptRoot\install-roslyn-lsp.ps1"

# npm global packages
Write-Host "`nInstalling npm global packages..." -ForegroundColor Cyan
npm install -g yaml-language-server
npm install -g @mermaid-js/mermaid-cli
npm install -g @github/copilot
npm install -g bash-language-server typescript-language-server vscode-langservers-extracted

# PowerShell notifications used by config/shell/doom.ps1
if (-not (Get-Module -ListAvailable BurntToast)) {
    Write-Host "`nInstalling BurntToast..." -ForegroundColor Cyan
    if (Get-Command Install-PSResource -ErrorAction SilentlyContinue) {
        Install-PSResource BurntToast -Scope CurrentUser -TrustRepository
    } else {
        Install-Module BurntToast -Scope CurrentUser -Force
    }
}

# Refresh PATH from registry (picks up changes from winget/MSI installers)
$machinePath = [Environment]::GetEnvironmentVariable("PATH", "Machine")
$userPath = [Environment]::GetEnvironmentVariable("PATH", "User")
$env:PATH = "$machinePath;$userPath"

# Python and Rust tools required by the enabled shared Doom modules.
uv tool install --force pyright
uv tool install --force ruff
rustup default stable
rustup component add rust-analyzer

# Verification
Write-Host "`nVerifying installations..." -ForegroundColor Cyan
$tools = @{
    "emacs" = "emacs --version"
    "rg" = "rg --version"
    "fd" = "fd --version"
    "pandoc" = "pandoc --version"
    "shellcheck" = "shellcheck --version"
    "ctags" = "ctags --version"
    "dotnet" = "dotnet --version"
    "node" = "node --version"
    "npm" = "npm --version"
    "jq" = "jq --version"
    "cmake" = "cmake --version"
    "clang-format" = "clang-format --version"
    "java" = "java --version"
    "plantuml" = "Test-Path `"$env:LOCALAPPDATA\plantuml\plantuml.jar`""
    "roslyn-lsp" = "Test-Path `"$env:LOCALAPPDATA\roslyn-lsp\Microsoft.CodeAnalysis.LanguageServer.dll`""
    "yaml-language-server" = "yaml-language-server --version"
    "mmdc" = "mmdc --version"
    "uv" = "uv --version"
    "pyright" = "pyright --version"
    "ruff" = "ruff --version"
    "rust-analyzer" = "rust-analyzer --version"
    "nu" = "nu --version"
    "fzf" = "fzf --version"
    "zoxide" = "zoxide --version"
    "bash-language-server" = "bash-language-server --version"
    "typescript-language-server" = "typescript-language-server --version"
    "gnuplot" = "gnuplot --version"
    "copilot" = "copilot --version"
}

foreach ($tool in $tools.Keys) {
    $found = Get-Command $tool -ErrorAction SilentlyContinue
    if (-not $found) {
        # Fall back to tool-specific check expression (e.g. roslyn-lsp is a DLL, not a command)
        try { $found = Invoke-Expression $tools[$tool] 2>$null } catch { $found = $null }
    }
    if ($found) {
        Write-Host "  OK: $tool" -ForegroundColor Green
    } else {
        Write-Host "  MISSING: $tool (not found - may need shell restart)" -ForegroundColor Red
    }
}

Write-Host "`nDone! Restart your shell if any tools were not found." -ForegroundColor Cyan
