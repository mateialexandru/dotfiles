# scripts/install-prerequisites.ps1
# Installs development tool prerequisites (idempotent)

Write-Host "Installing development prerequisites..." -ForegroundColor Cyan

# Winget packages
$wingetPackages = @(
    "UniversalCtags.Ctags",
    "OpenJS.NodeJS.LTS",
    "Microsoft.DotNet.SDK.8",
    "jqlang.jq",
    "Kitware.CMake",
    "PlantUML.PlantUML"
)

foreach ($pkg in $wingetPackages) {
    Write-Host "  Installing $pkg..." -ForegroundColor Yellow
    winget install --id $pkg -e --accept-source-agreements --accept-package-agreements
}

Write-Host "`nInstalling .NET global tools..." -ForegroundColor Cyan
dotnet tool install --global csharp-ls

Write-Host "`nInstalling npm global packages..." -ForegroundColor Cyan
npm install -g yaml-language-server
npm install -g @mermaid-js/mermaid-cli

# Verification
Write-Host "`nVerifying installations..." -ForegroundColor Cyan
$tools = @{
    "ctags" = "ctags --version"
    "dotnet" = "dotnet --version"
    "node" = "node --version"
    "npm" = "npm --version"
    "jq" = "jq --version"
    "cmake" = "cmake --version"
    "csharp-ls" = "csharp-ls --version"
    "yaml-language-server" = "yaml-language-server --version"
    "mmdc" = "mmdc --version"
}

foreach ($tool in $tools.Keys) {
    if (Get-Command $tool -ErrorAction SilentlyContinue) {
        Write-Host "  ✓ $tool" -ForegroundColor Green
    } else {
        Write-Host "  ✗ $tool (not found in PATH - may need shell restart)" -ForegroundColor Red
    }
}

Write-Host "`nDone! Restart your shell if any tools were not found." -ForegroundColor Cyan
