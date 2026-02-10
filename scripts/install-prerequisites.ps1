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

Write-Host "`nInstalling Roslyn LSP (C# language server)..." -ForegroundColor Cyan
& "$PSScriptRoot\install-roslyn-lsp.ps1"

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
    "roslyn-lsp" = "Test-Path `"$env:LOCALAPPDATA\roslyn-lsp\Microsoft.CodeAnalysis.LanguageServer.dll`""
    "yaml-language-server" = "yaml-language-server --version"
    "mmdc" = "mmdc --version"
}

foreach ($tool in $tools.Keys) {
    $found = Get-Command $tool -ErrorAction SilentlyContinue
    if (-not $found) {
        # Fall back to tool-specific check expression (e.g. roslyn-lsp is a DLL, not a command)
        $found = Invoke-Expression $tools[$tool] 2>$null
    }
    if ($found) {
        Write-Host "  OK: $tool" -ForegroundColor Green
    } else {
        Write-Host "  MISSING: $tool (not found - may need shell restart)" -ForegroundColor Red
    }
}

Write-Host "`nDone! Restart your shell if any tools were not found." -ForegroundColor Cyan
