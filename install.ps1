# Dotfiles Install Script (Windows)
# Run after cloning: .\install.ps1

$dotfilesDir = $PSScriptRoot
$scriptsDir = Join-Path $dotfilesDir "scripts"

Write-Host "Installing dotfiles from: $dotfilesDir" -ForegroundColor Cyan

# 1. PowerShell profile isolation (move out of OneDrive)
Write-Host "`n--- PowerShell profile setup ---" -ForegroundColor Cyan
& (Join-Path $scriptsDir "setup-profile.ps1")

# 2. Development prerequisites (ctags, node, dotnet, cmake, roslyn, etc.)
Write-Host "`n--- Development prerequisites ---" -ForegroundColor Cyan
& (Join-Path $scriptsDir "install-prerequisites.ps1")

# 3. Nushell configuration (parallel; does not change the default shell)
Write-Host "`n--- Nushell configuration ---" -ForegroundColor Cyan
& (Join-Path $scriptsDir "install-nushell.ps1")

# 4. Doom Emacs (symlinks + deps + doom install)
Write-Host "`n--- Pi coding agent ---" -ForegroundColor Cyan
& (Join-Path $scriptsDir "install-pi.ps1")

# 5. Doom Emacs (symlinks + deps + doom install)
Write-Host "`n--- Doom Emacs ---" -ForegroundColor Cyan
& (Join-Path $scriptsDir "install-doom.ps1")

# 6. Compiled Hack worktree tooling
Write-Host "`n--- Hack worktree tooling ---" -ForegroundColor Cyan
& (Join-Path $scriptsDir "install-hack.ps1")

# 7. Cross-platform system operations CLI
Write-Host "`n--- System operations CLI ---" -ForegroundColor Cyan
if ($env:SYS_SKIP_SELF_INSTALL -eq "1") {
    Write-Host "sys is running this install; keeping the current executable." -ForegroundColor Gray
} else {
    & (Join-Path $scriptsDir "install-sys.ps1")
}

# 8. Git performance settings (Windows-specific)
Write-Host "`n--- Git performance ---" -ForegroundColor Cyan
git config --global core.preloadindex true
git config --global core.fscache true
git config --global core.untrackedCache true
git config --global feature.manyFiles true
Write-Host "Git performance settings applied." -ForegroundColor Green

Write-Host "`nDotfiles installation complete!" -ForegroundColor Green
Write-Host "Run 'sys check' to verify everything is working." -ForegroundColor Gray
