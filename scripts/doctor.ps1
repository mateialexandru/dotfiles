# scripts/doctor.ps1
# Validates Doom Emacs prerequisites are installed and configured correctly

param(
    [switch]$Quiet,  # Only show failures
    [switch]$Fix     # Install missing tools
)

. "$PSScriptRoot\emacs-paths.ps1"

# Tool definitions - easily expandable
$coreTools = @(
    @{ Name = "emacs"; Fix = "winget install GNU.Emacs" }
    @{ Name = "rg"; Fix = "winget install BurntSushi.ripgrep.MSVC" }
    @{ Name = "fd"; Fix = "winget install sharkdp.fd" }
    @{ Name = "clangd"; Fix = "winget install LLVM.clangd" }
    @{ Name = "clang-format"; Fix = "winget install LLVM.LLVM" }
    @{ Name = "pandoc"; Fix = "winget install JohnMacFarlane.Pandoc" }
    @{ Name = "shellcheck"; Fix = "winget install koalaman.shellcheck" }
    @{ Name = "git"; Fix = "winget install Git.Git" }
    @{ Name = "nu"; Fix = "winget install Nushell.Nushell" }
)

$devTools = @(
    @{ Name = "ctags"; Fix = "winget install UniversalCtags.Ctags" }
    @{ Name = "node"; Fix = "winget install OpenJS.NodeJS.LTS" }
    @{ Name = "npm"; Fix = "winget install OpenJS.NodeJS.LTS" }
    @{ Name = "dotnet"; Fix = "winget install Microsoft.DotNet.SDK.8" }
    @{ Name = "jq"; Fix = "winget install jqlang.jq" }
    @{ Name = "magick"; Fix = "winget install ImageMagick.ImageMagick" }
    @{ Name = "cmake"; Fix = "winget install Kitware.CMake" }
    @{ Name = "java"; Fix = "winget install Microsoft.OpenJDK.21" }
    @{ Name = "plantuml"; Fix = "New-Item -ItemType Directory -Force -Path `"$env:LOCALAPPDATA\plantuml`" | Out-Null; Invoke-WebRequest -Uri 'https://github.com/plantuml/plantuml/releases/latest/download/plantuml.jar' -OutFile `"$env:LOCALAPPDATA\plantuml\plantuml.jar`"" }
    @{ Name = "roslyn-lsp"; Fix = "& `"$PSScriptRoot\install-roslyn-lsp.ps1`"" }
    @{ Name = "yaml-language-server"; Fix = "npm install -g yaml-language-server" }
    @{ Name = "mmdc"; Fix = "npm install -g @mermaid-js/mermaid-cli" }
    @{ Name = "excalidraw-cli"; Fix = "npm install -g @swiftlysingh/excalidraw-cli" }
    @{ Name = "csharpier"; Fix = "dotnet tool install --global csharpier" }
    @{ Name = "excalidraw-dir"; Fix = "New-Item -ItemType Directory -Force -Path (Join-Path `$env:USERPROFILE 'Documents\org\excalidraw') | Out-Null" }
    @{ Name = "uv"; Fix = "winget install astral-sh.uv" }
    @{ Name = "pyright"; Manual = $true; Fix = "& `"$PSScriptRoot\install-python-tools.ps1`" -AllowUvDownloads -PyrightVersion <exact-version> -RuffVersion <exact-version>" }
    @{ Name = "ruff"; Manual = $true; Fix = "& `"$PSScriptRoot\install-python-tools.ps1`" -AllowUvDownloads -PyrightVersion <exact-version> -RuffVersion <exact-version>" }
    @{ Name = "rust-analyzer"; Fix = "rustup component add rust-analyzer" }
    @{ Name = "typescript-language-server"; Fix = "npm install -g typescript-language-server" }
    @{ Name = "pi"; Fix = "& `"$PSScriptRoot\install-pi.ps1`"" }
    @{ Name = "pi-acp"; Fix = "& `"$PSScriptRoot\install-pi.ps1`"" }
    @{ Name = "hack"; Fix = "& `"$PSScriptRoot\install-hack.ps1`"" }
    @{ Name = "sys"; Fix = "& `"$PSScriptRoot\install-sys.ps1`"" }
    @{ Name = "fzf"; Fix = "winget install junegunn.fzf" }
    @{ Name = "zoxide"; Fix = "winget install ajeetdsouza.zoxide" }
)

# Track results
$script:passed = 0
$script:failed = 0
$script:advisories = 0
$script:missingTools = @()

# Find executable (may not be in PATH immediately after install)
function Find-Tool($name, $locations) {
    $cmd = Get-Command $name -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }

    foreach ($pattern in $locations) {
        $found = Get-ChildItem $pattern -ErrorAction SilentlyContinue | Sort-Object -Descending | Select-Object -First 1
        if ($found) { return $found.FullName }
    }
    return $null
}

function Find-Java {
    Find-Tool "java" @(
        "$env:ProgramFiles\Microsoft\jdk-*\bin\java.exe"
        "$env:ProgramFiles\Eclipse Adoptium\jdk-*\bin\java.exe"
        "$env:ProgramFiles\Java\jdk-*\bin\java.exe"
    )
}

function Find-Cmake {
    Find-Tool "cmake" @(
        "$env:ProgramFiles\CMake\bin\cmake.exe"
    )
}

function Find-UniversalCtags {
    # Check if ctags is Universal Ctags (not Emacs etags)
    $ctags = Get-Command ctags -ErrorAction SilentlyContinue
    if ($ctags) {
        $ver = & $ctags.Source --version 2>$null | Select-Object -First 1
        if ($ver -match "Universal Ctags") { return $ctags.Source }
    }
    # Check winget links location
    $wingetCtags = "$env:LOCALAPPDATA\Microsoft\WinGet\Links\ctags.exe"
    if (Test-Path $wingetCtags) {
        $ver = & $wingetCtags --version 2>$null | Select-Object -First 1
        if ($ver -match "Universal Ctags") { return $wingetCtags }
    }
    return $null
}


function Find-RoslynLsp {
    $dll = Join-Path $env:LOCALAPPDATA "roslyn-lsp\Microsoft.CodeAnalysis.LanguageServer.dll"
    if (Test-Path $dll) { return $dll }
    return $null
}

function Get-ToolVersion($name) {
    try {
        $output = switch ($name) {
            "emacs" { & emacs --version 2>$null | Select-Object -First 1 }
            "node" { & node --version 2>$null }
            "npm" { & npm --version 2>$null }
            "dotnet" { & dotnet --version 2>$null }
            "git" { & git --version 2>$null }
            "rg" { & rg --version 2>$null | Select-Object -First 1 }
            "fd" { & fd --version 2>$null }
            "clangd" { & clangd --version 2>$null | Select-Object -First 1 }
            "clang-format" { & clang-format --version 2>$null | Select-Object -First 1 }
            "pandoc" { & pandoc --version 2>$null | Select-Object -First 1 }
            "shellcheck" { & shellcheck --version 2>$null | Where-Object { $_ -match "version:" } }
            "ctags" {
                # Must be Universal Ctags, not Emacs etags
                $ver = & ctags --version 2>$null | Select-Object -First 1
                if ($ver -match "Universal Ctags") { $ver }
            }
            "jq" { & jq --version 2>$null }
            "magick" { & magick --version 2>$null | Select-Object -First 1 }
            "cmake" {
                $cmakeExe = Find-Cmake
                if ($cmakeExe) { & $cmakeExe --version 2>$null | Select-Object -First 1 }
            }
            "java" {
                $javaExe = Find-Java
                if ($javaExe) { & $javaExe --version 2>$null | Select-Object -First 1 }
            }
            "plantuml" {
                $jar = "$env:LOCALAPPDATA\plantuml\plantuml.jar"
                $javaExe = Find-Java
                if ((Test-Path $jar) -and $javaExe) { & $javaExe -jar $jar -version 2>$null | Select-Object -First 1 }
            }
            "roslyn-lsp" {
                $dll = Find-RoslynLsp
                if ($dll) { "installed" }
            }
            "yaml-language-server" { & yaml-language-server --version 2>$null }
            "mmdc" { & mmdc --version 2>$null }
            "excalidraw-cli" { & excalidraw-cli --version 2>$null }
            "csharpier" { & csharpier --version 2>$null }
            "uv" { & uv --version 2>$null }
            "pyright" { & pyright --version 2>$null }
            "ruff" { & ruff --version 2>$null }
            "rust-analyzer" { & rust-analyzer --version 2>$null }
            "typescript-language-server" { & typescript-language-server --version 2>$null }
            "pi" { & pi --version 2>$null }
            "pi-acp" { "installed" }
            "sys" { & sys --version 2>$null }
            "nu" { & nu --version 2>$null }
            "fzf" { & fzf --version 2>$null }
            "zoxide" { & zoxide --version 2>$null }
            default { $null }
        }
        if ($output) {
            # Extract version number pattern
            if ($output -match '(\d+\.\d+(\.\d+)?)') {
                return $Matches[1]
            }
            return $output.Trim()
        }
    } catch {
        return $null
    }
    return $null
}

function Test-Tool($tool) {
    # Special cases for tools not in PATH
    if ($tool.Name -eq "plantuml") {
        $jar = "$env:LOCALAPPDATA\plantuml\plantuml.jar"
        $exists = Test-Path $jar
    } elseif ($tool.Name -eq "java") {
        $exists = Find-Java
    } elseif ($tool.Name -eq "cmake") {
        $exists = Find-Cmake
    } elseif ($tool.Name -eq "ctags") {
        $exists = Find-UniversalCtags
    } elseif ($tool.Name -eq "roslyn-lsp") {
        $exists = Find-RoslynLsp
    } elseif ($tool.Name -eq "excalidraw-dir") {
        $exists = Test-Path (Join-Path $env:USERPROFILE "Documents\org\excalidraw")
    } else {
        $exists = Get-Command $tool.Name -ErrorAction SilentlyContinue
    }
    if ($exists) {
        $version = Get-ToolVersion $tool.Name
        $script:passed++
        if (-not $Quiet) {
            if ($version) {
                Write-Host "  " -NoNewline
                Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
                Write-Host " $($tool.Name) ($version)"
            } else {
                Write-Host "  " -NoNewline
                Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
                Write-Host " $($tool.Name)"
            }
        }
        return $true
    } else {
        $script:failed++
        $script:missingTools += $tool
        Write-Host "  " -NoNewline
        Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
        Write-Host " $($tool.Name) " -NoNewline
        Write-Host "(not found)" -ForegroundColor DarkGray
        Write-Host "    " -NoNewline
        Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
        Write-Host " Install: " -NoNewline
        Write-Host $tool.Fix -ForegroundColor Cyan
        return $false
    }
}

function Install-MissingTools {
    if ($script:missingTools.Count -eq 0) {
        Write-Host "Nothing to fix!" -ForegroundColor Green
        return
    }

    Write-Host "Installing missing tools..." -ForegroundColor Cyan
    Write-Host ""

    foreach ($tool in $script:missingTools) {
        if ($tool.Manual) {
            Write-Host "  Manual action required: " -NoNewline
            Write-Host $tool.Fix -ForegroundColor Yellow
            Write-Host "  Review and supply approved exact versions; doctor will not authorize downloads." -ForegroundColor DarkGray
            Write-Host ""
            continue
        }
        Write-Host "  Running: " -NoNewline
        Write-Host $tool.Fix -ForegroundColor Yellow
        Invoke-Expression $tool.Fix
        Write-Host ""
    }

    Write-Host "Restart your shell, then run doctor.ps1 again to verify." -ForegroundColor Cyan
}

function Test-DefenderExclusions {
    # Exclusions are optional mitigations, not evidence that Emacs is healthy.
    $requiredExclusions = @(Get-DotfilesEmacsPaths | Select-Object -Unique)

    try {
        $currentExclusions = (Get-MpPreference -ErrorAction Stop).ExclusionPath
        if (-not $currentExclusions) { $currentExclusions = @() }

        $missing = @()
        foreach ($path in $requiredExclusions) {
            # Only check paths that actually exist
            if (Test-Path $path) {
                $found = $false
                foreach ($exc in $currentExclusions) {
                    # Check if the required path is covered by an existing exclusion
                    if ($path -eq $exc -or
                        $path.StartsWith($exc.TrimEnd('\') + '\', [StringComparison]::OrdinalIgnoreCase)) {
                        $found = $true
                        break
                    }
                }
                if (-not $found) {
                    $missing += $path
                }
            }
        }

        if ($missing.Count -eq 0) {
            if (-not $Quiet) {
                Write-Host "  [info] Existing Emacs paths are covered by Defender exclusions."
            }
        } else {
            $script:advisories++
            Write-Host "  [advisory] Defender exclusion coverage is not confirmed by this query." -ForegroundColor Yellow
            foreach ($path in $missing) {
                Write-Host "    $path"
            }
            Write-Host "    No exclusion is required to pass doctor. Keep organizational protection policy intact."
            Write-Host "    Exclusions may be hidden in non-elevated sessions; an elevated check is authoritative."
        }
    } catch {
        $script:advisories++
        Write-Warning "Defender exclusion status could not be queried: $($_.Exception.Message)"
    }
}

function Test-Environment {
    $results = @()

    # Check HOME env var
    $homeDir = $env:HOME
    if ($homeDir) {
        $script:passed++
        if (-not $Quiet) {
            Write-Host "  " -NoNewline
            Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
            Write-Host " HOME set to $homeDir"
        }
    } else {
        $script:failed++
        Write-Host "  " -NoNewline
        Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
        Write-Host " HOME " -NoNewline
        Write-Host "(not set)" -ForegroundColor DarkGray
        Write-Host "    " -NoNewline
        Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
        Write-Host " Set HOME environment variable to your user directory" -ForegroundColor Cyan
    }

    # Check doom in PATH
    $doom = Get-Command doom -ErrorAction SilentlyContinue
    if ($doom) {
        $script:passed++
        if (-not $Quiet) {
            Write-Host "  " -NoNewline
            Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
            Write-Host " doom in PATH"
        }
    } else {
        $script:failed++
        Write-Host "  " -NoNewline
        Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
        Write-Host " doom " -NoNewline
        Write-Host "(not in PATH)" -ForegroundColor DarkGray
        Write-Host "    " -NoNewline
        Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
        Write-Host " Add ~/.config/emacs/bin to PATH" -ForegroundColor Cyan
    }

    # Check doom config symlink
    $doomConfigPath = Join-Path $env:USERPROFILE ".config\doom"
    if (Test-Path $doomConfigPath) {
        $item = Get-Item $doomConfigPath -Force
        if ($item.LinkType -eq "SymbolicLink" -or (Test-Path (Join-Path $doomConfigPath "init.el"))) {
            $script:passed++
            if (-not $Quiet) {
                Write-Host "  " -NoNewline
                Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
                Write-Host " doom config at $doomConfigPath"
            }
        } else {
            $script:failed++
            Write-Host "  " -NoNewline
            Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
            Write-Host " doom config " -NoNewline
            Write-Host "(exists but invalid)" -ForegroundColor DarkGray
            Write-Host "    " -NoNewline
            Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
            Write-Host " Run: .\install.ps1" -ForegroundColor Cyan
        }
    } else {
        $script:failed++
        Write-Host "  " -NoNewline
        Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
        Write-Host " doom config symlink " -NoNewline
        Write-Host "(missing)" -ForegroundColor DarkGray
        Write-Host "    " -NoNewline
        Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
        Write-Host " Run: .\install.ps1" -ForegroundColor Cyan
    }
}

function Test-EmacsCapability($name, $expression, $remedy) {
    $oldPath = $env:PATH
    $exitCode = 1
    try {
        $nativeRoot = $env:EMACS_NATIVE_COMP_ROOT
        if (-not $nativeRoot) {
            $nativeRoot = [Environment]::GetEnvironmentVariable('EMACS_NATIVE_COMP_ROOT', 'User')
        }
        if ($nativeRoot) { $env:PATH = "$(Join-Path $nativeRoot 'bin');$env:PATH" }
        & emacs --quick --batch --eval $expression 2>$null
        $exitCode = $LASTEXITCODE
    } catch {
        Write-Warning "Could not check ${name}: $($_.Exception.Message)"
    } finally {
        $env:PATH = $oldPath
    }
    if ($exitCode -eq 0) {
        $script:passed++
        if (-not $Quiet) {
            Write-Host "  " -NoNewline
            Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
            Write-Host " $name"
        }
    } else {
        $script:failed++
        Write-Host "  " -NoNewline
        Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
        Write-Host " $name " -NoNewline
        Write-Host "(unavailable)" -ForegroundColor DarkGray
        Write-Host "    " -NoNewline
        Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
        Write-Host " $remedy" -ForegroundColor Cyan
    }
}

# Main execution
Write-Host ""
Write-Host "Doom Emacs Doctor" -ForegroundColor Magenta
Write-Host "=================" -ForegroundColor Magenta
Write-Host ""

Write-Host "Core Tools" -ForegroundColor White
foreach ($tool in $coreTools) {
    Test-Tool $tool | Out-Null
}
Write-Host ""

Write-Host "Dev Prerequisites" -ForegroundColor White
foreach ($tool in $devTools) {
    Test-Tool $tool | Out-Null
}
Write-Host ""

Write-Host "Environment" -ForegroundColor White
Test-Environment
Test-EmacsCapability "Emacs file notifications" `
    '(unless (and (require (quote filenotify) nil t) (fboundp (quote file-notify-add-watch))) (kill-emacs 1))' `
    "Install the current GNU.Emacs winget build"
Test-EmacsCapability "Emacs SVG images" `
    '(unless (image-type-available-p (quote svg)) (kill-emacs 1))' `
    "Install a GNU Emacs build with SVG support"
if ($env:EMACS_NATIVE_COMP_ROOT -or
    [Environment]::GetEnvironmentVariable('EMACS_NATIVE_COMP_ROOT', 'User')) {
    Test-EmacsCapability "Emacs native compilation" `
        '(unless (native-comp-available-p) (kill-emacs 1))' `
        "Repair the approved compiler runtime selected by EMACS_NATIVE_COMP_ROOT"
}
Write-Host ""

Write-Host "Performance" -ForegroundColor White
Test-DefenderExclusions
Write-Host ""

# Summary
$total = $script:passed + $script:failed
Write-Host "Summary: " -NoNewline
if ($script:failed -eq 0) {
    Write-Host "$($script:passed)/$total checks passed" -ForegroundColor Green
} else {
    Write-Host "$($script:passed)/$total checks passed" -ForegroundColor Yellow
}
Write-Host "Advisories: $($script:advisories)"
Write-Host ""

# Install missing tools if -Fix flag specified
if ($Fix -and $script:missingTools.Count -gt 0) {
    Install-MissingTools
}

# Exit code
if ($script:failed -gt 0) {
    exit 1
}
exit 0
