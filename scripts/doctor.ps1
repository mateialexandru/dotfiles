# scripts/doctor.ps1
# Validates Doom Emacs prerequisites are installed and configured correctly

param(
    [switch]$Quiet,  # Only show failures
    [switch]$Fix     # Install missing tools
)

# Tool definitions - easily expandable
$coreTools = @(
    @{ Name = "emacs"; Fix = "winget install GNU.Emacs" }
    @{ Name = "rg"; Fix = "winget install BurntSushi.ripgrep.MSVC" }
    @{ Name = "fd"; Fix = "winget install sharkdp.fd" }
    @{ Name = "clangd"; Fix = "winget install LLVM.clangd" }
    @{ Name = "pandoc"; Fix = "winget install JohnMacFarlane.Pandoc" }
    @{ Name = "shellcheck"; Fix = "winget install koalaman.shellcheck" }
    @{ Name = "git"; Fix = "winget install Git.Git" }
)

$devTools = @(
    @{ Name = "ctags"; Fix = "winget install UniversalCtags.Ctags" }
    @{ Name = "node"; Fix = "winget install OpenJS.NodeJS.LTS" }
    @{ Name = "npm"; Fix = "winget install OpenJS.NodeJS.LTS" }
    @{ Name = "dotnet"; Fix = "winget install Microsoft.DotNet.SDK.8" }
    @{ Name = "jq"; Fix = "winget install jqlang.jq" }
    @{ Name = "cmake"; Fix = "winget install Kitware.CMake" }
    @{ Name = "java"; Fix = "winget install Microsoft.OpenJDK.21" }
    @{ Name = "plantuml"; Fix = "New-Item -ItemType Directory -Force -Path `"$env:LOCALAPPDATA\plantuml`" | Out-Null; Invoke-WebRequest -Uri 'https://github.com/plantuml/plantuml/releases/latest/download/plantuml.jar' -OutFile `"$env:LOCALAPPDATA\plantuml\plantuml.jar`"" }
    @{ Name = "roslyn-lsp"; Fix = "& `"$PSScriptRoot\install-roslyn-lsp.ps1`"" }
    @{ Name = "yaml-language-server"; Fix = "npm install -g yaml-language-server" }
    @{ Name = "mmdc"; Fix = "npm install -g @mermaid-js/mermaid-cli" }
)

# Track results
$script:passed = 0
$script:failed = 0
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
            "pandoc" { & pandoc --version 2>$null | Select-Object -First 1 }
            "shellcheck" { & shellcheck --version 2>$null | Where-Object { $_ -match "version:" } }
            "ctags" {
                # Must be Universal Ctags, not Emacs etags
                $ver = & ctags --version 2>$null | Select-Object -First 1
                if ($ver -match "Universal Ctags") { $ver }
            }
            "jq" { & jq --version 2>$null }
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
        Write-Host "  Running: " -NoNewline
        Write-Host $tool.Fix -ForegroundColor Yellow
        Invoke-Expression $tool.Fix
        Write-Host ""
    }

    Write-Host "Restart your shell, then run doctor.ps1 again to verify." -ForegroundColor Cyan
}

function Test-DefenderExclusions {
    # Paths that should be excluded from Windows Defender scanning for Emacs performance
    $requiredExclusions = @(
        (Join-Path $env:USERPROFILE ".config\emacs")
        (Join-Path $env:USERPROFILE ".emacs.d")
        "C:\Program Files\Emacs"
    )

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
                    if ($path -like "$exc*" -or $path -eq $exc) {
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
            $script:passed++
            if (-not $Quiet) {
                Write-Host "  " -NoNewline
                Write-Host ([char]0x2713) -ForegroundColor Green -NoNewline
                Write-Host " Windows Defender exclusions configured"
            }
        } else {
            $script:failed++
            Write-Host "  " -NoNewline
            Write-Host ([char]0x2717) -ForegroundColor Red -NoNewline
            Write-Host " Windows Defender exclusions " -NoNewline
            Write-Host "(missing)" -ForegroundColor DarkGray
            foreach ($path in $missing) {
                Write-Host "    " -NoNewline
                Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
                Write-Host " Missing: $path" -ForegroundColor Cyan
            }
            Write-Host "    " -NoNewline
            Write-Host ([char]0x2192) -ForegroundColor Yellow -NoNewline
            Write-Host " Run as Admin: " -NoNewline
            Write-Host ".\scripts\setup-doom.ps1 -AddDefenderExclusions" -ForegroundColor Cyan
        }
    } catch {
        # Can't read Defender prefs (maybe not admin or Defender disabled)
        $script:passed++
        if (-not $Quiet) {
            Write-Host "  " -NoNewline
            Write-Host ([char]0x2713) -ForegroundColor DarkGray -NoNewline
            Write-Host " Windows Defender exclusions (skipped - cannot query)"
        }
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
