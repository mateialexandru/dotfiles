# scripts/Test-DotNetLsp.ps1
# Validates DotNet LSP setup (Roslyn via --stdio) against a C# project.
# Catches missing .sln/.csproj, broken dotnet runtime, or DLL problems.
# Output is designed for Emacs compilation-mode (file:line for errors).

param(
    [string]$Path = "."
)

$Path = Resolve-Path $Path -ErrorAction Stop

# Track results
$script:passed = 0
$script:failed = 0

function Write-Ok($label, $detail) {
    $script:passed++
    $detailStr = if ($detail) { " ($detail)" } else { "" }
    Write-Host "ok: $label$detailStr"
}

function Write-Fail($label, $detail) {
    $script:failed++
    $detailStr = if ($detail) { " ($detail)" } else { "" }
    # Point at the calling line in this script so Emacs jumps to the relevant check
    $line = (Get-PSCallStack)[1].ScriptLineNumber
    Write-Host "${PSCommandPath}:${line}: error: $label$detailStr"
}

function Write-Hint($message) {
    Write-Host "  hint: $message"
}

# --- Main ---

Write-Host ""
Write-Host "DotNet LSP Test"
Write-Host "================"
Write-Host "  Project: $Path"
Write-Host ""

# Check 1: dotnet available
$dotnet = Get-Command dotnet -ErrorAction SilentlyContinue
if ($dotnet) {
    $dotnetVersion = & dotnet --version 2>$null
    Write-Ok "dotnet" $dotnetVersion
} else {
    Write-Fail "dotnet not found"
    Write-Hint "Install: winget install Microsoft.DotNet.SDK.8"
    Write-Host ""
    Write-Host "Summary: $($script:passed)/$($script:passed + $script:failed) checks passed"
    Write-Host ""
    exit 1
}

# Check 2: Roslyn DLL exists
$roslynDll = Join-Path $env:LOCALAPPDATA "roslyn-lsp\Microsoft.CodeAnalysis.LanguageServer.dll"
if (Test-Path $roslynDll) {
    Write-Ok "Roslyn LSP DLL"
} else {
    Write-Fail "Roslyn LSP DLL not found"
    Write-Hint "Install: .\scripts\install-roslyn-lsp.ps1"
}

# Check 3: Project file found (walk upward from $Path toward drive root)
$projectFile = $null
$searchedDirs = @()
$searchDir = $Path
while ($searchDir) {
    $searchedDirs += $searchDir
    $found = Get-ChildItem -Path $searchDir -Filter "*.sln" -File -ErrorAction SilentlyContinue |
             Select-Object -First 1
    if (-not $found) {
        $found = Get-ChildItem -Path $searchDir -Filter "*.csproj" -File -ErrorAction SilentlyContinue |
                 Select-Object -First 1
    }
    if ($found) { $projectFile = $found; break }
    $parent = Split-Path $searchDir -Parent
    if ($parent -eq $searchDir) { break }
    $searchDir = $parent
}
if ($projectFile) {
    Write-Ok "Project file" "$($projectFile.Name) in $($projectFile.DirectoryName)"
} else {
    Write-Fail "no .sln or .csproj found"
    foreach ($dir in $searchedDirs) { Write-Host "  searched: $dir" }
    Write-Hint "Roslyn needs a .sln or .csproj to provide language features"
}

# Check 4: dotnet restore (run it automatically if needed)
if ($projectFile) {
    $projectDir = $projectFile.DirectoryName
    $objDirs = Get-ChildItem -Path $projectDir -Filter "obj" -Directory -Recurse -Depth 3 -ErrorAction SilentlyContinue
    if ($objDirs) {
        Write-Ok "Packages restored"
    } else {
        Write-Host "  Restoring packages..."
        $restoreOutput = & dotnet restore $projectFile.FullName 2>&1
        $restoreOk = $LASTEXITCODE -eq 0
        if ($restoreOk) {
            Write-Ok "Packages restored" "dotnet restore succeeded"
        } else {
            Write-Fail "dotnet restore failed"
            $restoreOutput | ForEach-Object { Write-Host "  $_" }
        }
    }
} else {
    Write-Fail "packages not restored" "no project file"
}

# Check 5: LSP smoke test — start server with --stdio and send an LSP initialize request
if (Test-Path $roslynDll) {
    $tempLogDir = Join-Path $env:TEMP "roslyn-lsp-test-$(Get-Random)"
    New-Item -ItemType Directory -Force -Path $tempLogDir | Out-Null

    $psi = [System.Diagnostics.ProcessStartInfo]::new()
    $psi.FileName = $dotnet.Source
    $psi.Arguments = "`"$roslynDll`" --logLevel Information --extensionLogDirectory `"$tempLogDir`" --stdio"
    $psi.UseShellExecute = $false
    $psi.RedirectStandardInput = $true
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.CreateNoWindow = $true

    $proc = $null
    $serverAlive = $false
    try {
        $proc = [System.Diagnostics.Process]::Start($psi)

        # Send a minimal LSP initialize request over stdin (JSON-RPC)
        $initRequest = '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":' + $PID + ',"capabilities":{}}}'
        $header = "Content-Length: $($initRequest.Length)`r`n`r`n"
        $proc.StandardInput.Write($header)
        $proc.StandardInput.Write($initRequest)
        $proc.StandardInput.Flush()

        # Read response: expect "Content-Length:" header then JSON-RPC response
        $deadline = (Get-Date).AddSeconds(30)
        $outputTask = $proc.StandardOutput.ReadLineAsync()

        while ((Get-Date) -lt $deadline) {
            if ($proc.HasExited) { break }
            if ($outputTask.IsCompleted) {
                $line = $outputTask.Result
                if ($line -match 'Content-Length') {
                    $serverAlive = $true
                    break
                }
                $outputTask = $proc.StandardOutput.ReadLineAsync()
            }
            Start-Sleep -Milliseconds 200
        }

        if ($serverAlive) {
            Write-Ok "LSP server responds" "stdio JSON-RPC OK"
        } else {
            if ($proc.HasExited) {
                Write-Fail "LSP server exited with code $($proc.ExitCode)"
            } else {
                Write-Fail "LSP server no response within 30s"
            }
            Write-Hint "Try manually: dotnet `"$roslynDll`" --stdio"
        }
    } catch {
        Write-Fail "LSP server error: $($_.Exception.Message)"
        Write-Hint "Try manually: dotnet `"$roslynDll`" --stdio"
    } finally {
        if ($proc -and -not $proc.HasExited) {
            $proc.Kill()
            $null = $proc.WaitForExit(5000)
        }
        if ($proc) { $proc.Dispose() }
        Remove-Item $tempLogDir -Recurse -Force -ErrorAction SilentlyContinue
    }
} else {
    Write-Fail "LSP server skipped" "DLL not found"
}

# Summary
Write-Host ""
$total = $script:passed + $script:failed
if ($script:failed -eq 0) {
    Write-Host "Summary: $($script:passed)/$total checks passed"
} else {
    Write-Host "Summary: $($script:passed)/$total checks passed, $($script:failed) failed"
}
Write-Host ""

if ($script:failed -gt 0) {
    exit 1
}
exit 0
