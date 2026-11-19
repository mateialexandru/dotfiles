#Requires -Version 7.3
param([switch]$Doom)

$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\emacs-windows.ps1"
function Assert-True($condition, $message) {
    if (-not $condition) { throw $message }
}

if ($Doom) {
    $runtime = Get-DotfilesEmacsRuntime
    $server = "dotfiles-smoke-$PID-$([guid]::NewGuid().ToString('N').Substring(0, 8))"
    $serverFile = Join-Path $env:USERPROFILE ".config\emacs\server\$server"
    $daemon = $null
    try {
        $daemon = Get-Process -Id (Invoke-DotfilesEmacs -Arguments @("--daemon=$server"))
        $deadline = (Get-Date).AddSeconds(180)
        $ready = $false
        do {
            Start-Sleep -Seconds 1
            if (Test-Path -LiteralPath $serverFile) {
                $reply = & $runtime.Client -f $serverFile --eval '(emacs-pid)' 2>&1
                if ($LASTEXITCODE -eq 0) { $ready = $true; break }
            }
        } until ((Get-Date) -ge $deadline)
        Assert-True $ready "Owned smoke daemon did not become ready: $reply"
        $probe = @'
(progn
  (unless (and (not init-file-had-error) (bound-and-true-p doom-init-time)
               (featurep 'doom) (> (hash-table-count doom-modules) 20))
    (error "Doom did not initialize"))
  (let ((ghostel-module-auto-install nil))
    (require 'ghostel)
    (unless (featurep 'ghostel-module) (error "Ghostel module did not load")))
  (let ((frame (make-frame '((window-system . w32) (visibility . nil)))))
    (unwind-protect
        (with-selected-frame frame
          (unless (and (display-graphic-p)
                       (image-size (create-image "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2\" height=\"3\"/>" 'svg t) t))
            (error "Windows GUI/SVG failed")))
      (delete-frame frame t)))
  (list :pid (emacs-pid) :version emacs-version :native (native-comp-available-p)
        :ghostel (featurep 'ghostel-module) :modules (hash-table-count doom-modules)))
'@
        & $runtime.Client -f $serverFile --eval $probe
        Assert-True ($LASTEXITCODE -eq 0) 'Doom/Ghostel/GUI smoke check failed'
        Write-Host 'PASS: separate Doom/Ghostel/GUI smoke check'
    } finally {
        if (Test-Path -LiteralPath $serverFile) {
            & $runtime.Client -f $serverFile --eval '(kill-emacs)'
            if ($LASTEXITCODE -ne 0) { Write-Warning 'Owned smoke daemon refused graceful shutdown.' }
        }
        if ($daemon) {
            if (-not $daemon.WaitForExit(30000)) { Stop-Process -Id $daemon.Id -ErrorAction Stop }
            $daemon.Dispose()
        }
    }
    return
}

$fixture = Join-Path ([IO.Path]::GetTempPath()) ("dotfiles-emacs-" + [guid]::NewGuid().ToString('N'))
$originalExit = $global:LASTEXITCODE
New-Item -ItemType Directory -Path "$fixture\scripts", "$fixture\.build\emacs" -Force | Out-Null
try {
    Copy-Item "$PSScriptRoot\build-emacs-windows.ps1", "$PSScriptRoot\build-emacs-windows.sh",
        "$PSScriptRoot\test-emacs-windows.el", "$PSScriptRoot\emacs-windows.ps1" "$fixture\scripts"
    & {
        . "$fixture\scripts\build-emacs-windows.ps1"
        $root = Join-Path $fixture '.build\emacs'
        $msys = Join-Path $root 'msys64'
        New-Item -ItemType Directory -Path "$msys\usr\bin" -Force | Out-Null
        New-Item -ItemType File -Path "$msys\usr\bin\bash.exe", "$msys\.dotfiles-initialized" | Out-Null
        $accepted = Join-Path $root 'accepted.json'
        $oldExe = Join-Path $root 'old.exe'
        Set-Content $oldExe 'original'
        $script:calls = @()
        function Test-Inventory { $global:LASTEXITCODE = 0; 'fixture-package 1.0' }
        function Get-CimInstance {
            if ($case -eq 'in-use') { [pscustomobject]@{ ExecutablePath = "$root\old.exe" } }
        }
        function Invoke-EmacsBuildStage {
            param($Stage, $Extra)
            $script:calls += $Stage
            if ($Stage -eq 'dependencies') { Set-Variable bash 'Test-Inventory' -Scope 1 }
            if ($Stage -eq 'source') {
                if ($case -eq 'initial') {
                    $script:baseline = @{
                        identity = 'original-build-identity'
                        sourceRevision = $configuration.sourceRevision
                        configureOptions = @($configuration.configureOptions)
                        recipeVersion = $configuration.recipeVersion
                        validationHash = $validationHash
                        packages = @($inventory)
                        compilerRoot = $compilerRoot
                        validatedAt = [datetime]::UtcNow.ToString('o')
                    }
                }
                throw 'Fixture stops before source/build'
            }
            if ($Stage -eq 'update' -and $case -eq 'failed-update') { throw 'Fixture update failure' }
        }
        function Invoke-BuildNative {
            param($Executable, $Arguments)
            $script:calls += 'validate'
            if ($case -eq 'validation-fails') { throw 'Fixture validation failure' }
        }
        foreach ($case in @('initial', 'cached', 'orchestration-changed', 'tests-changed', 'legacy',
            'validation-fails', 'source-changed', 'options-changed', 'toolchain-changed', 'compiler-changed', 'toolchain-reinitialized',
            'recipe-changed', 'corrupt', 'rebuild', 'update', 'in-use', 'failed-update')) {
            $script:calls = @()
            $script:msysRestartRequested = $false
            $beforePath = $env:PATH
            $beforeMsystem = $env:MSYSTEM
            if ($case -ne 'initial') {
                Set-Content $oldExe 'original'
                $receipt = $script:baseline.Clone()
                $receipt.executable = $oldExe
                $receipt.executableSha256 = (Get-FileHash $oldExe).Hash
                $receipt.validationHash = (Get-FileHash "$fixture\scripts\test-emacs-windows.el").Hash
                switch ($case) {
                    'source-changed' { $receipt.sourceRevision = '0' * 40 }
                    'options-changed' { $receipt.configureOptions = @('--without-modules') }
                    'toolchain-changed' { $receipt.packages = @('fixture-package 0.1') }
                    'compiler-changed' { $receipt.compilerRoot = "$fixture\other-compiler" }
                    'toolchain-reinitialized' { $receipt.validatedAt = '2000-01-01T00:00:00Z' }
                    'recipe-changed' { $receipt.recipeVersion = 0 }
                    'legacy' { $receipt.Remove('recipeVersion'); $receipt.Remove('validationHash') }
                    'validation-fails' { $receipt.validationHash = 'older-checks' }
                }
                $receipt | ConvertTo-Json | Set-Content $accepted
            }
            if ($case -eq 'orchestration-changed') {
                Add-Content "$fixture\scripts\build-emacs-windows.ps1" '# orchestration-only refactor'
                Add-Content "$fixture\scripts\build-emacs-windows.sh" '# comment-only change'
            }
            if ($case -eq 'tests-changed') { Add-Content "$fixture\scripts\test-emacs-windows.el" ';; additional checks' }
            if ($case -eq 'corrupt') { Set-Content $oldExe 'corrupted' }
            $beforeReceipt = if (Test-Path $accepted) { Get-Content $accepted -Raw }
            $failure = $null
            try { Invoke-EmacsBuild -Rebuild:($case -eq 'rebuild') -UpdateTools:($case -in @('update', 'in-use', 'failed-update')) }
            catch { $failure = $_ }
            $revalidated = $case -in @('tests-changed', 'legacy')
            Assert-True (($null -eq $failure) -eq ($case -in @('cached', 'orchestration-changed') -or $revalidated)) "Unexpected $case result: $failure"
            if ($case -in @('cached', 'orchestration-changed')) {
                Assert-True (($script:calls -join ',') -eq 'dependencies') 'Cache did not skip source/build'
            } elseif ($revalidated -or $case -eq 'validation-fails') {
                Assert-True (($script:calls -join ',') -eq 'dependencies,validate') 'Changed tests rebuilt Emacs instead of revalidating'
                if ($revalidated) {
                    $after = Get-Content $accepted -Raw | ConvertFrom-Json
                    Assert-True ($after.identity -eq $receipt.identity -and $after.executable -eq $oldExe) 'Revalidation rewrote build provenance'
                    $script:calls = @()
                    $beforeReceipt = Get-Content $accepted -Raw
                    Invoke-EmacsBuild
                    Assert-True (($script:calls -join ',') -eq 'dependencies') 'Revalidation did not make the next run a no-op'
                }
            } elseif ($case -eq 'in-use') {
                Assert-True ($script:calls.Count -eq 0) 'Updated an in-use compiler'
            } elseif ($case -eq 'failed-update') {
                Assert-True (-not (Test-Path "$msys\.dotfiles-initialized")) 'Failed update still marked initialized'
            } else {
                $expected = if ($case -eq 'update') { 'update,update,dependencies,source' } else { 'dependencies,source' }
                Assert-True (($script:calls -join ',') -eq $expected) 'Unexpected build order'
            }
            Assert-True ($env:PATH -eq $beforePath -and $env:MSYSTEM -eq $beforeMsystem) 'Builder leaked its environment'
            if ($beforeReceipt) {
                Assert-True ((Get-Content $accepted -Raw) -eq $beforeReceipt) 'Failed/skipped build replaced acceptance'
            }
            Write-Host "PASS: build $case"
        }
        New-Item -ItemType File -Path "$msys\.dotfiles-initialized" -Force | Out-Null
        $case = 'pending'
        function Invoke-EmacsBuildStage {
            param($Stage, $Extra)
            $script:calls += $Stage
            if ($Stage -eq 'dependencies') { Set-Variable bash 'Test-Inventory' -Scope 1 }
            if ($Stage -eq 'build') {
                New-Item -ItemType Directory -Path "$($Extra[0])\install\bin" -Force | Out-Null
                Set-Content -LiteralPath "$($Extra[0])\install\bin\emacs.exe" 'completed candidate'
            }
        }
        function Invoke-BuildNative {
            param($Executable, $Arguments)
            $script:calls += 'validate'
            throw 'Fixture TLS validation failure'
        }
        $beforeReceipt = Get-Content $accepted -Raw
        foreach ($attempt in @('first', 'retry', 'changed-tests', 'changed-inputs', 'corrupt', 'reinitialized', 'forced')) {
            $pendingPath = Join-Path $root 'pending.json'
            if ($attempt -eq 'changed-tests') {
                Add-Content "$fixture\scripts\test-emacs-windows.el" ';; fixed TLS state'
            }
            if ($attempt -in @('changed-inputs', 'corrupt', 'reinitialized')) {
                $pending = Get-Content $pendingPath -Raw | ConvertFrom-Json
                switch ($attempt) {
                    'changed-inputs' { $pending.identity = 'different-recipe' }
                    'corrupt' { Set-Content -LiteralPath $pending.executable 'corrupted candidate' }
                    'reinitialized' { $pending.completedAt = '2000-01-01T00:00:00Z' }
                }
                $pending | ConvertTo-Json | Set-Content $pendingPath
            }
            $script:calls = @()
            $failure = $null
            try { Invoke-EmacsBuild -Rebuild:($attempt -eq 'forced') } catch { $failure = $_ }
            Assert-True ($failure -match 'Fixture TLS validation failure') "Unexpected pending $attempt failure: $failure"
            $expected = if ($attempt -in @('retry', 'changed-tests')) {
                'dependencies,validate'
            } else {
                'dependencies,source,build,validate'
            }
            Assert-True (($script:calls -join ',') -eq $expected) "Unexpected pending $attempt stages: $script:calls"
            Assert-True (Test-Path $pendingPath) 'Validation failure lost the completed candidate'
            Assert-True ((Get-Content $accepted -Raw) -eq $beforeReceipt) 'Pending candidate replaced accepted build'
            Write-Host "PASS: pending candidate $attempt"
        }
        Remove-Item -LiteralPath $msys -Recurse -Force
        function Invoke-WebRequest {
            param($Uri, $OutFile)
            Set-Content -LiteralPath $OutFile 'not the pinned bootstrap'
        }
        $failed = $false
        try { Invoke-EmacsBuild } catch { $failed = $_ -match 'SHA-256 mismatch' }
        Assert-True $failed 'Corrupt cached bootstrap was not rejected before extraction'
        $lock = [IO.File]::Open("$root\build.lock", 'Open', 'ReadWrite', 'None')
        try {
            $failed = $false
            try { Invoke-EmacsBuild } catch { $failed = $_.Exception.InnerException -is [IO.IOException] }
            Assert-True $failed 'Concurrent build was not refused'
        } finally { $lock.Dispose() }
        Write-Host 'PASS: bootstrap hash and exclusive build lock'
    }
    & {
        . "$fixture\scripts\emacs-windows.ps1"
        $receipt = "$fixture\.build\emacs\accepted.json"
        Remove-Item -LiteralPath $receipt
        $failed = $false
        try { Get-DotfilesEmacsRuntime } catch { $failed = $_ -match 'No accepted Emacs build' }
        Assert-True $failed 'Missing build did not fail actionably'
        $hostExe = (Get-Process -Id $PID).Path
        New-Item -ItemType Directory -Path "$fixture\compiler\bin" -Force | Out-Null
        New-Item -ItemType File -Path "$fixture\compiler\bin\libgccjit-0.dll" | Out-Null
        @{ executable = $hostExe; compilerRoot = "$fixture\compiler"; sourceRevision = 'old-but-accepted' } |
            ConvertTo-Json | Set-Content $receipt
        $runtime = Get-DotfilesEmacsRuntime
        Assert-True ($runtime.Executable -eq $hostExe) 'Launch depends on current build configuration'
        $argumentsFile = "$fixture\arguments.ps1"
        Set-Content $argumentsFile @'
param([string]$PathValue, [string]$QuotedValue, [int]$Result, [string]$ExpectedServer)
if ($PathValue -cne 'C:\space path\' -or $QuotedValue -cne 'say "hi"') { exit 2 }
if (-not $env:EMACS_NATIVE_COMP_ROOT -or "$env:EMACS_NATIVE_COMP_ROOT\bin" -notin ($env:PATH -split ';')) { exit 3 }
if ($ExpectedServer -and $env:EMACS_SERVER_FILE -cne $ExpectedServer) { exit 4 }
'captured-output'
exit $Result
'@
        function Get-DotfilesEmacsRuntime {
            [pscustomobject]@{
                Executable = $hostExe; GuiExecutable = $hostExe
                Client = $hostExe; GuiClient = $hostExe; CompilerRoot = "$fixture\compiler"
            }
        }
        $arguments = @('-NoProfile', '-File', $argumentsFile, '-PathValue', 'C:\space path\',
            '-QuotedValue', 'say "hi"', '-Result')
        $beforePath = $env:PATH
        $beforeCompiler = $env:EMACS_NATIVE_COMP_ROOT
        foreach ($gui in @($false, $true)) {
            $output = Invoke-DotfilesEmacs -Gui:$gui -Arguments ($arguments + '0')
            if (-not $gui) { Assert-True ($output -eq 'captured-output') 'Console output was lost' }
            $failed = $false
            try { Invoke-DotfilesEmacs -Gui:$gui -Arguments ($arguments + '9') } catch { $failed = $_ -match 'exit 9' }
            Assert-True $failed 'Process failure was hidden'
            Assert-True ($env:PATH -eq $beforePath -and $env:EMACS_NATIVE_COMP_ROOT -eq $beforeCompiler) 'Launcher leaked environment'
        }
        $beforeServer = $env:EMACS_SERVER_FILE
        try {
            foreach ($configuredServer in @('', 'C:\custom server\connection')) {
                $env:EMACS_SERVER_FILE = $configuredServer
                $expectedServer = if ($configuredServer) { $configuredServer } else {
                    Join-Path $env:USERPROFILE '.config\emacs\server\server'
                }
                foreach ($gui in @($false, $true)) {
                    Invoke-DotfilesEmacs -Client -Gui:$gui -Arguments (
                        $arguments + @('0', '-ExpectedServer', $expectedServer)) | Out-Null
                    Assert-True ([string]$env:EMACS_SERVER_FILE -ceq $configuredServer) 'Client leaked server discovery settings'
                }
            }
        } finally { $env:EMACS_SERVER_FILE = $beforeServer }
        $env:PATH = "C:\DotfilesSessionFixture;C:\DotfilesSessionFixture\;$env:PATH"
        try {
            $actual = Get-DotfilesProcessPath
            $env:PATH = $actual
            Assert-True ($actual.StartsWith('C:\DotfilesSessionFixture;') -and
                $actual -eq (Get-DotfilesProcessPath)) 'PATH refresh lost session overrides or is not idempotent'
        } finally { $env:PATH = $beforePath }
        Write-Host 'PASS: accepted runtime, console/GUI arguments, output, failures, environment and PATH'
    }
    & {
        . "$fixture\scripts\emacs-windows.ps1"
        $hostExe = (Get-Process -Id $PID).Path
        $previousProfile = $env:USERPROFILE
        $env:USERPROFILE = Join-Path $fixture 'daemon home with spaces'
        $doomDirectory = Join-Path $env:USERPROFILE '.config\emacs'
        New-Item -ItemType Directory -Path $doomDirectory -Force | Out-Null
        New-Item -ItemType File -Path "$doomDirectory\early-init.el" | Out-Null
        $launches = [Collections.Generic.List[object]]::new()
        $probes = [Collections.Generic.Queue[bool]]::new()
        $probeTimeouts = [Collections.Generic.List[int]]::new()
        function Get-DotfilesEmacsRuntime {
            [pscustomobject]@{ Client = $hostExe; GuiClient = $hostExe }
        }
        function Test-DotfilesDoomServer {
            param($ClientPath, $ServerFile, $TimeoutMilliseconds)
            Assert-True ($ServerFile -eq "$doomDirectory\server\server") 'Probe used the wrong Doom server'
            $probeTimeouts.Add($TimeoutMilliseconds)
            return $probes.Dequeue()
        }
        function Invoke-DotfilesEmacs {
            param([switch]$Client, [switch]$Gui, [string[]]$Arguments)
            $launches.Add([pscustomobject]@{ Client = $Client; Gui = $Gui; Arguments = $Arguments })
            if (-not $Client) { return $PID }
        }
        try {
            foreach ($states in @(@($true), @($false, $false, $true))) {
                $launches.Clear()
                $probeTimeouts.Clear()
                foreach ($state in $states) { $probes.Enqueue($state) }
                Open-DotfilesDoom
                Assert-True ($probeTimeouts[0] -eq 120000) 'Existing server was not given the full readiness timeout'
                Assert-True ($launches.Count -eq $(if ($states[0]) { 1 } else { 2 })) 'Doom started an unnecessary daemon or omitted startup'
                $clientLaunch = $launches[$launches.Count - 1]
                Assert-True ($clientLaunch.Client -and $clientLaunch.Gui -and
                    ($clientLaunch.Arguments -join '|') -ceq "--server-file|$doomDirectory\server\server|-c|-n") 'Doom did not open a client on its explicit server'
                if (-not $states[0]) {
                    Assert-True (($launches[0].Arguments -join '|') -ceq "--daemon|--init-directory=$doomDirectory") 'Daemon lost its explicit Doom init directory'
                    Assert-True ($probeTimeouts.Count -eq 3 -and
                        $probeTimeouts[1] -gt 5000 -and $probeTimeouts[1] -le 120000 -and
                        $probeTimeouts[2] -lt $probeTimeouts[1]) 'Cold startup probes did not share the remaining readiness budget'
                }
            }
            $launches.Clear()
            $probes.Enqueue($false)
            $failed = $false
            try { Open-DotfilesDoom -StartupTimeoutSeconds 0 } catch { $failed = $_ -match 'timed out' }
            Assert-True ($failed -and $launches.Count -eq 1 -and -not $launches[0].Client) 'Startup timeout was hidden or opened a client too early'
            function Test-DotfilesDoomServer { throw 'Existing server did not respond' }
            $launches.Clear()
            $failed = $false
            try { Open-DotfilesDoom } catch { $failed = $_ -match 'Existing server did not respond' }
            Assert-True ($failed -and $launches.Count -eq 0) 'An unresponsive server triggered a duplicate daemon'
            Write-Host 'PASS: Doom server reuse, cold start, readiness, explicit paths, timeout and unresponsive-server safety'
        } finally { $env:USERPROFILE = $previousProfile }
    }
    & {
        $shortcutRoot = Join-Path $fixture 'shortcut repo with spaces'
        $programs = Join-Path $shortcutRoot 'Programs'
        $homeDirectory = Join-Path $shortcutRoot 'user home'
        $doomDirectory = Join-Path $homeDirectory '.config\emacs'
        $compiler = Join-Path $shortcutRoot 'compiler'
        $receiptPath = Join-Path $shortcutRoot '.build\emacs\accepted.json'
        $previousProfile = $env:USERPROFILE
        $process = $null
        try {
            New-Item -ItemType Directory -Path "$shortcutRoot\scripts", (Split-Path $receiptPath),
                $doomDirectory, "$compiler\bin" -Force | Out-Null
            Copy-Item "$PSScriptRoot\install-emacs-shortcut.ps1", "$PSScriptRoot\emacs-windows.ps1" "$shortcutRoot\scripts"
            New-Item -ItemType File -Path "$compiler\bin\libgccjit-0.dll", "$doomDirectory\early-init.el" | Out-Null
            $env:USERPROFILE = $homeDirectory
            $shell = New-Object -ComObject WScript.Shell
            $shortcutPath = Join-Path $programs 'Doom Emacs.lnk'
            $expectedArguments = "-NoLogo -NoProfile -WindowStyle Hidden -File `"$shortcutRoot\scripts\emacs-windows.ps1`" -DoomClient"
            foreach ($build in @('first build', 'replacement build')) {
                New-Item -ItemType Directory -Path "$shortcutRoot\$build" | Out-Null
                New-Item -ItemType File -Path "$shortcutRoot\$build\emacs.exe", "$shortcutRoot\$build\runemacs.exe",
                    "$shortcutRoot\$build\emacsclient.exe", "$shortcutRoot\$build\emacsclientw.exe" | Out-Null
                @{ executable = "$shortcutRoot\$build\emacs.exe"; compilerRoot = $compiler } |
                    ConvertTo-Json | Set-Content -LiteralPath $receiptPath
                & "$shortcutRoot\scripts\install-emacs-shortcut.ps1" -ProgramsDirectory $programs
                $shortcut = $shell.CreateShortcut($shortcutPath)
                Assert-True ($shortcut.TargetPath -eq (Join-Path $PSHOME 'pwsh.exe')) 'Shortcut bypasses PowerShell 7'
                Assert-True ($shortcut.Arguments -ceq $expectedArguments) 'Shortcut lost the managed launcher, Doom directory, or quoting'
                Assert-True ($shortcut.WorkingDirectory -eq $homeDirectory) 'Shortcut starts in the installer directory'
                Assert-True ($shortcut.IconLocation -eq "$shortcutRoot\$build\runemacs.exe,0") 'Shortcut retained the old build icon'
                $shortcut.Arguments = 'stale shortcut'
                $shortcut.Save()
            }
            foreach ($missing in @($receiptPath, "$doomDirectory\early-init.el", "$shortcutRoot\replacement build\emacsclientw.exe")) {
                $beforeShortcut = (Get-FileHash -LiteralPath $shortcutPath).Hash
                Move-Item -LiteralPath $missing -Destination "$missing.saved"
                try {
                    $failure = $null
                    try { & "$shortcutRoot\scripts\install-emacs-shortcut.ps1" -ProgramsDirectory $programs }
                    catch { $failure = $_ }
                    Assert-True ($null -ne $failure) "Shortcut ignored missing prerequisite: $missing"
                    Assert-True ((Get-FileHash -LiteralPath $shortcutPath).Hash -eq $beforeShortcut) 'Failed setup changed the existing shortcut'
                } finally { Move-Item -LiteralPath "$missing.saved" -Destination $missing }
            }
            & "$shortcutRoot\scripts\install-emacs-shortcut.ps1" -ProgramsDirectory $programs
            Set-Content -LiteralPath "$shortcutRoot\scripts\emacs-windows.ps1" @'
param([switch]$DoomClient)
if (-not $DoomClient) { exit 9 }
exit 0
'@
            $shortcut = $shell.CreateShortcut($shortcutPath)
            $start = [Diagnostics.ProcessStartInfo]::new($shortcut.TargetPath)
            $start.UseShellExecute = $false
            $start.Arguments = $shortcut.Arguments
            $process = [Diagnostics.Process]::Start($start)
            $process.WaitForExit()
            Assert-True ($process.ExitCode -eq 0) 'Windows could not pass the shortcut arguments to the GUI launcher'
            Write-Host 'PASS: Start menu shortcut creation, repair, accepted build changes, failures and client-or-daemon invocation'
        } finally {
            if ($process) { $process.Dispose() }
            $env:USERPROFILE = $previousProfile
        }
    }
} finally {
    Remove-Item -LiteralPath $fixture -Recurse -Force
    $global:LASTEXITCODE = $originalExit
}
