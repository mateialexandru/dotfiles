#Requires -Version 7.3
# Offline provisioning tests: execute the real bootstrap flow with mocked boundaries.
[Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseDeclaredVarsMoreThanAssignments', '',
    Justification = 'Fixture variables are consumed by extracted production script blocks.')]
[CmdletBinding()]
param()
$ErrorActionPreference = 'Stop'
$tokens = $null
$errors = $null
$ast = [Management.Automation.Language.Parser]::ParseFile(
    (Join-Path $PSScriptRoot 'install-python-runtime.ps1'), [ref]$tokens, [ref]$errors)
if ($errors.Count) { throw ($errors -join "`n") }
$install = $ast.Find({
    param($node)
    $node -is [Management.Automation.Language.FunctionDefinitionAst] -and
    $node.Name -eq 'Install-OfficialPackage'
}, $true)
$start = $ast.EndBlock.Statements | Where-Object { $_.Extent.Text -like '$python = Get-DotfilesPython*' }
if (-not $install -or @($start).Count -ne 1) { throw 'Bootstrap flow not found.' }
$flow = [scriptblock]::Create(($ast.EndBlock.Statements |
    Where-Object { $_.Extent.StartOffset -ge $start.Extent.StartOffset } |
    ForEach-Object { $_.Extent.Text }) -join "`n")
$directory = Join-Path ([IO.Path]::GetTempPath()) "python-bootstrap-$([guid]::NewGuid())"
$oldPath = $env:PATH
$oldExit = $global:LASTEXITCODE
$managerConfig = Join-Path $PSScriptRoot '..\config\python\windows-manager.json'
New-Item -ItemType Directory -Path $directory | Out-Null
try {
    $managerProbe = Join-Path $directory 'pymanager.ps1'
    @'
$state.managerCalls += ,@($args)
$global:LASTEXITCODE = 0
if ($args[0] -eq 'install') {
    $state.installed = $true
    if ($state.case -eq 'runtime-failure') { $global:LASTEXITCODE = 2 }
} elseif ($args[0] -eq 'list') {
    if ($state.case -eq 'list-failure') { $global:LASTEXITCODE = 2 }
    'C:\Official Python\python.exe'
} else { throw 'Unexpected manager command.' }
'@ | Set-Content -LiteralPath $managerProbe

    foreach ($case in @('existing', 'check-existing', 'missing-no-consent', 'check-missing',
                         'manager-present', 'fresh', 'store-failure', 'runtime-failure',
                         'list-failure', 'uv-missing-no-consent', 'uv-origin-failure')) {
        & {
            $Check = $case -like 'check-*' -or $case -eq 'uv-origin-failure'
            $AllowDownloads = $case -in @('manager-present', 'fresh', 'store-failure', 'runtime-failure', 'list-failure')
            $PythonSeries = '3.13'
            $state = @{
                case = $case
                installed = $case -in @('existing', 'check-existing', 'uv-missing-no-consent', 'uv-origin-failure')
                manager = $case -in @('manager-present', 'runtime-failure', 'list-failure')
                uv = $case -notin @('fresh', 'uv-missing-no-consent')
                check = $Check
                wingetCalls = @(); managerCalls = @(); repairs = 0; policies = @()
            }
            function Get-DotfilesPython {
                param($Python, [switch]$AllowMissing)
                if (-not $state.installed) {
                    if ($AllowMissing) { return $null }
                    throw 'Official Python missing.'
                }
                if ($Python -and $Python -ne 'C:\Official Python\python.exe') { throw 'Incorrect runtime path.' }
                @{ executable = 'C:\Official Python\python.exe'; version = '3.13.17' }
            }
            function Get-Command {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '',
                    Justification = 'Isolate package-manager discovery in offline bootstrap tests.')]
                [OutputType([hashtable])]
                [CmdletBinding()]
                param($Name, $CommandType)
                if ($CommandType -ne 'Application') { throw 'Only native applications may be resolved.' }
                if ($Name -eq 'pymanager.exe' -and $state.manager) { @{ Source = $managerProbe } }
                elseif ($Name -eq 'uv' -and $state.uv) { @{ Source = 'uv.exe' } }
                elseif ($ErrorActionPreference -eq 'Stop') { throw "Missing $Name" }
            }
            function winget {
                $state.wingetCalls += ,@($args)
                $id = $args[$args.IndexOf('--id') + 1]
                if ($id -eq '9NQ7512CXL7T') { $state.manager = $true }
                elseif ($id -eq 'astral-sh.uv') { $state.uv = $true }
                else { throw "Unexpected package: $id" }
                $global:LASTEXITCODE = if ($case -eq 'store-failure') { 2 } else { 0 }
            }
            function Get-DotfilesProcessPath { $oldPath }
            function Repair-UvWindowsSelection { $state.repairs++ }
            function Assert-DotfilesUv {
                if (-not $state.uv -or $case -eq 'uv-origin-failure') { throw 'Invalid uv ownership.' }
            }
            function Assert-DotfilesUvPython {}
            function Set-DotfilesPythonPath {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseShouldProcessForStateChangingFunctions', '',
                    Justification = 'Record the verified interpreter handoff without changing the user PATH.')]
                param($Python, [switch]$Check)
                if ($Python -ne 'C:\Official Python\python.exe' -or [bool]$Check -ne $state.check) {
                    throw 'Incorrect verified Python PATH handoff.'
                }
                $state.pythonPath = $Python
            }
            function Set-DotfilesPythonPolicy {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseShouldProcessForStateChangingFunctions', '',
                    Justification = 'This test double only records intent; it never changes environment settings.')]
                param([switch]$Check)
                $state.policies += [bool]$Check
            }
            . ([scriptblock]::Create($install.Extent.Text))
            $failure = $null
            try { & $flow } catch { $failure = $_ }
            $expectedFailure = $case -notin @('existing', 'check-existing', 'manager-present', 'fresh')
            if (($null -ne $failure) -ne $expectedFailure) { throw "Unexpected ${case}: $failure" }
            if (-not $expectedFailure -and $state.pythonPath -ne 'C:\Official Python\python.exe') {
                throw "Bootstrap did not publish its verified Python on PATH for $case"
            }
            $packageCount = switch ($case) { 'fresh' { 2 }; 'store-failure' { 1 }; default { 0 } }
            if ($state.wingetCalls.Count -ne $packageCount) { throw "Unexpected package writes for $case" }
            if ($Check -and ($state.managerCalls.Count -or $state.repairs)) { throw "Check mutated $case" }
            if ($failure -and $state.policies.Count) { throw "Failure changed runtime policy for $case" }
            foreach ($call in $state.wingetCalls) {
                foreach ($option in @('--exact', '--source', '--no-upgrade', '--disable-interactivity')) {
                    if ($call -notcontains $option) { throw "Missing $option" }
                }
                $source = $call[$call.IndexOf('--source') + 1]
                $id = $call[$call.IndexOf('--id') + 1]
                if (($id -eq '9NQ7512CXL7T' -and $source -ne 'msstore') -or
                    ($id -eq 'astral-sh.uv' -and $source -ne 'winget')) { throw 'Wrong authoritative catalog.' }
            }
            foreach ($call in $state.managerCalls) {
                if ($call[0] -eq 'install') {
                    $suffix = if ([Runtime.InteropServices.RuntimeInformation]::OSArchitecture -eq 'Arm64') {
                        'arm64'
                    } else { '64' }
                    if ($call -notcontains 'https://www.python.org/ftp/python/index-windows.json' -or
                        $call -notcontains '--config' -or $call -notcontains '-y' -or
                        $call -notcontains "3.13-$suffix" -or
                        $call -contains '--force' -or $call -contains '--update') {
                        throw 'Runtime install lost source, signature policy, or preservation guarantees.'
                    }

                }
            }
        }
        Write-Host "PASS: runtime $case"
    }
    $doctor = [Management.Automation.Language.Parser]::ParseFile(
        (Join-Path $PSScriptRoot 'doctor.ps1'), [ref]$tokens, [ref]$errors)
    if ($errors.Count) { throw ($errors -join "`n") }
    $testTool = $doctor.Find({
        param($node)
        $node -is [Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -eq 'Test-Tool'
    }, $true)
    foreach ($case in @('python-good', 'python-bad', 'uv-bad-policy', 'uv-bad-selection')) {
        & {
            $script:passed = 0
            $script:failed = 0
            $Quiet = $true
            function Get-DotfilesPython {
                if ($case -eq 'python-bad') { throw 'Invalid Python publisher.' }
                @{ version = '3.13.17' }
            }
            function Get-Command {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '',
                    Justification = 'Mock tool existence while exercising actual doctor failure reporting.')]
                param($Name, $ErrorAction)
                if ($ErrorAction -ne 'SilentlyContinue') { throw 'Unexpected discovery error policy.' }
                @{ Name = $Name }
            }
            function Get-ToolVersion { '0.13.9' }
            function Assert-DotfilesUv {}
            function Set-DotfilesPythonPolicy {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseShouldProcessForStateChangingFunctions', '',
                    Justification = 'This test double verifies read-only intent without changing environment settings.')]
                param([switch]$Check)
                if (-not $Check) { throw 'Doctor must not mutate policy.' }
                if ($case -eq 'uv-bad-policy') { throw 'Incorrect runtime policy.' }
            }
            function Assert-DotfilesUvPython {
                if ($case -eq 'uv-bad-selection') { throw 'uv selects a non-PSF runtime.' }
            }
            . ([scriptblock]::Create($testTool.Extent.Text))
            $name = if ($case -like 'python-*') { 'python' } else { 'uv' }
            $passed = Test-Tool @{ Name = $name }
            if ($passed -ne ($case -eq 'python-good') -or
                $script:failed -ne [int]($case -ne 'python-good')) {
                throw "Doctor hid a runtime failure in $case"
            }
        }
        Write-Host "PASS: doctor $case"
    }
    foreach ($case in @('native', 'wrong-publisher', 'uv-failure', 'empty', 'multiple')) {
        & {
            . "$PSScriptRoot\python-windows.ps1"
            $state = @{ case = $case; validated = $false }
            function Get-Command {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '',
                    Justification = 'Run the actual interpreter-selection validator against an offline uv probe.')]
                [OutputType([hashtable])]
                [CmdletBinding()]
                param($Name, $CommandType)
                if ($Name -ne 'uv' -or $CommandType -ne 'Application') { throw 'Unexpected command discovery.' }
                @{ Source = 'uv.exe' }
            }
            function Invoke-DotfilesPythonProbe {
                param($FilePath, $Arguments)
                if ($FilePath -ne 'uv.exe') { throw 'Unexpected probe executable.' }
                $expected = @('python', 'find', '--system', '--no-config', '--no-python-downloads', '--python-preference', 'only-system')
                if (($Arguments -join '|') -ne ($expected -join '|')) { throw 'Unexpected uv discovery arguments.' }
                @{
                    ExitCode = if ($case -eq 'uv-failure') { 2 } else { 0 }
                    StdOut = switch ($case) {
                        'empty' { '' }
                        'multiple' { "C:\Selected Python\python.exe`nC:\Other Python\python.exe" }
                        default { 'C:\Selected Python\python.exe' }
                    }
                    StdErr = if ($case -eq 'uv-failure') { 'Discovery failed.' } else { '' }
                }
            }
            function Get-DotfilesPython {
                param($Python)
                if ($Python -ne 'C:\Selected Python\python.exe') { throw 'Selected interpreter was not validated.' }
                $state.validated = $true
                if ($case -eq 'wrong-publisher') { throw 'Invalid Python publisher.' }
            }
            $failure = $null
            try { Assert-DotfilesUvPython } catch { $failure = $_ }
            if (($null -ne $failure) -ne ($case -ne 'native') -or
                $state.validated -ne ($case -in @('native', 'wrong-publisher'))) {
                throw "Incorrect actual uv interpreter validation for ${case}: $failure"
            }
        }
        Write-Host "PASS: uv interpreter $case"
    }
    & {
        . "$PSScriptRoot\python-windows.ps1"
        $shell = (Get-Process -Id $PID).Path
        $oldAutomatic = $env:PYTHON_MANAGER_AUTOMATIC_INSTALL
        $env:PYTHON_MANAGER_AUTOMATIC_INSTALL = 'test-parent-value'
        try {
            $arguments = @('-NoProfile', '-NonInteractive', '-Command',
                '[Console]::Out.Write($env:PYTHON_MANAGER_AUTOMATIC_INSTALL); [Console]::Error.Write("probe error"); exit 7')
            $result = Invoke-DotfilesPythonProbe -FilePath $shell -Arguments $arguments
            if ($result.ExitCode -ne 7 -or $result.StdOut -ne 'false' -or $result.StdErr -ne 'probe error' -or
                $env:PYTHON_MANAGER_AUTOMATIC_INSTALL -ne 'test-parent-value') {
                throw 'Native probe lost output, exit code, or parent environment isolation.'
            }
            $mode = [Dotfiles.PythonProbeErrorMode]::GetErrorMode()
            $childScript = @'
Add-Type -TypeDefinition 'using System.Runtime.InteropServices; public static class ChildErrorMode { [DllImport("kernel32.dll")] public static extern uint GetErrorMode(); }'
[Console]::Write([ChildErrorMode]::GetErrorMode() -band 1)
'@
            $result = Invoke-DotfilesPythonProbe -FilePath $shell -Arguments @('-NoProfile', '-NonInteractive', '-Command', $childScript)
            if ($result.ExitCode -ne 0 -or $result.StdOut -ne '1' -or
                [Dotfiles.PythonProbeErrorMode]::GetErrorMode() -ne $mode) {
                throw 'Loader error mode was not inherited or parent mode was not restored.'
            }
            $timedOut = $false
            try {
                $null = Invoke-DotfilesPythonProbe -FilePath $shell -Arguments @(
                    '-NoProfile', '-NonInteractive', '-Command', 'Start-Sleep -Seconds 30'
                ) -TimeoutMilliseconds 1000
            } catch {
                if ($_ -notlike '*timed out*') { throw }
                $timedOut = $true
            }
            if (-not $timedOut) { throw 'A hung interpreter probe did not fail within its deadline.' }
            $childRecord = Join-Path $directory 'probe-child.txt'
            $childLauncher = Join-Path $directory 'probe-launcher.ps1'
            @'
param($RecordPath)
$child = Start-Process -FilePath (Get-Process -Id $PID).Path -ArgumentList @(
    '-NoProfile', '-NonInteractive', '-Command', 'Start-Sleep -Seconds 30'
) -WindowStyle Hidden -PassThru
[IO.File]::WriteAllText($RecordPath, [string]$child.Id)
Start-Sleep -Seconds 30
'@ | Set-Content -LiteralPath $childLauncher
            try {
                $null = Invoke-DotfilesPythonProbe -FilePath $shell -Arguments @(
                    '-NoProfile', '-NonInteractive', '-File', $childLauncher, $childRecord
                ) -TimeoutMilliseconds 3000
                throw 'Expected a timeout while waiting on an interpreter child.'
            } catch {
                if ($_ -notlike '*timed out*') { throw }
            }
            $childId = [int](Get-Content -LiteralPath $childRecord -Raw)
            if (Get-Process -Id $childId -ErrorAction SilentlyContinue) {
                throw 'A timed-out probe left its interpreter child running.'
            }
            $failedStart = $false
            try { $null = Invoke-DotfilesPythonProbe -FilePath (Join-Path $directory 'missing.exe') } catch {
                $failedStart = $true
            }
            if (-not $failedStart -or [Dotfiles.PythonProbeErrorMode]::GetErrorMode() -ne $mode) {
                throw 'Failed process creation leaked the loader error mode.'
            }
        } finally {
            $env:PYTHON_MANAGER_AUTOMATIC_INSTALL = $oldAutomatic
        }
        Write-Host 'PASS: native probe output, loader error inheritance, timeout, failure and parent isolation'
    }
} finally {
    $env:PATH = $oldPath
    $global:LASTEXITCODE = $oldExit
    Remove-Item -LiteralPath $directory -Recurse -Force
}
