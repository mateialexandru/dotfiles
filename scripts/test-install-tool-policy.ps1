# Offline policy tests: all package-manager calls and tool discovery are mocked.
$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot
$originalPath = $env:PATH
$originalExit = $global:LASTEXITCODE
$originalProgramFiles = $env:ProgramFiles
if (-not $env:ProgramFiles) { $env:ProgramFiles = [IO.Path]::GetTempPath() }

function Assert-True($condition, $message) {
    if (-not $condition) { throw $message }
}

try {
    foreach ($case in @(
        'python-existing', 'python-missing', 'python-broken', 'python-download-fails',
        'python-pinned-install', 'gnuplot-existing', 'gnuplot-no-elevation',
        'gnuplot-cancelled', 'gnuplot-pinned-install'
    )) {
        $global:DotfilesPolicyCase = $case
        $global:DotfilesPolicyCalls = @()
        $global:DotfilesPolicyInstalled = $case -in @('python-existing', 'python-broken', 'gnuplot-existing')
        $expectedFailure = $case -in @(
            'python-missing', 'python-broken', 'python-download-fails',
            'gnuplot-no-elevation', 'gnuplot-cancelled'
        )
        & {
            function pyright {
                $global:LASTEXITCODE = if ($global:DotfilesPolicyCase -eq 'python-broken') { 1 } else { 0 }
                'pyright 1.1.414'
            }
            function ruff { $global:LASTEXITCODE = 0; 'ruff 0.16.7' }
            function gnuplot { $global:LASTEXITCODE = 0; 'gnuplot 6.0 patchlevel 4' }
            function Get-Command($Name, $ErrorAction) {
                if ($global:DotfilesPolicyInstalled) {
                    Microsoft.PowerShell.Core\Get-Command $Name -CommandType Function
                }
            }
            function Test-Path { $false } # Never write the real user PATH.
            function uv {
                $global:DotfilesPolicyCalls += ,@($args)
                $global:LASTEXITCODE = if ($global:DotfilesPolicyCase -eq 'python-download-fails') { 1 } else { 0 }
            }
            function winget {
                $global:DotfilesPolicyCalls += ,@($args)
                $global:LASTEXITCODE = if ($global:DotfilesPolicyCase -eq 'gnuplot-cancelled') { 2 } else { 0 }
            }

            $failure = $null
            try {
                if ($case -like 'python-*') {
                    if ($case -in @('python-download-fails', 'python-pinned-install')) {
                        & (Join-Path $root 'install-python-tools.ps1') -AllowUvDownloads `
                            -PyrightVersion '1.1.414' -RuffVersion '0.16.7'
                    } else {
                        & (Join-Path $root 'install-python-tools.ps1')
                    }
                } else {
                    & (Join-Path $root 'install-gnuplot.ps1') -Version '6.0 patchlevel 4' `
                        -AllowElevation:($case -in @('gnuplot-cancelled', 'gnuplot-pinned-install'))
                }
            } catch {
                $failure = $_
            }
            Assert-True (($null -ne $failure) -eq $expectedFailure) `
                "Unexpected result for $case : $failure"
            $expectedCalls = switch ($case) {
                'python-download-fails' { 1 }
                'python-pinned-install' { 2 }
                'gnuplot-cancelled' { 1 }
                'gnuplot-pinned-install' { 1 }
                default { 0 }
            }
            Assert-True ($global:DotfilesPolicyCalls.Count -eq $expectedCalls) "Unexpected package-manager calls: $case"
            foreach ($call in $global:DotfilesPolicyCalls) {
                if ($case -like 'python-*') {
                    Assert-True ($call[2] -match '^(pyright==1\.1\.414|ruff==0\.16\.7)$') 'uv version was not pinned'
                    Assert-True ($call -notcontains '--force') 'uv must not force reinstall'
                } else {
                    foreach ($arg in @('--no-upgrade', '--scope', 'machine', '--source', 'winget',
                                       '--version', '6.0 patchlevel 4')) {
                        Assert-True ($call -contains $arg) "Missing WinGet safeguard: $arg"
                    }
                }
            }
        }
        Write-Host "PASS: $case"
    }

    foreach ($scriptPath in @(
        [IO.Path]::Combine($root, '..', 'install.ps1'),
        [IO.Path]::Combine($root, 'install-prerequisites.ps1'),
        [IO.Path]::Combine($root, 'install-python-tools.ps1'),
        [IO.Path]::Combine($root, 'install-gnuplot.ps1'),
        [IO.Path]::Combine($root, 'doctor.ps1'),
        [IO.Path]::Combine($root, 'setup-doom.ps1'),
        [IO.Path]::Combine($root, 'emacs-paths.ps1'),
        [IO.Path]::Combine($root, 'invoke-doom.ps1')
    )) {
        $tokens = $null
        $errors = $null
        $ast = [System.Management.Automation.Language.Parser]::ParseFile(
            $scriptPath, [ref]$tokens, [ref]$errors)
        Assert-True ($errors.Count -eq 0) "Parse errors in $scriptPath : $errors"
        if ($scriptPath -like '*install-prerequisites.ps1' -or
            $scriptPath -like '*doctor.ps1') {
            Assert-True ($ast.Extent.Text -notmatch 'bash-language-server') `
                "Windows provisioning must not require or reinstall Bash language server: $scriptPath"
        }
        if ($scriptPath -like '*doctor.ps1') {
            Assert-True ($ast.Extent.Text -notmatch 'uv\s+tool\s+install\s+--force\s+(pyright|ruff)') `
                'Doctor must not force unpinned Pyright or Ruff downloads'
            Assert-True ($ast.Extent.Text -match 'install-python-tools\.ps1') `
                'Doctor must direct Python tool remediation through the policy helper'
            $definition = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and
                $node.Name -eq 'Test-DefenderExclusions'
            }, $true)
            Assert-True ($null -ne $definition) 'Defender advisory function not found'
            foreach ($case in @('covered', 'missing', 'unavailable')) {
                & {
                    . ([scriptblock]::Create($definition.Extent.Text))
                    function Get-MpPreference {
                        if ($case -eq 'unavailable') { throw 'Policy prevents querying exclusions' }
                        @{ ExclusionPath = if ($case -eq 'covered') { @('C:\') } else { @() } }
                    }
                    function Test-Path { $true }
                    function Get-DotfilesEmacsPaths { 'C:\Test\Emacs' }
                    $script:passed = 0
                    $script:failed = 0
                    $script:advisories = 0
                    Test-DefenderExclusions
                    Assert-True ($script:passed -eq 0 -and $script:failed -eq 0) `
                        'Exclusion status must not count as a health pass or failure'
                    $expectedAdvisories = if ($case -eq 'covered') { 0 } else { 1 }
                    Assert-True ($script:advisories -eq $expectedAdvisories) 'Missing Defender advisory'
                }
            }
        }
        if ($scriptPath -like '*setup-doom.ps1') {
            $setup = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.AssignmentStatementAst] -and
                $node.Left.Extent.Text -eq '$scriptBlock'
            }, $true)
            $setupText = $setup.Right.Expression.Value
            $setupErrors = $null
            $setupAst = [System.Management.Automation.Language.Parser]::ParseInput(
                $setupText, [ref]$tokens, [ref]$setupErrors)
            Assert-True ($setupErrors.Count -eq 0) 'Embedded Doom setup must parse'
            Assert-True ($setupText -notmatch 'Add-Content') 'Setup must not append to the public config'
            $invoke = $setupAst.Find({
                param($node)
                $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and
                $node.Name -eq 'Invoke-DoomSetupCommand'
            }, $true)
            foreach ($exitCode in @(0, 1, 254)) {
                & {
                    . ([scriptblock]::Create($invoke.Extent.Text))
                    function Test-Emacs { $global:LASTEXITCODE = $exitCode }
                    $emacsExe = 'Test-Emacs'
                    $doomSetupInvoker = Join-Path $root 'invoke-doom.ps1'
                    $doomDir = $root
                    $doomBin = $root
                    $previousStep = $env:__DOOMSTEP
                    $failed = $false
                    try { Invoke-DoomSetupCommand 'sync' } catch { $failed = $true }
                    Assert-True ($failed -eq ($exitCode -ne 0)) "Lost Doom exit code $exitCode"
                    Assert-True ([string]$env:__DOOMSTEP -eq [string]$previousStep) 'Doom environment was not restored'
                }
            }
        }
        if ($scriptPath -like '*install-prerequisites.ps1') {
            # Exercise the actual loop without executing the rest of the installer.
            $loop = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.ForEachStatementAst] -and
                $node.Variable.VariablePath.UserPath -eq 'pkg'
            }, $true)
            Assert-True ($null -ne $loop) 'WinGet loop not found'
            foreach ($exitCode in @(0, -1978335189, -1978335135, 2, 1)) {
                & {
                    $wingetPackages = @('example.package')
                    function winget { $global:LASTEXITCODE = $exitCode }
                    $failed = $false
                    try { & ([scriptblock]::Create($loop.Extent.Text)) } catch { $failed = $true }
                    Assert-True ($failed -eq ($exitCode -in @(1, 2))) "Incorrect WinGet handling for $exitCode"
                }
            }
        }
    }
    Write-Host 'PASS: script parsing and WinGet exit handling'
} finally {
    $env:PATH = $originalPath
    if ($null -eq $originalProgramFiles) {
        Remove-Item Env:ProgramFiles -ErrorAction SilentlyContinue
    } else {
        $env:ProgramFiles = $originalProgramFiles
    }
    $global:LASTEXITCODE = $originalExit
    Remove-Variable DotfilesPolicyCase, DotfilesPolicyCalls, DotfilesPolicyInstalled -Scope Global
}
