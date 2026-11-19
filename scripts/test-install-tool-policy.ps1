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
        'python-pinned-install', 'python-ruff-uv-only', 'gnuplot-existing', 'gnuplot-no-elevation',
        'gnuplot-cancelled', 'gnuplot-pinned-install'
    )) {
        $global:DotfilesPolicyCase = $case
        $global:DotfilesPolicyCalls = @()
        $global:DotfilesPolicyInstalled = $case -in @('python-existing', 'python-broken', 'gnuplot-existing')
        $expectedFailure = $case -in @(
            'python-missing', 'python-broken', 'python-download-fails',
            'python-ruff-uv-only', 'gnuplot-no-elevation', 'gnuplot-cancelled'
        )
        & {
            function pyright {
                $global:LASTEXITCODE = if ($global:DotfilesPolicyCase -eq 'python-broken') { 1 } else { 0 }
                'pyright 1.1.414'
            }
            function ruff { $global:LASTEXITCODE = 0; 'ruff 0.16.7' }
            function gnuplot { $global:LASTEXITCODE = 0; 'gnuplot 6.0 patchlevel 4' }
            function Test-UvExecutable { $global:LASTEXITCODE = 0; 'uv 0.13.8' }
            function Get-Command($Name, $ErrorAction, $CommandType) {
                if ($Name -in @('uv', 'uvx')) {
                    return [pscustomobject]@{ Source = 'Test-UvExecutable' }
                }
                if ($global:DotfilesPolicyInstalled -or
                    ($Name -eq 'ruff' -and $global:DotfilesPolicyCase -in @('python-download-fails', 'python-pinned-install')) -or
                    ($Name -eq 'pyright' -and $global:DotfilesPolicyCase -eq 'python-ruff-uv-only')) {
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
                    } elseif ($case -eq 'python-ruff-uv-only') {
                        & (Join-Path $root 'install-python-tools.ps1') -AllowUvDownloads -RuffVersion '0.17.3'
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
                'python-pinned-install' { 1 }
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
        [IO.Path]::Combine($root, 'install-ruff-windows.ps1'),
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
        if ($scriptPath -like '*invoke-doom.ps1') {
            $pipeAssignment = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.AssignmentStatementAst] -and
                $node.Left.Extent.Text -eq '$env:__DOOMPIPE'
            }, $true)
            Assert-True ($null -ne $pipeAssignment) 'Doom pipe detection not found'
            foreach ($case in @(
                @{ InputRedirected = $false; OutputRedirected = $false; Expected = '' },
                @{ InputRedirected = $false; OutputRedirected = $true; Expected = '1' },
                @{ InputRedirected = $true; OutputRedirected = $false; Expected = '0' },
                @{ InputRedirected = $true; OutputRedirected = $true; Expected = '01' }
            )) {
                # Inject console facts, retaining the launcher's pipe-detection logic.
                $expression = $pipeAssignment.Right.Extent.Text.
                    Replace('[Console]::IsInputRedirected', '$case.InputRedirected').
                    Replace('[Console]::IsOutputRedirected', '$case.OutputRedirected')
                $pipes = & ([scriptblock]::Create($expression))
                Assert-True ([string]$pipes -ceq $case.Expected) `
                    "Incorrect Doom pipes for input=$($case.InputRedirected), output=$($case.OutputRedirected): '$pipes'"
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
            foreach ($exitCode in @(0, 1, 254)) {
                & {
                    function Test-Emacs {
                        $pipes = [string]$env:__DOOMPIPE
                        Assert-True ($pipes.Contains('0') -eq [Console]::IsInputRedirected) `
                            'Doom stdin redirection does not match the console'
                        Assert-True ($pipes.Contains('1') -eq [Console]::IsOutputRedirected) `
                            'Doom stdout redirection does not match the console'
                        $global:LASTEXITCODE = $exitCode
                    }
                    $emacsExe = 'Test-Emacs'
                    $doomSetupInvoker = Join-Path $root 'invoke-doom.ps1'
                    $doomDir = $root
                    $doomBin = $root
                    $previousStep = $env:__DOOMSTEP
                    $previousPipe = $env:__DOOMPIPE
                    $failed = $false
                    try { & $doomSetupInvoker -Command sync -DoomDirectory $doomDir -EmacsExecutable $emacsExe } catch { $failed = $true }
                    Assert-True ($failed -eq ($exitCode -ne 0)) "Lost Doom exit code $exitCode"
                    Assert-True ([string]$env:__DOOMSTEP -eq [string]$previousStep) 'Doom environment was not restored'
                    Assert-True ([string]$env:__DOOMPIPE -ceq [string]$previousPipe) 'Doom pipe state was not restored'
                }
            }
            foreach ($continuationExit in @(0, 5)) {
                & {
                    $temporary = Join-Path ([IO.Path]::GetTempPath()) ("dotfiles-doom-test-" + [guid]::NewGuid().ToString('N'))
                    New-Item -ItemType Directory -Path $temporary | Out-Null
                    $previousTemp = $env:TEMP
                    $previousStep = $env:__DOOMSTEP
                    $previousPath = $env:PATH
                    $global:DotfilesDoomCalls = 0
                    $global:DotfilesDoomExit = $continuationExit
                    function Test-Emacs {
                        $global:DotfilesDoomCalls++
                        if ($global:DotfilesDoomCalls -eq 1) {
                            $continuation = Join-Path $env:TEMP "doom.$($env:__DOOMPID).$($env:__DOOMSTEP).ps1"
                            Set-Content -LiteralPath $continuation -Value '$env:__DOOMSTEP = "1"; doom sync'
                            $global:LASTEXITCODE = 254
                        } else {
                            $global:LASTEXITCODE = $global:DotfilesDoomExit
                        }
                    }
                    try {
                        $env:TEMP = $temporary
                        $emacsExe = 'Test-Emacs'
                        $doomSetupInvoker = Join-Path $root 'invoke-doom.ps1'
                        $doomDir = $root
                        $failed = $false
                        try { & $doomSetupInvoker -Command sync -DoomDirectory $doomDir -EmacsExecutable $emacsExe } catch {
                            $failed = $true
                            Assert-True ($_.ToString() -match "exit $continuationExit") 'Continuation failure lost its exit code'
                        }
                        Assert-True ($global:DotfilesDoomCalls -eq 2) 'Doom continuation did not resume Emacs'
                        Assert-True ($failed -eq ($continuationExit -ne 0)) 'Incorrect Doom continuation result'
                        Assert-True ([string]$env:__DOOMSTEP -eq [string]$previousStep) 'Continuation context was not restored'
                        Assert-True ($env:PATH -eq $previousPath) 'Continuation PATH was not restored'
                    } finally {
                        $env:TEMP = $previousTemp
                        Remove-Item -LiteralPath $temporary -Recurse -Force
                        Remove-Variable DotfilesDoomCalls, DotfilesDoomExit -Scope Global
                    }
                }
            }
        }
        if ($scriptPath -like '*install-prerequisites.ps1') {
            $notifications = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.IfStatementAst] -and
                $node.Extent.Text.StartsWith('if (-not (Get-Module -ListAvailable BurntToast))')
            }, $true)
            Assert-True ($null -ne $notifications) 'BurntToast provisioning block not found'
            foreach ($case in @('existing', 'nuget', 'legacy', 'no-feed', 'untrusted', 'blocked', 'not-visible')) {
                & {
                    $script:burntToastInstalled = $case -eq 'existing'
                    $script:burntToastInstalls = 0
                    $script:burntToastQueries = 0
                    function Get-Module {
                        if ($script:burntToastInstalled) { [pscustomobject]@{ Name = 'BurntToast'; Version = '1.1.0' } }
                    }
                    function Get-Command {
                        if ($case -ne 'legacy') { [pscustomobject]@{ Name = 'Install-PSResource' } }
                    }
                    function Get-PSResourceRepository {
                        $script:burntToastQueries++
                        [pscustomobject]@{ Name = 'Containers'; ApiVersion = 'ContainerRegistry'; Priority = 0; Trusted = $true }
                        if ($case -ne 'no-feed') {
                            [pscustomobject]@{ Name = 'ApprovedModules'; ApiVersion = 'V3'; Priority = 2; Trusted = ($case -ne 'untrusted') }
                            [pscustomobject]@{ Name = 'PriorityModules'; ApiVersion = 'V2'; Priority = 1; Trusted = ($case -eq 'nuget') }
                        }
                    }
                    function Get-PSRepository {
                        $script:burntToastQueries++
                        [pscustomobject]@{ Name = 'ApprovedModules'; InstallationPolicy = 'Trusted' }
                        [pscustomobject]@{ Name = 'Untrusted'; InstallationPolicy = 'Untrusted' }
                    }
                    function Install-PSResource($Name, $Repository, $Scope, $ErrorAction, [switch]$TrustRepository) {
                        Assert-True ($Name -eq 'BurntToast' -and $Scope -eq 'CurrentUser') 'Unexpected module install'
                        $expectedRepositories = if ($case -eq 'nuget') { 'PriorityModules,ApprovedModules' } else { 'ApprovedModules' }
                        Assert-True (($Repository -join ',') -eq $expectedRepositories) 'Only trusted NuGet module feeds may be selected, in priority order'
                        Assert-True (-not $TrustRepository) 'Installer must not override repository trust'
                        Assert-True ($ErrorAction -eq 'Stop') 'Module download failures must be terminating'
                        $script:burntToastInstalls++
                        if ($case -eq 'blocked') { throw 'Simulated feed failure' }
                        $script:burntToastInstalled = $case -ne 'not-visible'
                    }
                    function Install-Module($Name, $Repository, $Scope, $ErrorAction, [switch]$Force) {
                        Assert-True (-not $Force) 'Legacy module install must not force a reinstall'
                        Install-PSResource -Name $Name -Repository $Repository -Scope $Scope -ErrorAction $ErrorAction
                    }
                    $failure = $null
                    try { . ([scriptblock]::Create($notifications.Extent.Text)) } catch { $failure = $_ }
                    Assert-True (($null -ne $failure) -eq ($case -in @('no-feed', 'untrusted', 'blocked', 'not-visible'))) `
                        "Unexpected BurntToast result for $case : $failure"
                    $expectedInstalls = if ($case -in @('existing', 'no-feed', 'untrusted')) { 0 } else { 1 }
                    Assert-True ($script:burntToastInstalls -eq $expectedInstalls) "Unexpected module downloads for $case"
                    if ($case -eq 'existing') {
                        Assert-True ($script:burntToastQueries -eq 0) 'Existing BurntToast must not query any feed'
                    }
                    if ($case -eq 'blocked') {
                        Assert-True ("$failure" -match 'Simulated feed failure') 'Feed errors must not be masked or retried'
                    }
                }
                Write-Host "PASS: BurntToast $case"
            }
            # Exercise the actual loop without executing the rest of the installer.
            $loop = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.ForEachStatementAst] -and
                $node.Variable.VariablePath.UserPath -eq 'pkg'
            }, $true)
            Assert-True ($null -ne $loop) 'WinGet loop not found'
            $packages = $ast.Find({
                param($node)
                $node -is [System.Management.Automation.Language.AssignmentStatementAst] -and
                $node.Left.Extent.Text -eq '$wingetPackages'
            }, $true)
            Assert-True ($null -ne $packages) 'WinGet package list not found'
            & {
                . ([scriptblock]::Create($packages.Extent.Text))
                $calls = [System.Collections.Generic.List[string]]::new()
                function winget {
                    if ($args -match '^Python\.') {
                        throw 'Python provisioning belongs to the dedicated official-runtime bootstrap'
                    }
                    $calls.Add($args[$args.IndexOf('--id') + 1])
                    $global:LASTEXITCODE = 0
                }
                & ([scriptblock]::Create($loop.Extent.Text))
                Assert-True ($calls -notcontains 'astral-sh.uv') 'uv belongs to the dedicated official-runtime bootstrap'
            }
            Assert-True ($ast.Extent.Text -match 'install-python-runtime\.ps1"\s+-AllowDownloads') `
                'Prerequisites must call the official Python/uv bootstrap'
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
