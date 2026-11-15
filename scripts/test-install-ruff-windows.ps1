# Offline tests of WinGet delegation; no downloads or persistent changes.
$ErrorActionPreference = 'Stop'
$tokens = $null
$errors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile(
    (Join-Path $PSScriptRoot 'install-ruff-windows.ps1'), [ref]$tokens, [ref]$errors)
if ($errors.Count) { throw "Ruff installer parse errors: $errors" }
$definition = $ast.Find({
    param($node)
    $node -is [System.Management.Automation.Language.FunctionDefinitionAst] -and
    $node.Name -eq 'Install-Ruff'
}, $true)
. ([scriptblock]::Create($definition.Extent.Text))

foreach ($case in @('reuse', 'broken', 'recover-path', 'no-consent', 'invalid-version',
                     'install', 'cancelled', 'version-mismatch', 'missing-executable')) {
    & {
        $script:installed = $case -in @('reuse', 'broken')
        $script:installs = 0
        function ruff {
            $global:LASTEXITCODE = if ($case -eq 'broken') { 7 } else { 0 }
            if ($case -eq 'version-mismatch') { 'ruff 0.1.0' } else { 'ruff 0.17.2' }
        }
        function Get-Command($Name, $ErrorAction) {
            if ($Name -eq 'ruff' -and $script:installed) {
                Microsoft.PowerShell.Core\Get-Command ruff -CommandType Function
            }
        }
        function Test-Path { $script:installed -or $case -eq 'recover-path' }
        function Add-EditorToolPath { $script:installed = $true }
        function Get-DotfilesProcessPath { $env:PATH }
        function winget {
            $script:installs++
            foreach ($argument in @('install', '--id', 'astral-sh.ruff', '--version',
                                     '0.17.2', '--source', 'winget', '--no-upgrade')) {
                if ($argument -notin $args) { throw "Missing WinGet safeguard: $argument" }
            }
            $global:LASTEXITCODE = if ($case -eq 'cancelled') { 2 } else { 0 }
            $script:installed = $case -notin @('cancelled', 'missing-executable')
        }
        $version = switch ($case) {
            'invalid-version' { 'latest' }
            default { '0.17.2' }
        }
        $options = @{ Version = $version; AllowDownloads = ($case -ne 'no-consent') }
        $failed = $false
        try { Install-Ruff @options } catch { $failed = $true }
        $expectedFailure = $case -in @('broken', 'no-consent', 'invalid-version',
                                       'cancelled', 'version-mismatch',
                                       'missing-executable')
        if ($failed -ne $expectedFailure) { throw "Unexpected result: $case" }
        $installAttempted = $case -in @('install', 'cancelled', 'version-mismatch', 'missing-executable')
        if ($script:installs -ne [int]$installAttempted) { throw "Unexpected install/retry: $case" }
        if ($case -eq 'install') {
            Install-Ruff -Version '0.17.2'
            if ($script:installs -ne 1) { throw 'Second run installed instead of reusing Ruff' }
        }
    }
    Write-Host "PASS: Ruff $case"
}
