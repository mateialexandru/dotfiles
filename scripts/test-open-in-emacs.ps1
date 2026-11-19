#Requires -Version 7.3
[CmdletBinding()]
param()
$ErrorActionPreference = 'Stop'
$directory = Join-Path ([IO.Path]::GetTempPath()) "emacs-navigation-$([guid]::NewGuid())"
$previousExitCode = $global:LASTEXITCODE
New-Item -ItemType Directory -Path $directory | Out-Null
try {
    $client = Join-Path $directory 'emacsclient.ps1'
    $file = Join-Path $directory 'source file.txt'
    $runtimeDirectory = Join-Path $directory 'dotfiles\scripts'
    New-Item -ItemType Directory -Path $runtimeDirectory -Force | Out-Null
    'function Get-DotfilesEmacsRuntime { @{ Client = $state.client } }' |
        Set-Content -LiteralPath (Join-Path $runtimeDirectory 'emacs-windows.ps1')
    Copy-Item -LiteralPath "$PSScriptRoot\open-in-emacs.ps1" -Destination $runtimeDirectory
    'Unsaved buffers must not be touched.' | Set-Content -LiteralPath $file
    @'
$state.calls += ,@($args)
$global:LASTEXITCODE = 0
if ($args -contains '--create-frame') {
    if ($state.case -eq 'create-failure') { $global:LASTEXITCODE = 2 }
    else { $state.graphical = $true }
} elseif ($args[-1] -like '*:opened*') {
    if (-not $state.graphical) { throw 'Navigation ran without a graphical frame.' }
    '(:opened "source file.txt" :view "File" :daemon-pid 4242)'
} else {
    if ($state.case -eq 'unavailable') { $global:LASTEXITCODE = 2 }
    if ($state.graphical) { 't' } else { 'nil' }
}
'@ | Set-Content -LiteralPath $client
    foreach ($case in @('existing-frame', 'missing-frame', 'unavailable', 'create-failure', 'installed-runtime')) {
        $state = @{
            case = $case; graphical = $case -in @('existing-frame', 'installed-runtime')
            calls = @(); client = $client
        }
        $failure = $null
        try {
            if ($case -eq 'installed-runtime') {
                & {
                    function Get-Command {
                        [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '',
                            Justification = 'Exercise the accepted-runtime fallback without a client on PATH.')]
                        [CmdletBinding()]
                        param($Name, $CommandType)
                        if ($Name -ne 'emacsclient.exe' -or $CommandType -ne 'Application') {
                            throw 'Unexpected command discovery.'
                        }
                    }
                    & "$runtimeDirectory\open-in-emacs.ps1" -Path $file | Out-Null
                }
            } else {
                & "$PSScriptRoot\open-in-emacs.ps1" -Path $file -EmacsClient $client | Out-Null
            }
        } catch { $failure = $_ }
        if (($null -ne $failure) -ne ($case -in @('unavailable', 'create-failure'))) {
            throw "Unexpected navigation result for ${case}: $failure"
        }
        $creations = @($state.calls | Where-Object { $_ -contains '--create-frame' })
        if ($creations.Count -ne [int]($case -in @('missing-frame', 'create-failure'))) {
            throw "Unexpected frame creation for $case"
        }
        foreach ($call in $state.calls) {
            if ($call -notcontains '--alternate-editor=false') { throw 'Daemon fallback was not disabled.' }
            if ($call -contains '--create-frame' -and $call -notcontains '--no-wait') {
                throw 'Frame creation must not wait for the user to close it.'
            }
            if ($call[-1] -match '\(make-frame\b') { throw 'GUI creation must use the graphical client path.' }
        }
        Write-Host "PASS: Emacs navigation $case"
    }
} finally {
    $global:LASTEXITCODE = $previousExitCode
    Remove-Item -LiteralPath $directory -Recurse -Force
}
