#Requires -Version 7.3
$ErrorActionPreference = 'Stop'
$temporary = Join-Path ([IO.Path]::GetTempPath()) "session-launch-$([guid]::NewGuid())"
$previousClaude = $env:CLAUDE_CODE_ENTRYPOINT
try {
    $directory = Join-Path $temporary "project's work & space"
    New-Item -ItemType Directory -Path $directory -Force | Out-Null
    foreach ($case in @('copilot', 'claude', 'explicit', 'missing-command', 'whatif')) {
        & {
            $env:CLAUDE_CODE_ENTRYPOINT = if ($case -eq 'claude') { 'test' } else { $null }
            $capture = @{ Launch = $null }
            function Get-Command {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '',
                    Justification = 'Offline launcher test; never starts an actual coding session.')]
                param($Name, $CommandType)
                if (-not $CommandType) { throw 'Executable discovery must restrict command types.' }
                if ($Name -eq 'missing') { throw 'Executable missing.' }
                [pscustomobject]@{ Source = "C:\Tools\$Name.exe" }
            }
            function Start-Process {
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '',
                    Justification = 'Capture the launch boundary without opening a terminal.')]
                [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseShouldProcessForStateChangingFunctions', '',
                    Justification = 'Mock captures calls without starting a process.')]
                param($FilePath, $WorkingDirectory, $ArgumentList, [switch]$PassThru)
                if (-not $PassThru) { throw 'Launcher must retain the created process identity.' }
                $capture.Launch = @{ File = $FilePath; Directory = $WorkingDirectory; Arguments = $ArgumentList }
                [pscustomobject]@{ Id = 4242 }
            }
            $parameters = @{ Path = $directory; Title = "a'; throw 'title injection" }
            if ($case -eq 'explicit') {
                $parameters.Command = 'custom-agent'
                $parameters.CommandArguments = @("a'b", 'x; throw "argument injection"', 'one argument')
            }
            if ($case -eq 'missing-command') { $parameters.Command = 'missing' }
            $failed = $false
            try { & "$PSScriptRoot\launch-session.ps1" @parameters -WhatIf:($case -eq 'whatif') | Out-Null }
            catch { $failed = $true }
            $launch = $capture.Launch
            if ($failed -ne ($case -eq 'missing-command')) { throw "Unexpected launcher failure: $case" }
            if ($case -in @('missing-command', 'whatif')) {
                if ($launch) { throw 'A rejected or preview-only launch started a process.' }
            } else {
                if ($launch.Directory -ne $directory -or $launch.File -ne 'C:\Tools\pwsh.exe') {
                    throw "Working directory or shell was not preserved: $($launch | ConvertTo-Json -Compress); expected $directory"
                }
                $arguments = $launch.Arguments
                $expression = [Text.Encoding]::Unicode.GetString([Convert]::FromBase64String($arguments[-1]))
                $tokens = $null
                $errors = $null
                $ast = [Management.Automation.Language.Parser]::ParseInput($expression, [ref]$tokens, [ref]$errors)
                if ($errors.Count -or $ast.EndBlock.Statements.Count -ne 2) { throw 'Launch values escaped their literals.' }
                $command = $ast.EndBlock.Statements[1].PipelineElements[0]
                $expectedCommand = if ($case -eq 'explicit') { 'custom-agent' } else { $case }
                if ($command.CommandElements[0].Value -ne "C:\Tools\$expectedCommand.exe") {
                    throw 'Wrong coding executable selected.'
                }
                if ($case -eq 'explicit') {
                    $actual = @($command.CommandElements | Select-Object -Skip 1 | ForEach-Object Value)
                    if (($actual | ConvertTo-Json -Compress) -ne ($parameters.CommandArguments | ConvertTo-Json -Compress)) {
                        throw 'Explicit command arguments did not round-trip.'
                    }
                } elseif ($command.CommandElements.Count -ne 1) { throw 'Default launches must not add hidden arguments.' }
            }
            Write-Host "PASS: public session launcher $case"
        }
    }
    $failed = $false
    try { & "$PSScriptRoot\launch-session.ps1" -Path "$temporary\missing" -WhatIf } catch { $failed = $true }
    if (-not $failed) { throw 'A missing working directory was accepted.' }
} finally {
    $env:CLAUDE_CODE_ENTRYPOINT = $previousClaude
    Remove-Item -LiteralPath $temporary -Recurse -Force
}
