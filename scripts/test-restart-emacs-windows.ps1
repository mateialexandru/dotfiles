#Requires -Version 7.3
[Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSAvoidOverwritingBuiltInCmdlets', '', Justification = 'Offline tests replace process and notification boundaries.')]
[Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseShouldProcessForStateChangingFunctions', '', Justification = 'Test mocks only record calls; they cannot change system state.')]
param()

$ErrorActionPreference = 'Stop'

function Assert-True($Condition, [string]$Message) {
    if (-not $Condition) { throw $Message }
}

$fixture = Join-Path ([IO.Path]::GetTempPath()) "doom restart $([guid]::NewGuid())"
$previousProfile = $env:USERPROFILE
try {
    New-Item -ItemType Directory -Path "$fixture\.config\emacs" -Force | Out-Null
    foreach ($file in @('client.exe', 'gui-client.exe', '.config\emacs\early-init.el')) {
        New-Item -ItemType File -Path (Join-Path $fixture $file) | Out-Null
    }
    $env:USERPROFILE = $fixture
    & {
        . "$PSScriptRoot\restart-emacs-windows.ps1"
        $state = @{}
        function Get-DotfilesEmacsRuntime {
            [pscustomobject]@{ Client = "$fixture\client.exe"; GuiClient = "$fixture\gui-client.exe" }
        }
        function Get-Command {
            param($Name, $ErrorAction)
            Assert-True ($Name -eq 'New-BurntToastNotification' -and $ErrorAction -eq 'SilentlyContinue') 'Unexpected command discovery'
            if ($state.Case -ne 'missing-notifier') { return 'notification command' }
        }
        function New-BurntToastNotification {
            param($Text, $ErrorAction)
            $state.Events.Add('notify')
            Assert-True ($Text[0] -eq 'Doom Emacs' -and $ErrorAction -eq 'Stop') 'Notification failed to request explicit errors'
            $state.Notice = $Text[1]
            if ($state.Case -eq 'notification-failure') { throw 'notification failure' }
        }
        function Start-Sleep {
            param($Seconds, $Milliseconds)
            if ($Milliseconds) {
                $state.Events.Add('gui-wait')
            } else {
                $state.Events.Add('sleep')
                $state.Delay = $Seconds
                if ($state.Case -eq 'countdown-cancelled') { throw 'countdown cancelled' }
            }
        }
        function Invoke-DotfilesDoomClient {
            param($ClientPath, $ServerFile, $Expression = '(emacs-pid)', $TimeoutMilliseconds = 5000)
            Assert-True ($ClientPath -eq "$fixture\client.exe" -and
                $ServerFile -eq "$fixture\.config\emacs\server\server") 'Client used the wrong runtime/server'
            Assert-True ($TimeoutMilliseconds -gt 0) 'Unbounded client call'
            if ($Expression -in @('(emacs-pid)', '(if (daemonp) (emacs-pid) nil)')) {
                if ($state.Started) {
                    $state.Events.Add('new-pid')
                    if ($state.Case -eq 'same-pid') { return '12345' }
                    return '23456'
                }
                $state.Events.Add('probe')
                Assert-True ($Expression -match 'daemonp') 'Restart did not verify daemon mode'
                if ($state.Case -eq 'unreachable') { throw 'server unreachable' }
                if ($state.Case -eq 'not-daemon') { return 'nil' }
                if ($state.Case -eq 'invalid-pid') { return 'not a PID' }
                return '12345'
            }
            if ($Expression -match 'save-some-buffers') {
                $state.Events.Add('save')
                Assert-True ($Expression.Contains('(unless (= (emacs-pid) 12345)') -and
                    $Expression -notmatch 'kill-emacs|run-at-time') 'Save omitted identity guard or queued shutdown'
                if ($state.Case -eq 'save-failure') { throw 'save failed' }
                if ($state.Case -eq 'save-unconfirmed') { return 'nil' }
                return 't'
            }
            if ($Expression -match 'kill-emacs') {
                $state.Events.Add('quit')
                Assert-True ($Expression.Contains('(unless (= (emacs-pid) 12345)') -and
                    $Expression.Contains('(confirm-kill-processes nil)')) 'Shutdown omitted identity or prompt guard'
                if ($state.Case -eq 'quit-failure') { throw 'quit failed' }
                return 't'
            }
            $state.Events.Add('gui')
            Assert-True ($Expression -match 'display-graphic-p' -and $Expression -match 'frame-visible-p') 'GUI was not verified'
            if ($state.Case -eq 'gui-delay' -and -not $state.GuiRetried) {
                $state.GuiRetried = $true
                return 'nil'
            }
            return 't'
        }
        function Get-Process {
            param($Id, $ErrorAction)
            Assert-True ($Id -eq 12345 -and $ErrorAction -eq 'Stop') 'Looked up an unrelated process'
            $process = [pscustomobject]@{
                Id = 12345
                ProcessName = $(if ($state.Case -eq 'wrong-process') { 'unrelated' } else { 'emacs' })
                SessionId = [Diagnostics.Process]::GetCurrentProcess().SessionId +
                    $(if ($state.Case -eq 'wrong-session') { 1 } else { 0 })
                HasExited = $false
                Handle = 1
            }
            $process | Add-Member ScriptMethod WaitForExit {
                param($Milliseconds)
                $state.Events.Add('wait')
                Assert-True ($Milliseconds -ge 0) 'Unbounded shutdown wait'
                return $state.Stopped -or $state.Case -notin @('force', 'quit-failure', 'stop-failure', 'stop-timeout')
            }
            $process | Add-Member ScriptMethod Dispose { $state.Events.Add('dispose') }
            return $process
        }
        function Stop-Process {
            param($Id, $ErrorAction)
            $state.Events.Add('force')
            Assert-True ($Id -eq 12345 -and $ErrorAction -eq 'Stop') 'Killed an unrelated PID or ignored stop errors'
            if ($state.Case -eq 'stop-failure') { throw 'stop failed' }
            $state.Stopped = $state.Case -ne 'stop-timeout'
        }
        function Open-DotfilesDoom {
            $state.Events.Add('open')
            if ($state.Case -eq 'startup-failure') { throw 'startup failed' }
            $state.Started = $true
        }
        foreach ($case in @('default', 'zero', 'custom', 'force', 'quit-failure', 'unreachable', 'not-daemon',
                'invalid-pid', 'wrong-process', 'wrong-session', 'missing-notifier', 'notification-failure',
                'countdown-cancelled', 'save-failure', 'save-unconfirmed', 'stop-failure', 'stop-timeout',
                'startup-failure', 'same-pid', 'missing-gui', 'gui-delay', 'what-if')) {
            $state.Clear()
            $state.Case = $case
            $state.Events = [Collections.Generic.List[string]]::new()
            $state.Started = $false
            $state.Stopped = $false
            $failure = $null
            try {
                switch ($case) {
                    'zero' { Restart-DotfilesDoom -Seconds 0 }
                    'custom' { Restart-DotfilesDoom -Seconds 30 }
                    'missing-gui' { Restart-DotfilesDoom -GuiTimeoutSeconds 0 }
                    'what-if' { Restart-DotfilesDoom -WhatIf }
                    default { Restart-DotfilesDoom }
                }
            } catch { $failure = $_ }
            $success = $case -in @('default', 'zero', 'custom', 'force', 'quit-failure', 'gui-delay', 'what-if')
            Assert-True (($null -eq $failure) -eq $success) "Unexpected result for ${case}: $failure"
            $events = $state.Events -join ','
            if ($case -eq 'what-if') {
                Assert-True ($events -eq '') 'WhatIf touched the daemon'
            } elseif ($success) {
                $delay = if ($case -eq 'zero') { 0 } elseif ($case -eq 'custom') { 30 } else { 10 }
                Assert-True ($state.Notice.Contains("in ${delay}s")) 'Notification lost the countdown'
                Assert-True (($delay -eq 0 -and 'sleep' -notin $state.Events) -or $state.Delay -eq $delay) 'Countdown mismatch'
                $expected = 'probe,notify,' + $(if ($delay) { 'sleep,' }) + 'save,quit,wait,' +
                    $(if ($case -in @('force', 'quit-failure')) { 'force,wait,' }) + 'open,new-pid,gui,' +
                    $(if ($case -eq 'gui-delay') { 'gui-wait,gui,' }) + 'dispose'
                Assert-True ($events -eq $expected) "Unexpected restart order for ${case}: $events"
            } elseif ($case -in @('startup-failure', 'same-pid', 'missing-gui')) {
                Assert-True (@($state.Events | Where-Object { $_ -eq 'open' }).Count -eq 1) 'Retried daemon launch after failure'
            } else {
                Assert-True ('open' -notin $state.Events) "Started replacement after ${case}: $events"
                if ($case -notin @('stop-failure', 'stop-timeout')) {
                    Assert-True ('force' -notin $state.Events -and 'quit' -notin $state.Events) "Stopped daemon after ${case}: $events"
                }
            }
            Write-Host "PASS: $case"
        }
        foreach ($invalidCountdown in @(-1, 3601)) {
            $failed = $false
            try { Restart-DotfilesDoom -Seconds $invalidCountdown } catch { $failed = $true }
            Assert-True $failed "Invalid countdown accepted: $invalidCountdown"
        }
        Write-Host 'PASS: countdown validation'
    }
} finally {
    $env:USERPROFILE = $previousProfile
    Remove-Item -LiteralPath $fixture -Recurse -Force
}
