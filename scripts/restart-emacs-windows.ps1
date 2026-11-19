#Requires -Version 7.3
[CmdletBinding(SupportsShouldProcess)]
param([ValidateRange(0, 3600)][int]$Seconds = 10)

. "$PSScriptRoot\emacs-windows.ps1"

function Restart-DotfilesDoom {
    [CmdletBinding(SupportsShouldProcess)]
    param(
        [ValidateRange(0, 3600)][int]$Seconds = 10,
        [ValidateRange(0, 120)][int]$StopTimeoutSeconds = 10,
        [ValidateRange(0, 120)][int]$GuiTimeoutSeconds = 10
    )
    if (-not $PSCmdlet.ShouldProcess('Managed Doom Emacs daemon',
            "Notify, wait ${Seconds}s, save file buffers and restart")) { return }

    $runtime = Get-DotfilesEmacsRuntime
    foreach ($clientPath in @($runtime.Client, $runtime.GuiClient)) {
        if (-not (Test-Path -LiteralPath $clientPath -PathType Leaf)) {
            throw "Emacs client is missing: $clientPath. Run scripts\build-emacs-windows.ps1."
        }
    }
    $doomDirectory = Join-Path $env:USERPROFILE '.config\emacs'
    if (-not (Test-Path -LiteralPath "$doomDirectory\early-init.el" -PathType Leaf)) {
        throw "Doom is not set up at $doomDirectory. Run install.ps1 first."
    }
    if (-not (Get-Command New-BurntToastNotification -ErrorAction SilentlyContinue)) {
        throw 'BurntToast is missing. Run scripts\install-prerequisites.ps1 before restarting.'
    }
    $client = @{ ClientPath = $runtime.Client; ServerFile = Join-Path $doomDirectory 'server\server' }
    $userId = [Security.Principal.WindowsIdentity]::GetCurrent().User.Value
    # Serialize restarts separately; Open-DotfilesDoom owns the shared launch lock.
    $mutex = [Threading.Mutex]::new($false, "Local\DotfilesDoomRestart-$userId")
    $locked = $false
    $daemon = $null
    try {
        try { $locked = $mutex.WaitOne(0) }
        catch [Threading.AbandonedMutexException] { $locked = $true }
        if (-not $locked) { throw 'Another Doom restart is already in progress.' }
        $reply = Invoke-DotfilesDoomClient @client -Expression '(if (daemonp) (emacs-pid) nil)'
        $daemonId = 0
        if ($reply -notmatch '^[1-9]\d*$' -or -not [int]::TryParse($reply, [ref]$daemonId)) {
            throw "Cannot identify the managed Emacs daemon: $reply. No process was stopped."
        }
        $daemon = Get-Process -Id $daemonId -ErrorAction Stop
        if ($daemon.ProcessName -ne 'emacs' -or
            $daemon.SessionId -ne [Diagnostics.Process]::GetCurrentProcess().SessionId) {
            throw 'The server PID is not an Emacs process in this session. No process was stopped.'
        }
        # Pin the Windows process object so this PID cannot be recycled during the countdown.
        $null = $daemon.Handle
        New-BurntToastNotification -Text 'Doom Emacs', "Restarting the daemon in ${Seconds}s. Save your work now." `
            -ErrorAction Stop | Out-Null
        Write-Host "Restarting Doom Emacs (PID $daemonId) in ${Seconds}s. Ctrl+C cancels."
        if ($Seconds -gt 0) { Start-Sleep -Seconds $Seconds }

        # A failed or timed-out save must never queue a later shutdown.
        $identity = "(unless (= (emacs-pid) $daemonId) (error `"Doom daemon changed; restart aborted`"))"
        $saved = Invoke-DotfilesDoomClient @client -TimeoutMilliseconds 30000 `
            -Expression "(progn $identity (save-some-buffers t) t)"
        if ($saved -ne 't') { throw 'Emacs did not confirm saving file buffers. Restart aborted.' }
        if (-not $daemon.HasExited) {
            $shutdown = @'
(progn
  (run-at-time 0.2 nil
    (lambda ()
      (let ((confirm-kill-processes nil)
            (confirm-kill-emacs nil)
            (kill-emacs-query-functions nil)
            (persp-auto-save-opt 0))
        (kill-emacs 0))))
  t)
'@
            try { Invoke-DotfilesDoomClient @client -Expression "(progn $identity $shutdown)" | Out-Null }
            catch { Write-Warning "Graceful shutdown request failed: $_. Waiting before forcing PID $daemonId to stop." }
            if (-not $daemon.WaitForExit($StopTimeoutSeconds * 1000)) {
                Write-Warning "Emacs did not exit; forcing the verified daemon PID $daemonId to stop."
                Stop-Process -Id $daemonId -ErrorAction Stop
                if (-not $daemon.WaitForExit(5000)) { throw 'The old daemon is still running. No replacement was started.' }
            }
        }
        Open-DotfilesDoom
        $newDaemonId = Invoke-DotfilesDoomClient @client
        if ($newDaemonId -notmatch '^[1-9]\d*$' -or $newDaemonId -eq $reply) {
            throw "A fresh Doom daemon was not verified: $newDaemonId."
        }
        $gui = $false
        $startup = [Diagnostics.Stopwatch]::StartNew()
        while ($true) {
            $remaining = $GuiTimeoutSeconds * 1000 - $startup.ElapsedMilliseconds
            if ($remaining -le 0) { break }
            $reply = Invoke-DotfilesDoomClient @client -TimeoutMilliseconds $remaining -Expression `
                '(and (seq-some (lambda (frame) (and (display-graphic-p frame) (eq t (frame-visible-p frame)))) (frame-list)) t)'
            if ($reply -eq 't') { $gui = $true; break }
            Start-Sleep -Milliseconds 100
        }
        if (-not $gui) { throw "Doom daemon PID $newDaemonId is running, but no visible GUI frame was verified." }
        Write-Host "Doom restarted: PID $newDaemonId with a visible GUI frame."
    } finally {
        if ($daemon) { $daemon.Dispose() }
        if ($locked) { $mutex.ReleaseMutex() }
        $mutex.Dispose()
    }
}

if ($MyInvocation.InvocationName -ne '.') {
    $ErrorActionPreference = 'Stop'
    Restart-DotfilesDoom -Seconds $Seconds
}
