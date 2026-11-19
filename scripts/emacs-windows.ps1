#Requires -Version 7.3
param(
    [switch]$Client,
    [switch]$Gui,
    [switch]$DoomClient,
    [string[]]$EmacsArguments = @()
)

function Get-DotfilesEmacsRuntime {
    $receipt = Join-Path $PSScriptRoot '..\.build\emacs\accepted.json'
    if (-not (Test-Path -LiteralPath $receipt)) {
        throw 'No accepted Emacs build. Run scripts\build-emacs-windows.ps1.'
    }
    $build = Get-Content -LiteralPath $receipt -Raw -ErrorAction Stop | ConvertFrom-Json
    if (-not $build.executable -or -not (Test-Path -LiteralPath $build.executable) -or
        -not $build.compilerRoot -or -not (Test-Path -LiteralPath "$($build.compilerRoot)\bin\libgccjit-0.dll")) {
        throw 'Accepted Emacs installation is incomplete. Run scripts\build-emacs-windows.ps1.'
    }
    $bin = Split-Path $build.executable
    [pscustomobject]@{
        Executable = $build.executable
        GuiExecutable = Join-Path $bin 'runemacs.exe'
        Client = Join-Path $bin 'emacsclient.exe'
        GuiClient = Join-Path $bin 'emacsclientw.exe'
        CompilerRoot = $build.compilerRoot
    }
}

function Invoke-DotfilesEmacs {
    param([switch]$Client, [switch]$Gui, [string[]]$Arguments = @())
    $runtime = Get-DotfilesEmacsRuntime
    $executable = if ($Client) {
        if ($Gui) { $runtime.GuiClient } else { $runtime.Client }
    } elseif ($Gui) { $runtime.GuiExecutable } else { $runtime.Executable }
    if (-not (Test-Path -LiteralPath $executable)) { throw "Emacs executable is missing: $executable" }
    $path = $env:PATH
    $compiler = $env:EMACS_NATIVE_COMP_ROOT
    $serverFile = $env:EMACS_SERVER_FILE
    $daemon = -not $Client -and [bool]($Arguments -match '^--daemon(?:=[A-Za-z0-9._-]+)?$')
    try {
        $PSNativeCommandArgumentPassing = 'Standard'
        $env:EMACS_NATIVE_COMP_ROOT = $runtime.CompilerRoot
        $env:PATH = "$(Join-Path $runtime.CompilerRoot 'bin');$(Get-DotfilesProcessPath)"
        if ($Client -and -not $env:EMACS_SERVER_FILE) {
            $env:EMACS_SERVER_FILE = Join-Path $env:USERPROFILE '.config\emacs\server\server'
        }
        if (-not $Gui -and -not $daemon) {
            & $executable @Arguments
            if ($LASTEXITCODE -ne 0) { throw "Emacs failed (exit $LASTEXITCODE)." }
            return
        }
        $start = [Diagnostics.ProcessStartInfo]::new($executable)
        $start.UseShellExecute = $false
        foreach ($argument in $Arguments) { $start.ArgumentList.Add($argument) }
        $start.CreateNoWindow = $daemon
        $process = [Diagnostics.Process]::Start($start)
        try {
            # Wait for the launcher, not runemacs.exe's long-lived child.
            if (-not $daemon) {
                $process.WaitForExit()
                $global:LASTEXITCODE = $process.ExitCode
                if ($process.ExitCode -ne 0) { throw "Emacs failed (exit $($process.ExitCode))." }
            } else { $process.Id }
        } finally { $process.Dispose() }
    } finally {
        $env:PATH = $path
        $env:EMACS_NATIVE_COMP_ROOT = $compiler
        $env:EMACS_SERVER_FILE = $serverFile
    }
}

function Invoke-DotfilesDoomClient {
    param(
        [string]$ClientPath,
        [string]$ServerFile,
        [string]$Expression = '(emacs-pid)',
        [int]$TimeoutMilliseconds = 5000,
        [switch]$AllowUnavailable
    )
    if (-not (Test-Path -LiteralPath $ServerFile -PathType Leaf)) {
        if ($AllowUnavailable) { return $null }
        throw "Doom server file is missing: $ServerFile. No daemon was stopped or started."
    }
    $start = [Diagnostics.ProcessStartInfo]::new($ClientPath)
    $start.UseShellExecute = $false
    $start.CreateNoWindow = $true
    $start.RedirectStandardOutput = $true
    $start.RedirectStandardError = $true
    [void]$start.Environment.Remove('ALTERNATE_EDITOR')
    foreach ($argument in @('--alternate-editor=false', '--server-file', $ServerFile, '--eval', $Expression)) {
        $start.ArgumentList.Add($argument)
    }
    $process = [Diagnostics.Process]::Start($start)
    $output = $process.StandardOutput.ReadToEndAsync()
    $errors = $process.StandardError.ReadToEndAsync()
    try {
        if (-not $process.WaitForExit($TimeoutMilliseconds)) {
            Stop-Process -Id $process.Id -ErrorAction Stop
            throw 'Doom client timed out. The daemon may still be busy; no additional daemon was started.'
        }
        $reply = $output.GetAwaiter().GetResult().Trim()
        $diagnostic = $errors.GetAwaiter().GetResult()
        if ($process.ExitCode -eq 0) { return $reply }
        if (-not $AllowUnavailable) {
            throw "Doom client failed (exit $($process.ExitCode)): $diagnostic"
        }
        Write-Verbose "Doom server connection failed: $diagnostic"
        return $null
    } finally { $process.Dispose() }
}

function Test-DotfilesDoomServer {
    param([string]$ClientPath, [string]$ServerFile, [int]$TimeoutMilliseconds = 120000)
    $reply = Invoke-DotfilesDoomClient -ClientPath $ClientPath -ServerFile $ServerFile `
        -TimeoutMilliseconds $TimeoutMilliseconds -AllowUnavailable
    if ($null -eq $reply) { return $false }
    if ($reply -notmatch '^\d+$') { throw "Unexpected Doom server reply: $reply" }
    return $true
}

function Open-DotfilesDoom {
    param([int]$StartupTimeoutSeconds = 120)
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
    $serverFile = Join-Path $doomDirectory 'server\server'
    $userId = [Security.Principal.WindowsIdentity]::GetCurrent().User.Value
    $mutex = [Threading.Mutex]::new($false, "Local\DotfilesDoom-$userId")
    $locked = $false
    $daemon = $null
    try {
        try { $locked = $mutex.WaitOne($StartupTimeoutSeconds * 1000) }
        catch [Threading.AbandonedMutexException] { $locked = $true }
        if (-not $locked) { throw 'Another Doom launch is still in progress. Try again after it finishes.' }
        $timeoutMilliseconds = [Math]::Max(1, $StartupTimeoutSeconds * 1000)
        if (-not (Test-DotfilesDoomServer -ClientPath $runtime.Client -ServerFile $serverFile -TimeoutMilliseconds $timeoutMilliseconds)) {
            $daemonId = Invoke-DotfilesEmacs -Arguments @('--daemon', "--init-directory=$doomDirectory")
            $daemon = [Diagnostics.Process]::GetProcessById($daemonId)
            $startup = [Diagnostics.Stopwatch]::StartNew()
            while ($true) {
                $remaining = $StartupTimeoutSeconds * 1000 - $startup.ElapsedMilliseconds
                if ($remaining -le 0) {
                    throw 'Doom daemon startup timed out. Check Emacs before retrying; no additional daemon was started.'
                }
                if (Test-DotfilesDoomServer -ClientPath $runtime.Client -ServerFile $serverFile -TimeoutMilliseconds $remaining) {
                    break
                }
                if ($daemon.HasExited) {
                    throw "Doom daemon exited before its server became ready (exit $($daemon.ExitCode)). Run scripts\invoke-doom.ps1 -Command doctor."
                }
                Start-Sleep -Milliseconds 250
            }
        }
        Invoke-DotfilesEmacs -Client -Gui -Arguments @('--server-file', $serverFile, '-c', '-n')
    } finally {
        if ($daemon) { $daemon.Dispose() }
        if ($locked) { $mutex.ReleaseMutex() }
        $mutex.Dispose()
    }
}

function Get-DotfilesProcessPath {
    $persistent = @(
        [Environment]::GetEnvironmentVariable('PATH', 'Machine') -split ';'
        [Environment]::GetEnvironmentVariable('PATH', 'User') -split ';'
    ) | Where-Object { $_ }
    if (-not $persistent) { return $env:PATH }
    $normalize = { param($entry) $entry.Trim().TrimEnd('\').ToLowerInvariant() }
    $persistentKeys = @($persistent | ForEach-Object { & $normalize $_ })
    $processOnly = @($env:PATH -split ';' | Where-Object {
        $_ -and (& $normalize $_) -notin $persistentKeys
    })
    $seen = @{}
    (@($processOnly) + @($persistent) | Where-Object {
        $key = & $normalize $_
        if (-not $seen.ContainsKey($key)) { $seen[$key] = $true; $true }
    }) -join ';'
}

if ($MyInvocation.InvocationName -ne '.') {
    $ErrorActionPreference = 'Stop'
    if ($DoomClient) {
        try { Open-DotfilesDoom }
        catch {
            Add-Type -AssemblyName System.Windows.Forms
            [void][System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'Doom Emacs', 'OK', 'Error')
            throw
        }
    } else {
        Invoke-DotfilesEmacs -Client:$Client -Gui:$Gui -Arguments (@($EmacsArguments) + @($args))
    }
}
