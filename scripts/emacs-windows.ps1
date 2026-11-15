#Requires -Version 7.3
param(
    [switch]$Client,
    [switch]$Gui,
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
    $daemon = -not $Client -and $Arguments.Count -eq 1 -and
        $Arguments[0] -match '^--daemon(?:=[A-Za-z0-9._-]+)?$'
    try {
        $PSNativeCommandArgumentPassing = 'Standard'
        $env:EMACS_NATIVE_COMP_ROOT = $runtime.CompilerRoot
        $env:PATH = "$(Join-Path $runtime.CompilerRoot 'bin');$(Get-DotfilesProcessPath)"
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
    Invoke-DotfilesEmacs -Client:$Client -Gui:$Gui -Arguments (@($EmacsArguments) + @($args))
}
