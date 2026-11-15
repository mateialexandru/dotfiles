#Requires -Version 7.3
param(
    [string]$Command = 'doctor',
    [string]$DoomDirectory = (Join-Path $env:USERPROFILE '.config\emacs'),
    [string]$EmacsExecutable,
    [switch]$Force
)

$ErrorActionPreference = 'Stop'
$doomArguments = @($args)
. "$PSScriptRoot\emacs-paths.ps1"
$PSNativeCommandArgumentPassing = 'Standard'
$names = @('EMACSDIR', '__DOOMSH', '__DOOMPID', '__DOOMSTEP', '__DOOMGEOM', '__DOOMPIPE', 'PATH',
    'EMACS_NATIVE_COMP_ROOT')
$saved = @{}
foreach ($name in $names) { $saved[$name] = [Environment]::GetEnvironmentVariable($name, 'Process') }
try {
    $env:PATH = Get-DotfilesProcessPath
    if (-not $EmacsExecutable) {
        $selected = Get-DotfilesEmacsRuntime
        $EmacsExecutable = $selected.Executable
        $env:EMACS_NATIVE_COMP_ROOT = $selected.CompilerRoot
    }
    $nativeRoot = $env:EMACS_NATIVE_COMP_ROOT
    if ($nativeRoot) {
        $env:PATH = "$(Join-Path $nativeRoot 'bin');$env:PATH"
    }
    $env:EMACSDIR = $DoomDirectory
    $env:__DOOMSH = 'ps1'
    $env:__DOOMPID = "$PID"
    $env:__DOOMSTEP = '0'
    $env:__DOOMGEOM = '120x40'
    $env:__DOOMPIPE = '01'
    $options = @('--no-color')
    if ($Force) { $options += '--force' }
    $bootstrap = "(setq warning-inhibit-types '((files missing-lexbind-cookie)) coding-system-for-write 'utf-8-unix)"
    if ($nativeRoot) {
        $bootstrap = "(progn $bootstrap (setq native-comp-async-jobs-number 2))"
    }
    function doom {
        $nativeArguments = @(
            '-q', '--no-site-file', '--batch', '--eval', $bootstrap,
            '--load', (Join-Path $DoomDirectory 'bin\doom'), '--') + $options + @($args)
        & $EmacsExecutable @nativeArguments
        $result = $LASTEXITCODE
        if ($result -eq 254) {
            # Doom hands initialized context to a generated shell continuation.
            $continuation = Join-Path $env:TEMP "doom.$($env:__DOOMPID).$($env:__DOOMSTEP).ps1"
            if (-not (Test-Path -LiteralPath $continuation -PathType Leaf)) {
                throw "Doom $Command requested a missing continuation: $continuation"
            }
            . $continuation
        } elseif ($result -ne 0) {
            throw "Doom $Command failed (exit $result)."
        }
    }
    doom $Command @doomArguments
} finally {
    foreach ($name in $names) { Set-Item -LiteralPath "Env:$name" -Value $saved[$name] }
}
