param(
    [string]$Command = 'doctor',
    [string]$DoomDirectory = (Join-Path $env:USERPROFILE '.config\emacs'),
    [string]$EmacsExecutable,
    [switch]$Force
)

$ErrorActionPreference = 'Stop'
$doomArguments = @($args)
if (-not $EmacsExecutable) {
    $EmacsExecutable = (Get-Command emacs.exe -ErrorAction Stop).Source
}
$names = @('EMACSDIR', '__DOOMSH', '__DOOMPID', '__DOOMSTEP', '__DOOMGEOM', '__DOOMPIPE', 'PATH')
$saved = @{}
foreach ($name in $names) { $saved[$name] = [Environment]::GetEnvironmentVariable($name, 'Process') }
try {
    $nativeRoot = $env:EMACS_NATIVE_COMP_ROOT
    if (-not $nativeRoot) {
        $nativeRoot = [Environment]::GetEnvironmentVariable('EMACS_NATIVE_COMP_ROOT', 'User')
    }
    if ($nativeRoot) {
        $nativeBin = Join-Path $nativeRoot 'bin'
        foreach ($file in @('libgccjit-0.dll', 'gcc.exe', 'as.exe', 'ld.exe')) {
            if (-not (Test-Path (Join-Path $nativeBin $file))) {
                throw "Configured native compiler runtime is incomplete: $file"
            }
        }
        $env:PATH = "$nativeBin;$env:PATH"
    }
    $env:EMACSDIR = $DoomDirectory
    $env:__DOOMSH = 'ps1'
    $env:__DOOMPID = "$PID"
    $env:__DOOMSTEP = '0'
    $env:__DOOMGEOM = '120x40'
    $env:__DOOMPIPE = '01'
    $options = @('--no-color')
    if ($Force) { $options += '--force' }
    $bootstrap = "(setq warning-inhibit-types '((files missing-lexbind-cookie)))"
    if ($nativeRoot) {
        $bootstrap = "(progn $bootstrap (setq native-comp-async-jobs-number 2))"
    }
    # The upstream PowerShell wrapper can lose nonzero Emacs exit codes.
    & $EmacsExecutable -q --no-site-file --batch `
        --eval $bootstrap `
        --load (Join-Path $DoomDirectory 'bin\doom') -- @options $Command @doomArguments
    if ($LASTEXITCODE -ne 0) { throw "Doom $Command failed (exit $LASTEXITCODE)." }
} finally {
    foreach ($name in $names) { [Environment]::SetEnvironmentVariable($name, $saved[$name], 'Process') }
}
