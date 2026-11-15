. "$PSScriptRoot\emacs-windows.ps1"

function Get-DotfilesEmacsPaths {
    $runtime = Join-Path $env:USERPROFILE '.config\emacs'
    if (-not (Test-Path -LiteralPath $runtime)) {
        $runtime = Join-Path $env:USERPROFILE '.emacs.d'
    }
    if (Test-Path -LiteralPath $runtime) {
        $runtime
        $item = Get-Item -LiteralPath $runtime -Force
        if ($item.LinkType -and $item.Target) {
            $target = [string]@($item.Target)[0]
            if (-not [IO.Path]::IsPathRooted($target)) {
                $target = Join-Path (Split-Path $runtime) $target
            }
            if (Test-Path -LiteralPath $target) { $target }
        }
    }
    $emacs = Get-DotfilesEmacsRuntime
    Split-Path (Split-Path $emacs.Executable)
}
