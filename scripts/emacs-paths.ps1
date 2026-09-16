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
    $emacs = Get-Command emacs.exe -ErrorAction SilentlyContinue
    if ($emacs) {
        $bin = Split-Path $emacs.Source
        $install = Split-Path $bin
        if ((Split-Path $bin -Leaf) -eq 'bin' -and
            (Split-Path $install -Leaf) -match '^emacs(?:[-.]|$)') {
            $install
        } else {
            # Never exclude a shared tools directory for a standalone executable.
            $emacs.Source
        }
    }
}
