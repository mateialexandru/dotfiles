# Shared Windows interpreter selection; no installation occurs when dot-sourced.
function Invoke-DotfilesPythonProbe {
    param(
        [Parameter(Mandatory)][string]$FilePath,
        [string[]]$Arguments,
        [ValidateRange(1, 120000)][int]$TimeoutMilliseconds = 15000
    )

    if (-not ('Dotfiles.PythonProbeErrorMode' -as [type])) {
        Add-Type -TypeDefinition @'
using System.Runtime.InteropServices;
namespace Dotfiles {
    public static class PythonProbeErrorMode {
        [DllImport("kernel32.dll")] public static extern uint GetErrorMode();
        [DllImport("kernel32.dll")] public static extern uint SetErrorMode(uint mode);
    }
}
'@
    }
    $start = [Diagnostics.ProcessStartInfo]::new($FilePath)
    $start.UseShellExecute = $false
    $start.CreateNoWindow = $true
    $start.RedirectStandardOutput = $true
    $start.RedirectStandardError = $true
    $start.Environment['PYTHON_MANAGER_AUTOMATIC_INSTALL'] = 'false'
    foreach ($argument in $Arguments) { $start.ArgumentList.Add($argument) }
    $process = $null
    try {
        $previousMode = [Dotfiles.PythonProbeErrorMode]::GetErrorMode()
        try {
            # Child processes inherit this mode, so missing DLLs return errors instead of modal dialogs.
            [void][Dotfiles.PythonProbeErrorMode]::SetErrorMode($previousMode -bor 1)
            $process = [Diagnostics.Process]::Start($start)
        } finally {
            [void][Dotfiles.PythonProbeErrorMode]::SetErrorMode($previousMode)
        }
        $stdout = $process.StandardOutput.ReadToEndAsync()
        $stderr = $process.StandardError.ReadToEndAsync()
        if (-not $process.WaitForExit($TimeoutMilliseconds)) {
            try {
                # uv and py may be waiting on an interpreter child; stop only this probe's children.
                foreach ($entry in @(Get-CimInstance Win32_Process -Filter "ParentProcessId=$($process.Id)")) {
                    $child = Get-Process -Id $entry.ProcessId -ErrorAction SilentlyContinue
                    if (-not $child) { continue }
                    try {
                        $null = $child.Handle
                        if ($child.StartTime.ToUniversalTime().ToString('yyyyMMddHHmmssfff') -eq
                            $entry.CreationDate.ToUniversalTime().ToString('yyyyMMddHHmmssfff')) {
                            Stop-Process -Id $child.Id -ErrorAction Stop
                        }
                    } finally {
                        $child.Dispose()
                    }
                }
            } finally {
                if (-not $process.HasExited) { Stop-Process -Id $process.Id -ErrorAction Stop }
            }
            throw "Python discovery timed out after $TimeoutMilliseconds ms: $FilePath"
        }
        [pscustomobject]@{
            ExitCode = $process.ExitCode
            StdOut = $stdout.GetAwaiter().GetResult().Trim()
            StdErr = $stderr.GetAwaiter().GetResult().Trim()
        }
    } finally {
        if ($process) { $process.Dispose() }
    }
}

function Get-DotfilesPython {
    param([string]$Python, [switch]$AllowMissing)

    $probe = @'
import ensurepip, json, ssl, struct, sys, sysconfig, venv
assert sys.platform == "win32" and sys.implementation.name == "cpython"
assert sys.version_info >= (3, 12) and sys.version_info.releaselevel == "final"
assert struct.calcsize("P") == 8 and sys.prefix == sys.base_prefix
assert not sysconfig.get_config_var("Py_GIL_DISABLED")
print(json.dumps({"executable": sys.executable, "version": ".".join(map(str, sys.version_info[:3]))}))
'@
    $candidates = if ($Python) { @($Python) } else { @('python.exe', 'py.exe') }
    foreach ($candidate in $candidates) {
        $command = Get-Command $candidate -CommandType Application -ErrorAction SilentlyContinue |
            Select-Object -First 1
        if (-not $command) { continue }
        if ($command.Name -eq 'python.exe' -and
            $command.Source -like '*\Microsoft\WindowsApps\python.exe') { continue }
        $arguments = @(if ($command.Name -eq 'py.exe') { '-3' })
        $result = Invoke-DotfilesPythonProbe -FilePath $command.Source -Arguments (@($arguments) + @('-I', '-c', $probe))
        if ($result.ExitCode -ne 0) {
            if ($Python) { throw "Incompatible Python $Python (exit $($result.ExitCode)): $($result.StdErr)" }
            Write-Warning "Skipping incompatible Python $($command.Source) (exit $($result.ExitCode)): $($result.StdErr)"
            continue
        }
        $runtime = $result.StdOut | ConvertFrom-Json
        if (-not $runtime.executable -or
            -not (Test-Path -LiteralPath $runtime.executable -PathType Leaf)) {
            throw 'Python reported a missing executable.'
        }
        $signature = Get-AuthenticodeSignature -LiteralPath $runtime.executable
        if ($signature.Status -ne 'Valid' -or
            $signature.SignerCertificate.Subject -notmatch '(?:^|,\s*)O=Python Software Foundation(?:,|$)') {
            if ($Python) { throw "Python is not signed by the Python Software Foundation: $($runtime.executable)" }
            Write-Verbose "Skipping Python without a valid PSF signature: $($runtime.executable)"
            continue
        }
        return $runtime
    }
    if ($AllowMissing -and -not $Python) { return $null }
    throw 'Official 64-bit CPython 3.12+ is required. Run dotfiles scripts\install-python-runtime.ps1 -AllowDownloads, or supply -Python with a PSF-signed interpreter.'
}

function Set-DotfilesPythonPath {
    [CmdletBinding(SupportsShouldProcess)]
    param([Parameter(Mandatory)][string]$Python, [switch]$Check)

    $root = Split-Path -Parent $Python
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    $entries = @($userPath -split ';' | Where-Object { $_ })
    $normalized = @($entries | ForEach-Object {
        [Environment]::ExpandEnvironmentVariables($_.Trim().Trim('"')).TrimEnd('\')
    })
    if ($Check) {
        if ($root.TrimEnd('\') -notin $normalized) {
            throw 'The verified Python is missing from your user PATH. Run scripts\install-python-runtime.ps1, then open a new terminal.'
        }
        return
    }
    $desired = (@($root) + @(for ($index = 0; $index -lt $entries.Count; $index++) {
        if ($normalized[$index] -ne $root.TrimEnd('\')) { $entries[$index] }
    })) -join ';'
    if ($desired -ne $userPath -and $PSCmdlet.ShouldProcess('Current user PATH', "Prepend verified Python: $root")) {
        [Environment]::SetEnvironmentVariable('Path', $desired, 'User')
    }
}

function Assert-DotfilesUv {
    Assert-UvSecurity
    foreach ($tool in @('uv', 'uvx')) {
        $command = Get-Command $tool -CommandType Application -ErrorAction Stop | Select-Object -First 1
        $file = Get-Item -LiteralPath $command.Source
        $target = if ($file.LinkType) { $file.ResolveLinkTarget($true).FullName } else { $file.FullName }
        $managed = foreach ($root in @($env:LOCALAPPDATA, $env:ProgramFiles)) {
            if ($root) {
                $packages = Join-Path $root 'Microsoft\WinGet\Packages'
                if ($root -eq $env:ProgramFiles) { $packages = Join-Path $root 'WinGet\Packages' }
                $target -like "$packages\astral-sh.uv_*\$tool.exe"
            }
        }
        if ($managed -notcontains $true) {
            throw "$tool resolves outside the WinGet-owned Astral package: $($command.Source). Review PATH or migrate it explicitly; no executable was replaced."
        }
    }
}

function Set-DotfilesPythonPolicy {
    [CmdletBinding(SupportsShouldProcess)]
    param([switch]$Check)
    foreach ($entry in @{
        UV_PYTHON_DOWNLOADS = 'never'
        UV_PYTHON_PREFERENCE = 'only-system'
    }.GetEnumerator()) {
        if ($Check) {
            if ([Environment]::GetEnvironmentVariable($entry.Key, 'Process') -ne $entry.Value -or
                [Environment]::GetEnvironmentVariable($entry.Key, 'User') -ne $entry.Value) {
                throw "$($entry.Key) must be '$($entry.Value)'. Run scripts\install-python-runtime.ps1, then open a new terminal."
            }
        } elseif ($PSCmdlet.ShouldProcess('Current user and process', "Set $($entry.Key)=$($entry.Value)")) {
            [Environment]::SetEnvironmentVariable($entry.Key, $entry.Value, 'User')
            [Environment]::SetEnvironmentVariable($entry.Key, $entry.Value, 'Process')
        }
    }

}

function Assert-DotfilesUvPython {
    $command = Get-Command uv -CommandType Application -ErrorAction Stop | Select-Object -First 1
    $result = Invoke-DotfilesPythonProbe -FilePath $command.Source -Arguments @(
        'python', 'find', '--system', '--no-config', '--no-python-downloads', '--python-preference', 'only-system'
    )
    if ($result.ExitCode -ne 0 -or -not $result.StdOut -or $result.StdOut -match '[\r\n]') {
        throw "uv cannot select an installed system Python (exit $($result.ExitCode)): $($result.StdErr) $($result.StdOut)"
    }
    $null = Get-DotfilesPython -Python $result.StdOut
}
