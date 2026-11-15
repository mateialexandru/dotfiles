$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\uv-security.ps1"

foreach ($case in @('secure', 'multiple', 'retire', 'custom', 'old-managed', 'rollback')) {
    & {
        $script:moved = @()
        $script:restored = @()
        $script:checked = 0
        function Get-Command($Name, $CommandType, $ErrorAction) {
            $path = if ($case -eq 'custom') { "C:\custom\$Name.exe" }
                    elseif ($case -in @('secure', 'multiple') -or $script:moved.Count -eq 2) {
                        Join-Path $env:LOCALAPPDATA "Microsoft\WinGet\Links\$Name.exe"
                    } else { Join-Path $env:USERPROFILE ".local\bin\$Name.exe" }
            [pscustomobject]@{ Source = $path }
            if ($case -eq 'multiple') { [pscustomobject]@{ Source = "C:\other\$Name.exe" } }
        }
        function Get-CheckedUvVersion($Executable) {
            if ($Executable -is [array]) { throw 'Only the selected executable may be checked' }
            if ($Executable -like '*WinGet*') {
                if ($case -eq 'old-managed') { [version]'0.8.6' }
                else { [version]'0.13.8' }
            } else { [version]'0.8.6' }
        }
        function Test-Path($LiteralPath, $PathType) { $LiteralPath -like '*WinGet*' }
        function Get-FileHash { [pscustomobject]@{ Hash = 'ABC123' } }
        function New-Item {}
        function Move-Item($LiteralPath, $Destination, $ErrorAction) {
            if ($LiteralPath -like '*retired-tools*') { $script:restored += $Destination }
            else {
                if ($case -eq 'rollback' -and $script:moved.Count -eq 1) {
                    throw 'Simulated second retirement failure'
                }
                $script:moved += $LiteralPath
            }
        }
        $failed = $false
        try { Repair-UvWindowsSelection } catch { $failed = $true }
        if ($failed -ne ($case -in @('custom', 'old-managed', 'rollback'))) {
            throw "Unexpected result: $case"
        }
        if ($case -eq 'retire' -and $script:moved.Count -ne 2) {
            throw 'Both stale executables must be retired'
        }
        if ($case -in @('secure', 'multiple') -and $script:moved.Count -ne 0) {
            throw 'Patched installations must remain unchanged'
        }
        if ($case -eq 'rollback' -and $script:restored.Count -ne 1) {
            throw 'A partial retirement must be rolled back'
        }
    }
    Write-Host "PASS: uv $case"
}

& {
    function Invoke-TestUv {
        $global:LASTEXITCODE = $script:testUvExit
        $script:testUvOutput
    }
    foreach ($test in @(
        @{ Output = 'uv 0.13.8 (release metadata)'; Exit = 0; Valid = $true },
        @{ Output = 'uvx 0.13.8'; Exit = 0; Valid = $true },
        @{ Output = 'uv 0.13.8-beta.1'; Exit = 0; Valid = $false },
        @{ Output = 'uv 0.13.8garbage'; Exit = 0; Valid = $false },
        @{ Output = 'unexpected 0.13.8'; Exit = 0; Valid = $false },
        @{ Output = 'uv 0.13.8'; Exit = 7; Valid = $false }
    )) {
        $script:testUvOutput = $test.Output
        $script:testUvExit = $test.Exit
        $accepted = $true
        try { $null = Get-CheckedUvVersion 'Invoke-TestUv' } catch { $accepted = $false }
        if ($accepted -ne $test.Valid) { throw "Wrong version acceptance: $($test.Output)" }
    }
    $global:LASTEXITCODE = 0
    Write-Host 'PASS: uv stable version parsing and nonzero exit rejection'
}
