#Requires -Version 5.1
[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [string]$Path
)

$ErrorActionPreference = 'Stop'
try {
    $file = Get-Item -LiteralPath $Path -ErrorAction Stop
    if ($file.PSIsContainer -or $file.Name -notlike '*.Tests.ps1') {
        throw 'Select a .Tests.ps1 file.'
    }
    Import-Module Pester -ErrorAction Stop
    $version = (Get-Module Pester).Version
    Write-Host "Pester $version | $($file.FullName)"
    if ($version.Major -ge 5) {
        $configuration = New-PesterConfiguration
        $configuration.Run.Path = @($file.FullName)
        $configuration.Run.PassThru = $true
        $configuration.Run.Exit = $false
        $configuration.Output.Verbosity = 'Detailed'
        $result = Invoke-Pester -Configuration $configuration
    } else {
        $result = Invoke-Pester -Script $file.FullName -PassThru
    }
    if ($result.TotalCount -eq 0) {
        throw "Pester executed no tests in $($file.FullName)."
    }
    $failures = if ($version.Major -ge 5) { $result.Failed }
                else { $result.TestResult | Where-Object { $_.Result -eq 'Failed' } }
    foreach ($failure in $failures) {
        $trace = if ($version.Major -ge 5) {
            ($failure.ErrorRecord | ForEach-Object { $_.ScriptStackTrace }) -join "`n"
        } else { $failure.StackTrace }
        $message = if ($version.Major -ge 5) {
            ($failure.ErrorRecord | ForEach-Object { $_.Exception.Message }) -join '; '
        } else { $failure.FailureMessage }
        $location = $null
        if ($trace -match '(?m)^at line: (\d+) in (.+?)\r?$') {
            $location = '{0}:{1}' -f $Matches[2], $Matches[1]
        } elseif ($trace -match '(?m)^at .+?, (.+): line (\d+)\r?$') {
            $location = '{0}:{1}' -f $Matches[1], $Matches[2]
        }
        if ($location) {
            Write-Host ("{0}: error: {1}" -f $location, ($message -replace '[\r\n]+', ' '))
        }
    }
    Write-Host "Pester result: $($result.PassedCount) passed, $($result.FailedCount) failed, $($result.SkippedCount) skipped."
    if ($result.FailedCount -gt 0 -or
        ($version.Major -ge 5 -and $result.Result -ne 'Passed')) { exit 1 }
    exit 0
} catch {
    Write-Error -ErrorRecord $_ -ErrorAction Continue
    exit 2
}
