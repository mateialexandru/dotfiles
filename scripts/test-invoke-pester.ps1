#Requires -Version 5.1
$ErrorActionPreference = 'Stop'
$root = Join-Path ([IO.Path]::GetTempPath()) ("pester-runner-" + [guid]::NewGuid().ToString('N'))
$executable = (Get-Process -Id $PID).Path
$runner = Join-Path $PSScriptRoot 'invoke-pester.ps1'
try {
    $directory = Join-Path $root "space & quote's"
    New-Item -ItemType Directory -Path $directory -Force | Out-Null
    foreach ($case in @(
        @{ Name = 'passing'; Body = "Describe 'suite' { It 'passes' { } }"; Exit = 0 },
        @{ Name = 'failing'; Body = "Describe 'suite' { It 'fails' { throw 'expected fixture failure' } }"; Exit = 1 },
        @{ Name = 'empty'; Body = '# No tests'; Exit = 2 }
    )) {
        $file = Join-Path $directory ($case.Name + '.Tests.ps1')
        Set-Content -LiteralPath $file -Value $case.Body -Encoding UTF8
        $output = & $executable -NoLogo -NoProfile -File $runner -Path $file 2>&1 | Out-String
        if ($LASTEXITCODE -ne $case.Exit) {
            throw "$($case.Name): expected exit $($case.Exit), got $LASTEXITCODE`n$output"
        }
        if ($case.Name -eq 'failing' -and
            $output -notmatch '\.Tests\.ps1:1: error: expected fixture failure') {
            throw "Pester failure did not include a clickable source location.`n$output"
        }
        Write-Host "Passed: $($case.Name), exit $LASTEXITCODE, quoted path preserved"
    }
} finally {
    if (Test-Path -LiteralPath $root) { Remove-Item -LiteralPath $root -Recurse -Force }
}
exit 0
