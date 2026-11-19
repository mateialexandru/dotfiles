#Requires -Version 7.3
$ErrorActionPreference = 'Stop'
. "$PSScriptRoot\install-skills.ps1"
$temporary = Join-Path ([IO.Path]::GetTempPath()) "dotfiles-skills-$([guid]::NewGuid())"
try {
    $homeDirectory = Join-Path $temporary 'home'
    $sourceDirectory = Join-Path $temporary 'scripts\skills\example'
    New-Item -ItemType Directory -Path $sourceDirectory -Force | Out-Null
    $source = Join-Path $sourceDirectory 'SKILL.md'
    Set-Content -LiteralPath $source -Value 'canonical skill'
    function Set-DotfilesSkillEnvironment {
        [Diagnostics.CodeAnalysis.SuppressMessageAttribute('PSUseShouldProcessForStateChangingFunctions', '',
            Justification = 'Mock only records environment intent; it never changes the environment.')]
        param($Root, [switch]$Check)
        if ($Root -ne $temporary) { throw 'Wrong public checkout.' }
        $script:checked = [bool]$Check
    }
    Install-DotfilesSkillSet -Root $temporary -HomeDirectory $homeDirectory
    $targets = @(
        (Join-Path $homeDirectory '.copilot\skills\example\SKILL.md'),
        (Join-Path $homeDirectory '.claude\commands\example.md')
    )
    foreach ($target in $targets) {
        if ((Get-Content -LiteralPath $target) -ne 'canonical skill') { throw 'Fresh skill install failed.' }
    }
    Set-Content -LiteralPath $targets[0] -Value 'local edits'
    Install-DotfilesSkillSet -Root $temporary -HomeDirectory $homeDirectory
    if ((Get-Content -LiteralPath $targets[0]) -ne 'local edits') { throw 'Rerun overwrote an existing skill.' }
    $failed = $false
    try { Install-DotfilesSkillSet -Root $temporary -HomeDirectory $homeDirectory -Check } catch {
        $failed = $_.Exception.Message -like 'Installed skill is missing or stale:*'
    }
    if (-not $failed -or -not $checked) { throw 'Read-only check did not detect a stale skill.' }
    if ((Get-Content -LiteralPath $targets[0]) -ne 'local edits') { throw 'Check modified a skill.' }
    Install-DotfilesSkillSet -Root $temporary -HomeDirectory $homeDirectory -Force
    Install-DotfilesSkillSet -Root $temporary -HomeDirectory $homeDirectory -Check
    $failed = $false
    try { Install-DotfilesSkillSet -Root $temporary -HomeDirectory $homeDirectory -Check -Force } catch { $failed = $true }
    if (-not $failed) { throw 'Mutating and read-only flags must conflict.' }
    Write-Host 'PASS: public skill installation, preservation, explicit refresh and read-only checks'
} finally {
    Remove-Item -LiteralPath $temporary -Recurse -Force
}
