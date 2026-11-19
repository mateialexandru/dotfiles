#Requires -Version 7.3
[CmdletBinding()]
param([switch]$Force, [switch]$Check)
$ErrorActionPreference = 'Stop'

function Set-DotfilesSkillEnvironment {
    [CmdletBinding(SupportsShouldProcess)]
    param([string]$Root, [switch]$Check)
    $current = [Environment]::GetEnvironmentVariable('DOOM_DOTFILES_DIR', 'User')
    if ($current -and [IO.Path]::GetFullPath($current).TrimEnd('\') -ne $Root.TrimEnd('\')) {
        throw "DOOM_DOTFILES_DIR already identifies $current. Update it explicitly before installing from $Root."
    }
    if ($Check) {
        if (-not $env:DOOM_DOTFILES_DIR -or
            [IO.Path]::GetFullPath($env:DOOM_DOTFILES_DIR).TrimEnd('\') -ne $Root.TrimEnd('\')) {
            throw 'DOOM_DOTFILES_DIR is missing or stale. Run scripts\install-skills.ps1, then open a new terminal.'
        }
    } elseif ($PSCmdlet.ShouldProcess('Current-user environment', "Set DOOM_DOTFILES_DIR to $Root")) {
        [Environment]::SetEnvironmentVariable('DOOM_DOTFILES_DIR', $Root, 'User')
        $env:DOOM_DOTFILES_DIR = $Root
    }
}

function Install-DotfilesSkillSet {
    [CmdletBinding()]
    param(
        [string]$Root = (Split-Path $PSScriptRoot -Parent),
        [string]$HomeDirectory = $env:USERPROFILE,
        [switch]$Force,
        [switch]$Check
    )
    if ($Check -and $Force) { throw '-Check cannot be combined with -Force.' }
    $Root = (Get-Item -LiteralPath $Root -ErrorAction Stop).FullName
    Set-DotfilesSkillEnvironment -Root $Root -Check:$Check
    $templates = Join-Path $Root 'scripts\skills'
    $skills = @(Get-ChildItem -LiteralPath $templates -Directory -ErrorAction Stop)
    if (-not $skills.Count) { throw "No public skill templates found: $templates" }
    foreach ($skill in $skills) {
        $source = Join-Path $skill.FullName 'SKILL.md'
        if (-not (Test-Path -LiteralPath $source -PathType Leaf)) {
            throw "Skill template is missing: $source"
        }
        $targets = @(
            (Join-Path $HomeDirectory ".copilot\skills\$($skill.Name)\SKILL.md"),
            (Join-Path $HomeDirectory ".claude\commands\$($skill.Name).md")
        )
        foreach ($target in $targets) {
            if ($Check) {
                if (-not (Test-Path -LiteralPath $target -PathType Leaf) -or
                    (Get-FileHash -LiteralPath $source).Hash -ne (Get-FileHash -LiteralPath $target).Hash) {
                    throw "Installed skill is missing or stale: $target. Review local edits, then run scripts\install-skills.ps1 -Force."
                }
            } elseif (-not (Test-Path -LiteralPath $target) -or $Force) {
                New-Item -ItemType Directory -Path (Split-Path $target -Parent) -Force | Out-Null
                Copy-Item -LiteralPath $source -Destination $target -Force
            }
        }
        Write-Host "Public skill: $($skill.Name)"
    }
}

if ($MyInvocation.InvocationName -ne '.') {
    Install-DotfilesSkillSet -Force:$Force -Check:$Check
}
