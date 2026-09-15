# hack.ps1 — KISS worktree manager (v3)
# Dot-source this file in your PowerShell profile:
#   . "<dotfiles>/shell/hack.ps1"
#
# Two commands: `workshop` (repo config) and `hack` (worktree lifecycle)
# Requires: git, fzf

# ---------------------------------------------------------------------------
# Config helpers
# ---------------------------------------------------------------------------

# PS 5.1 compat: null-coalescing helper (no ?? operator)
function script:Coalesce { foreach ($a in $args) { if ($null -ne $a -and $a -ne '') { return $a } } }

$script:HackConfigDir = if ($IsLinux -or $IsMacOS) {
    Join-Path (Join-Path $env:HOME ".config") "hack"
} else {
    Join-Path (Join-Path $env:USERPROFILE ".config") "hack"
}
$script:HackConfigPath = Join-Path $script:HackConfigDir "config.json"

function Get-HackConfig {
    if (Test-Path $script:HackConfigPath) {
        try { return Get-Content $script:HackConfigPath -Raw | ConvertFrom-Json }
        catch { Write-Warning "Failed to parse hack config: $_"; return $null }
    }
    return $null
}

function Save-HackConfig {
    param([Parameter(Mandatory)][object]$Config)
    if (-not (Test-Path $script:HackConfigDir)) {
        New-Item -ItemType Directory -Path $script:HackConfigDir -Force | Out-Null
    }
    $Config | ConvertTo-Json -Depth 10 | Set-Content -Path $script:HackConfigPath -Encoding UTF8
}

function Initialize-HackConfig {
    $defaultBase = if ($IsLinux -or $IsMacOS) { Join-Path $env:HOME "worktree" } else { "C:\worktree" }
    $defaultPrefix = "user/$(Coalesce $env:USERNAME $env:USER)"

    Write-Host ""
    $baseDir = Read-Host "Worktree base directory [$defaultBase]"
    if ([string]::IsNullOrWhiteSpace($baseDir)) { $baseDir = $defaultBase }

    $branchPrefix = Read-Host "Branch prefix [$defaultPrefix]"
    if ([string]::IsNullOrWhiteSpace($branchPrefix)) { $branchPrefix = $defaultPrefix }

    $config = [ordered]@{
        baseDir           = $baseDir
        branchPrefix      = $branchPrefix
        defaultBaseBranch = "develop"
        defaultRepo       = $null
        repos             = [ordered]@{}
    }
    Save-HackConfig $config
    Write-Host "(Saved to $script:HackConfigPath)" -ForegroundColor Gray
    return Get-HackConfig
}

function Require-HackConfig {
    $c = Get-HackConfig
    if (-not $c) { throw "No hack config. Run: workshop add ALIAS URL" }
    return $c
}

# ---------------------------------------------------------------------------
# Repo helpers
# ---------------------------------------------------------------------------

function Resolve-Repo {
    param([string]$Alias, [object]$Config)
    if (-not $Alias) {
        # Infer from PWD or default
        $Alias = Resolve-RepoFromPwd -Config $Config
        if (-not $Alias) { $Alias = $Config.defaultRepo }
        if (-not $Alias) { throw "No repo specified and no default set. Use: hack REPO/BRANCH" }
    }
    if (-not ($Config.repos.PSObject.Properties.Name -contains $Alias)) {
        throw "Unknown repo '$Alias'. Run 'workshop add ALIAS URL' first."
    }
    $repo = $Config.repos.$Alias
    return @{
        Alias      = $Alias
        Url        = $repo.url
        BaseBranch = Coalesce $repo.baseBranch $Config.defaultBaseBranch "develop"
    }
}

function Resolve-RepoFromPwd {
    param([object]$Config)
    $pwd = (Get-Location).Path
    if (-not $pwd.StartsWith($Config.baseDir)) { return $null }
    $relative = $pwd.Substring($Config.baseDir.Length).TrimStart([IO.Path]::DirectorySeparatorChar, '/')
    $parts = $relative -split '[/\\]'
    if ($parts.Count -ge 1 -and $parts[0] -ne ".trees" -and
        $Config.repos.PSObject.Properties.Name -contains $parts[0]) {
        return $parts[0]
    }
    return $null
}

function Get-BaseRepoDir {
    param([string]$Alias, [object]$Config)
    return [IO.Path]::Combine($Config.baseDir, ".trees", $Alias)
}

function Ensure-BaseClone {
    param(
        [Parameter(Mandatory)][string]$Alias,
        [Parameter(Mandatory)][string]$Url,
        [Parameter(Mandatory)][string]$BaseBranch,
        [Parameter(Mandatory)][object]$Config
    )
    $baseRepoDir = Get-BaseRepoDir -Alias $Alias -Config $Config

    if (-not (Test-Path $baseRepoDir)) {
        Write-Host "Cloning '$Alias'..." -ForegroundColor Cyan
        $treesDir = [IO.Path]::Combine($Config.baseDir, ".trees")
        New-Item -ItemType Directory -Path $treesDir -Force | Out-Null
        git clone --branch $BaseBranch $Url $baseRepoDir | Out-Host
        if ($LASTEXITCODE -ne 0) { throw "Failed to clone repository." }
    } else {
        Write-Host "Fetching latest $BaseBranch..." -ForegroundColor Cyan
        git -C $baseRepoDir fetch origin "${BaseBranch}:${BaseBranch}" --force 2>$null
    }
    return $baseRepoDir
}

# ---------------------------------------------------------------------------
# Worktree helpers
# ---------------------------------------------------------------------------

function New-HackWorktree {
    param(
        [Parameter(Mandatory)][string]$Alias,
        [Parameter(Mandatory)][string]$BranchName,
        [Parameter(Mandatory)][string]$DirName,
        [Parameter(Mandatory)][object]$Config,
        [switch]$FromExisting
    )
    $resolved = Resolve-Repo -Alias $Alias -Config $Config
    $baseRepoDir = Ensure-BaseClone -Alias $resolved.Alias -Url $resolved.Url `
        -BaseBranch $resolved.BaseBranch -Config $Config
    $worktreeDir = [IO.Path]::Combine($Config.baseDir, $Alias, $DirName)

    if (Test-Path $worktreeDir) { throw "Worktree '$worktreeDir' already exists." }

    $parentDir = [IO.Path]::Combine($Config.baseDir, $Alias)
    New-Item -ItemType Directory -Path $parentDir -Force | Out-Null

    if ($FromExisting) {
        Write-Host "Checking out existing branch '$BranchName'..." -ForegroundColor Cyan
        git -C $baseRepoDir fetch origin "${BranchName}:${BranchName}" 2>&1 | Out-Null
        git -C $baseRepoDir worktree add $worktreeDir $BranchName | Out-Host
    } else {
        Write-Host "Creating worktree '$DirName' on new branch '$BranchName'..." -ForegroundColor Cyan
        git -C $baseRepoDir worktree add -b $BranchName $worktreeDir $resolved.BaseBranch | Out-Host
    }
    if ($LASTEXITCODE -ne 0) { throw "Failed to create worktree." }

    Write-Host "Worktree: $worktreeDir" -ForegroundColor Green
    return $worktreeDir
}

function Get-HackWorktrees {
    param([string]$Repo = "all")

    $config = Get-HackConfig
    if (-not $config -or -not (Test-Path $config.baseDir)) { return @() }

    $worktrees = @()
    $repoDirs = Get-ChildItem $config.baseDir -Directory | Where-Object { $_.Name -ne ".trees" }

    foreach ($repoDir in $repoDirs) {
        $repoName = $repoDir.Name
        if ($Repo -ne "all" -and $repoName -ne $Repo) { continue }
        if (-not ($config.repos.PSObject.Properties.Name -contains $repoName)) { continue }

        $repoConf = $config.repos.$repoName
        $baseBranch = Coalesce $repoConf.baseBranch $config.defaultBaseBranch "develop"
        $baseRepoDir = Get-BaseRepoDir -Alias $repoName -Config $config

        $taskDirs = Get-ChildItem $repoDir.FullName -Directory -ErrorAction SilentlyContinue
        foreach ($taskDir in $taskDirs) {
            $wtPath = $taskDir.FullName
            if (-not (Test-Path ([IO.Path]::Combine($wtPath, ".git")))) { continue }

            $branch = git -C $wtPath branch --show-current 2>$null
            $isDirty = $null -ne (git -C $wtPath status --porcelain 2>$null)

            # Merged check via merge-base --is-ancestor
            $isMerged = $false
            if ($branch -and (Test-Path $baseRepoDir)) {
                git -C $baseRepoDir merge-base --is-ancestor $branch $baseBranch 2>$null
                if ($LASTEXITCODE -eq 0) { $isMerged = $true }
            }

            # Ahead/behind vs base branch
            $ahead = 0; $behind = 0
            if ($branch) {
                $aheadRaw = git -C $wtPath rev-list --count "origin/${baseBranch}..${branch}" 2>$null
                $behindRaw = git -C $wtPath rev-list --count "${branch}..origin/${baseBranch}" 2>$null
                if ($aheadRaw) { $ahead = [int]$aheadRaw }
                if ($behindRaw) { $behind = [int]$behindRaw }
            }

            # Age from last commit
            $lastCommitRaw = git -C $wtPath log -1 --format=%ci 2>$null
            $lastModified = if ($lastCommitRaw) { [DateTime]::Parse($lastCommitRaw) } else { $taskDir.LastWriteTime }
            $age = [math]::Max(0, ((Get-Date) - $lastModified).Days)

            $worktrees += [PSCustomObject]@{
                Repo     = $repoName
                DirName  = $taskDir.Name
                Branch   = $branch
                Path     = $wtPath
                Dirty    = $isDirty
                Merged   = $isMerged
                Ahead    = $ahead
                Behind   = $behind
                Age      = $age
            }
        }
    }
    return $worktrees
}

# ---------------------------------------------------------------------------
# fzf helper
# ---------------------------------------------------------------------------

function Invoke-Fzf {
    param(
        [Parameter(Mandatory)][array]$Items,
        [Parameter(Mandatory)][scriptblock]$DisplayFn,
        [string]$Query = "",
        [string]$Preview = "",
        [switch]$Multi,
        [string]$Header = ""
    )

    if (-not (Get-Command fzf -ErrorAction SilentlyContinue)) {
        throw "fzf is required. Install: winget install junegunn.fzf"
    }

    $lines = @()
    for ($i = 0; $i -lt $Items.Count; $i++) {
        $display = & $DisplayFn $Items[$i]
        $lines += "$i`t$display"
    }

    $fzfArgs = @("--with-nth=2..", "--ansi", "--no-sort", "--reverse")
    if ($Query) { $fzfArgs += "--query=$Query" }
    if ($Multi) { $fzfArgs += "--multi" }
    if ($Header) { $fzfArgs += "--header=$Header" }
    if ($Preview) { $fzfArgs += "--preview=$Preview" }

    $selected = $lines | fzf @fzfArgs
    if (-not $selected) { return $null }

    $results = @()
    foreach ($line in ($selected -split "`n")) {
        $idx = [int]($line -split "`t")[0]
        $results += $Items[$idx]
    }
    if ($Multi) { return $results }
    return $results[0]
}

# ---------------------------------------------------------------------------
# Worktree display formatter
# ---------------------------------------------------------------------------

function Format-WorktreeDisplay {
    param([PSCustomObject]$Wt)

    $flags = @()
    if ($Wt.Dirty) { $flags += "`e[31m*`e[0m" }       # red
    if ($Wt.Merged) { $flags += "`e[32m=`e[0m" }       # green
    if ($Wt.Ahead -gt 0) { $flags += "`e[33m+$($Wt.Ahead)`e[0m" }  # yellow
    if ($Wt.Behind -gt 0) { $flags += "`e[33m-$($Wt.Behind)`e[0m" }
    $flagStr = if ($flags.Count -gt 0) { " [" + ($flags -join " ") + "]" } else { "" }

    $ageStr = if ($Wt.Age -gt 14) { "`e[90m($($Wt.Age)d)`e[0m" } else { "($($Wt.Age)d)" }
    $staleTag = if ($Wt.Age -gt 14) { " `e[90mstale`e[0m" } else { "" }

    return "$($Wt.Repo)/$($Wt.DirName)${flagStr} ${ageStr}${staleTag}"
}

# ---------------------------------------------------------------------------
# hack REPO/BRANCH — create worktree
# ---------------------------------------------------------------------------

function Invoke-HackCreate {
    param([Parameter(Mandatory)][string]$Spec)

    $config = Require-HackConfig

    # Parse repo/branch-slug
    if ($Spec -match '^([^/]+)/(.+)$') {
        $repoAlias = $matches[1]
        $slug = $matches[2]
    } else {
        throw "Expected format: hack REPO/BRANCH-SLUG"
    }

    $resolved = Resolve-Repo -Alias $repoAlias -Config $config
    $prefix = $config.branchPrefix
    $branchName = if ($prefix) { "$prefix/$slug" } else { $slug }

    # Check if branch exists on remote
    $baseRepoDir = Ensure-BaseClone -Alias $resolved.Alias -Url $resolved.Url `
        -BaseBranch $resolved.BaseBranch -Config $config
    $remoteMatch = git -C $baseRepoDir ls-remote --heads origin $branchName 2>$null
    $fromExisting = $null -ne $remoteMatch -and $remoteMatch -ne ""

    if ($fromExisting) {
        Write-Host "Branch '$branchName' exists on remote, checking out..." -ForegroundColor Cyan
    }

    $dir = New-HackWorktree -Alias $resolved.Alias -BranchName $branchName `
        -DirName $slug -Config $config -FromExisting:$fromExisting

    Set-Location $dir
}

# ---------------------------------------------------------------------------
# hack go [filter] — switch worktree via fzf
# ---------------------------------------------------------------------------

function Invoke-HackGo {
    param([string]$Filter)

    $worktrees = @(Get-HackWorktrees)
    if ($worktrees.Count -eq 0) {
        Write-Host "No worktrees found." -ForegroundColor Yellow
        return
    }

    # If filter matches exactly one, jump directly
    if ($Filter) {
        $matching = @($worktrees | Where-Object {
            "$($_.Repo)/$($_.DirName)" -like "*$Filter*"
        })
        if ($matching.Count -eq 1) {
            Write-Host "Going to: $($matching[0].Repo)/$($matching[0].DirName)" -ForegroundColor Green
            Set-Location $matching[0].Path
            return
        }
    }

    $config = Get-HackConfig
    $previewCmd = "git -C {1} log --oneline -10 2>nul"

    # Build preview-friendly lines: index\trepo/dir\tpath
    $lines = @()
    for ($i = 0; $i -lt $worktrees.Count; $i++) {
        $wt = $worktrees[$i]
        $display = Format-WorktreeDisplay $wt
        $lines += "$i`t$display`t$($wt.Path)"
    }

    $fzfArgs = @("--with-nth=2", "--delimiter=\t", "--ansi", "--no-sort", "--reverse")
    if ($Filter) { $fzfArgs += "--query=$Filter" }
    $fzfArgs += "--preview=git -C {3} log --oneline -10 2>nul || echo No commits"
    $fzfArgs += "--header=Select worktree (enter to cd)"

    $selected = $lines | fzf @fzfArgs
    if (-not $selected) { return }

    $idx = [int]($selected -split "`t")[0]
    $wt = $worktrees[$idx]
    Write-Host "Going to: $($wt.Repo)/$($wt.DirName)" -ForegroundColor Green
    Set-Location $wt.Path
}

# ---------------------------------------------------------------------------
# hack list [repo] — actionable dashboard
# ---------------------------------------------------------------------------

function Invoke-HackList {
    param([string]$Repo = "all")
    if ([string]::IsNullOrWhiteSpace($Repo)) { $Repo = "all" }

    $worktrees = @(Get-HackWorktrees -Repo $Repo)
    if ($worktrees.Count -eq 0) {
        Write-Host "No worktrees found." -ForegroundColor Yellow
        return
    }

    $grouped = $worktrees | Group-Object Repo
    foreach ($group in $grouped) {
        Write-Host "`n  $($group.Name)" -ForegroundColor Cyan

        foreach ($wt in ($group.Group | Sort-Object { $_.Age })) {
            $statusParts = @()
            if ($wt.Dirty) { $statusParts += "M" }
            if ($wt.Merged) { $statusParts += "merged" }
            if ($wt.Ahead -gt 0) { $statusParts += "+$($wt.Ahead)" }
            if ($wt.Behind -gt 0) { $statusParts += "-$($wt.Behind)" }
            $statusStr = if ($statusParts.Count -gt 0) { " [" + ($statusParts -join " ") + "]" } else { "" }

            $color = if ($wt.Merged) { "DarkGreen" }
                     elseif ($wt.Dirty) { "Red" }
                     elseif ($wt.Age -gt 14) { "DarkGray" }
                     else { "White" }

            Write-Host "    " -NoNewline
            Write-Host "$($wt.DirName)" -ForegroundColor $color -NoNewline
            Write-Host "$statusStr " -ForegroundColor $color -NoNewline
            Write-Host "($($wt.Age)d)" -ForegroundColor Gray -NoNewline
            if ($wt.Branch -and $wt.Branch -ne $wt.DirName) {
                Write-Host " $($wt.Branch)" -ForegroundColor DarkGray
            } else { Write-Host "" }
        }
    }

    # Summary
    $mergedCount = ($worktrees | Where-Object { $_.Merged }).Count
    $staleCount = ($worktrees | Where-Object { $_.Age -gt 14 -and -not $_.Merged }).Count
    Write-Host ""
    Write-Host "  $($worktrees.Count) worktrees" -ForegroundColor Cyan
    if ($mergedCount -gt 0) { Write-Host "  $mergedCount merged (run ``hack clean``)" -ForegroundColor DarkGreen }
    if ($staleCount -gt 0) { Write-Host "  $staleCount stale (>14d)" -ForegroundColor DarkGray }
}

# ---------------------------------------------------------------------------
# hack clean [repo] — cleanup merged + fzf multi-select
# ---------------------------------------------------------------------------

function Invoke-HackClean {
    param([string]$Repo = "all")
    if ([string]::IsNullOrWhiteSpace($Repo)) { $Repo = "all" }

    $config = Require-HackConfig
    $worktrees = @(Get-HackWorktrees -Repo $Repo)
    if ($worktrees.Count -eq 0) {
        Write-Host "No worktrees found." -ForegroundColor Yellow
        return
    }

    # Phase 1: auto-delete merged worktrees (with confirmation)
    $merged = @($worktrees | Where-Object { $_.Merged })
    if ($merged.Count -gt 0) {
        $confirm = Read-Host "Delete $($merged.Count) merged worktree(s)? [y/N]"
        if ($confirm -eq 'y' -or $confirm -eq 'Y') {
            foreach ($wt in $merged) { Remove-HackWorktree -Wt $wt -Config $config }
            $worktrees = @($worktrees | Where-Object { -not $_.Merged })
        }
    }

    # Phase 2: fzf multi-select for remaining
    if ($worktrees.Count -eq 0) {
        Write-Host "All clean." -ForegroundColor Green
        return
    }

    $displayFn = { param($wt) Format-WorktreeDisplay $wt }
    try {
        $toRemove = Invoke-Fzf -Items $worktrees -DisplayFn $displayFn `
            -Multi -Header "Select worktrees to remove (tab to multi-select, enter to confirm)"
    } catch {
        # fzf not available — skip interactive phase
        return
    }

    if (-not $toRemove) { return }
    $removeList = @($toRemove)

    $dirtyOnes = @($removeList | Where-Object { $_.Dirty })
    if ($dirtyOnes.Count -gt 0) {
        $confirm = Read-Host "$($dirtyOnes.Count) have uncommitted changes. Continue? [y/N]"
        if ($confirm -ne 'y' -and $confirm -ne 'Y') { return }
    }

    foreach ($wt in $removeList) { Remove-HackWorktree -Wt $wt -Config $config }
}

function Remove-HackWorktree {
    param([Parameter(Mandatory)][PSCustomObject]$Wt, [Parameter(Mandatory)][object]$Config)

    Write-Host "Removing $($Wt.Repo)/$($Wt.DirName)..." -ForegroundColor Cyan

    # Step out if we're inside the worktree being deleted
    if ((Get-Location).Path.StartsWith($Wt.Path)) { Set-Location $Config.baseDir }

    $baseRepoDir = Get-BaseRepoDir -Alias $Wt.Repo -Config $Config
    if (Test-Path $baseRepoDir) {
        git -C $baseRepoDir worktree remove $Wt.Path --force 2>&1 | Out-Null
        git -C $baseRepoDir worktree prune 2>&1 | Out-Null
        # Delete local branch
        if ($Wt.Branch) {
            git -C $baseRepoDir branch -D $Wt.Branch 2>&1 | Out-Null
        }
    }

    # Fallback removal if git worktree remove didn't fully clean up
    if (Test-Path $Wt.Path) {
        Remove-Item -Path $Wt.Path -Recurse -Force -ErrorAction SilentlyContinue
    }

    Write-Host "Removed." -ForegroundColor Green
}

# ---------------------------------------------------------------------------
# workshop — repo config
# ---------------------------------------------------------------------------

function workshop {
    param(
        [Parameter(Position=0)][string]$Action,
        [Parameter(Position=1)][string]$AliasName,
        [Parameter(Position=2)][string]$Url,
        [Parameter(Position=3)][string]$BaseBranch
    )

    if ([string]::IsNullOrWhiteSpace($Action)) {
        $config = Get-HackConfig
        if (-not $config -or -not $config.repos -or $config.repos.PSObject.Properties.Count -eq 0) {
            Write-Host "No repos registered. Run: workshop add ALIAS URL" -ForegroundColor Yellow
            return
        }
        Write-Host "`nRegistered repos:" -ForegroundColor Cyan
        Write-Host ""
        foreach ($prop in $config.repos.PSObject.Properties) {
            $repo = $prop.Value
            $base = Coalesce $repo.baseBranch $config.defaultBaseBranch "develop"
            $defaultTag = if ($config.defaultRepo -eq $prop.Name) { " (default)" } else { "" }
            Write-Host "  $($prop.Name)" -ForegroundColor White -NoNewline
            Write-Host "$defaultTag" -ForegroundColor Green -NoNewline
            Write-Host " — $($repo.url) [$base]" -ForegroundColor Gray
        }
        Write-Host ""
        Write-Host "Base dir: $($config.baseDir)" -ForegroundColor DarkGray
        Write-Host "Branch prefix: $($config.branchPrefix)" -ForegroundColor DarkGray
        return
    }

    switch ($Action) {
        "add" {
            if ([string]::IsNullOrWhiteSpace($AliasName) -or [string]::IsNullOrWhiteSpace($Url)) {
                Write-Host "Usage: workshop add ALIAS URL [baseBranch]" -ForegroundColor Yellow
                return
            }
            $config = Get-HackConfig
            if (-not $config) { $config = Initialize-HackConfig }
            $branch = if ([string]::IsNullOrWhiteSpace($BaseBranch)) { Coalesce $config.defaultBaseBranch "develop" } else { $BaseBranch }
            $config.repos | Add-Member -NotePropertyName $AliasName -NotePropertyValue ([ordered]@{
                url        = $Url
                baseBranch = $branch
            }) -Force
            if ($config.repos.PSObject.Properties.Count -eq 1 -or -not $config.defaultRepo) {
                $config | Add-Member -NotePropertyName "defaultRepo" -NotePropertyValue $AliasName -Force
            }
            Save-HackConfig $config
            Write-Host "Registered '$AliasName' (base: $branch)" -ForegroundColor Green
        }
        "remove" {
            if ([string]::IsNullOrWhiteSpace($AliasName)) {
                Write-Host "Usage: workshop remove ALIAS" -ForegroundColor Yellow; return
            }
            $config = Get-HackConfig
            if (-not $config) { Write-Host "No config found." -ForegroundColor Yellow; return }
            if (-not ($config.repos.PSObject.Properties.Name -contains $AliasName)) {
                Write-Host "Unknown repo '$AliasName'." -ForegroundColor Red; return
            }
            $config.repos.PSObject.Properties.Remove($AliasName)
            if ($config.defaultRepo -eq $AliasName) {
                $first = $config.repos.PSObject.Properties | Select-Object -First 1
                $config.defaultRepo = if ($first) { $first.Name } else { $null }
            }
            Save-HackConfig $config
            Write-Host "Removed '$AliasName'." -ForegroundColor Green
        }
        "default" {
            if ([string]::IsNullOrWhiteSpace($AliasName)) {
                Write-Host "Usage: workshop default ALIAS" -ForegroundColor Yellow; return
            }
            $config = Get-HackConfig
            if (-not $config) { Write-Host "No config found." -ForegroundColor Yellow; return }
            if (-not ($config.repos.PSObject.Properties.Name -contains $AliasName)) {
                Write-Host "Unknown repo '$AliasName'." -ForegroundColor Red; return
            }
            $config | Add-Member -NotePropertyName "defaultRepo" -NotePropertyValue $AliasName -Force
            Save-HackConfig $config
            Write-Host "Default repo set to '$AliasName'." -ForegroundColor Green
        }
        default {
            Write-Host "Usage: workshop [add|remove|default]" -ForegroundColor Yellow
            Write-Host "  workshop              — list repos"
            Write-Host "  workshop add ALIAS URL [baseBranch]"
            Write-Host "  workshop remove ALIAS"
            Write-Host "  workshop default ALIAS"
        }
    }
}

# ---------------------------------------------------------------------------
# hack — dispatcher
# ---------------------------------------------------------------------------

function hack {
    param(
        [Parameter(Position=0)]
        [ArgumentCompleter({
            param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
            $subs = @("go", "list", "clean")
            $cfg = Get-HackConfig
            $repos = @()
            if ($cfg -and $cfg.repos) {
                $repos = @($cfg.repos.PSObject.Properties.Name)
                # repo/branch completion
                foreach ($r in $repos) {
                    if ($wordToComplete -match "^$r/") {
                        $baseDir = Get-BaseRepoDir -Alias $r -Config $cfg
                        if (Test-Path $baseDir) {
                            $branches = git -C $baseDir ls-remote --heads origin 2>$null |
                                ForEach-Object { ($_ -split '\t')[1] -replace '^refs/heads/', '' } |
                                Where-Object { $_ -like "$wordToComplete*" -or "$r/$_" -like "$wordToComplete*" }
                            foreach ($b in $branches) {
                                $label = "$r/$b"
                                [System.Management.Automation.CompletionResult]::new($label, $label, 'ParameterValue', $label)
                            }
                        }
                        return
                    }
                }
                # repo/ prefix suggestions
                $repos = $repos | ForEach-Object { "$_/" }
            }
            ($subs + $repos) | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
                [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
            }
        })]
        [string]$Subcommand,

        [Parameter(Position=1)][string]$Arg1
    )

    try {
        switch ($Subcommand) {
            ""      { Write-Host "Usage: hack REPO/BRANCH | hack go | hack list | hack clean" -ForegroundColor Yellow }
            "go"    { Invoke-HackGo -Filter $Arg1 }
            "list"  { Invoke-HackList -Repo $Arg1 }
            "clean" { Invoke-HackClean -Repo $Arg1 }
            default {
                # Treat as repo/branch create spec
                if ($Subcommand -match '/') {
                    Invoke-HackCreate -Spec $Subcommand
                } else {
                    Write-Host "Unknown command '$Subcommand'. Did you mean: hack $Subcommand/<branch-slug>?" -ForegroundColor Yellow
                }
            }
        }
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# Get-HackPrompt — for shell prompt integration
# ---------------------------------------------------------------------------

function Get-HackPrompt {
    $config = Get-HackConfig
    if (-not $config) { return "" }
    $repoAlias = Resolve-RepoFromPwd -Config $config
    if (-not $repoAlias) { return "" }
    $pwd = (Get-Location).Path
    $relative = $pwd.Substring($config.baseDir.Length).TrimStart([IO.Path]::DirectorySeparatorChar, '/')
    $parts = $relative -split '[/\\]'
    if ($parts.Count -ge 2) { return "$($parts[0])/$($parts[1])" }
    return ""
}

# ---------------------------------------------------------------------------
# Aliases
# ---------------------------------------------------------------------------

function goto-hack { hack go @args }
function list-hacks { hack list @args }

Set-Alias -Name g -Value goto-hack
Set-Alias -Name lh -Value list-hacks
