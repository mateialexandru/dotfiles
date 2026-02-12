# hack.ps1 — Worktree-based developer workflow tooling (v2)
# Dot-source this file in your PowerShell profile:
#   . "<dotfiles>/shell/hack.ps1"
#
# Two commands: `workshop` (repo management) and `hack` (worktree lifecycle)

# ---------------------------------------------------------------------------
# Config infrastructure
# ---------------------------------------------------------------------------

$script:HackConfigDir = if ($IsLinux -or $IsMacOS) {
    Join-Path $env:HOME ".config" "hack"
} else {
    Join-Path $env:USERPROFILE ".config" "hack"
}
$script:HackConfigPath = Join-Path $script:HackConfigDir "config.json"
$script:AgentModeFile = if ($IsLinux -or $IsMacOS) {
    Join-Path $env:HOME ".agent-mode"
} else {
    Join-Path $env:USERPROFILE ".agent-mode"
}

function Get-HackConfig {
    if (Test-Path $script:HackConfigPath) {
        try {
            return Get-Content $script:HackConfigPath -Raw | ConvertFrom-Json
        }
        catch {
            Write-Warning "Failed to parse hack config: $_"
            return $null
        }
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
    <#
    .SYNOPSIS
    First-run inline config creation. Prompts for baseDir and branchPrefix.
    #>
    $defaultBase = if ($IsLinux -or $IsMacOS) {
        Join-Path $env:HOME "worktree"
    } else {
        "C:\worktree"
    }
    $defaultPrefix = "user/$($env:USERNAME ?? $env:USER)"

    Write-Host ""
    $baseDir = Read-Host "Worktree base directory [$defaultBase]"
    if ([string]::IsNullOrWhiteSpace($baseDir)) { $baseDir = $defaultBase }

    $branchPrefix = Read-Host "Branch prefix [$defaultPrefix]"
    if ([string]::IsNullOrWhiteSpace($branchPrefix)) { $branchPrefix = $defaultPrefix }

    $config = [ordered]@{
        baseDir           = $baseDir
        branchPrefix      = $branchPrefix
        defaultBaseBranch = "develop"
        defaultModel      = "claude-sonnet-4-5"
        defaultRepo       = $null
        repos             = [ordered]@{}
    }

    Save-HackConfig $config
    Write-Host "(Saved to $script:HackConfigPath)" -ForegroundColor Gray
    return Get-HackConfig
}

# ---------------------------------------------------------------------------
# Agent mode helpers
# ---------------------------------------------------------------------------

function Get-AgentMode {
    if (Test-Path $script:AgentModeFile) {
        $mode = (Get-Content $script:AgentModeFile -Raw).Trim()
        if ($mode -eq "copilot") { return "copilot" }
    }
    return "claude"
}

function Set-AgentMode {
    param([ValidateSet("claude","copilot")][string]$Mode)
    Set-Content -Path $script:AgentModeFile -Value $Mode -NoNewline
}

function Get-AgentCommand {
    param([string]$Prompt, [string]$Model, [switch]$Resume, [switch]$PlanMode)
    $mode = Get-AgentMode

    if ($mode -eq "copilot") {
        if ([string]::IsNullOrWhiteSpace($Prompt)) {
            return "copilot --yolo"
        } else {
            $escaped = $Prompt -replace '"','\"'
            return "copilot --yolo -i `"$escaped`""
        }
    } else {
        $parts = @("claude")
        if ($Model) { $parts += "--model", $Model }
        if ($Resume) {
            $parts += "--resume"
        } elseif ($PlanMode) {
            $parts += "--permission-mode", "plan"
        }
        if (-not [string]::IsNullOrWhiteSpace($Prompt) -and -not $Resume) {
            $escaped = $Prompt -replace '"','\"'
            $parts += "-p", "`"$escaped`""
        }
        return $parts -join " "
    }
}

# ---------------------------------------------------------------------------
# Slug helper
# ---------------------------------------------------------------------------

function Get-SlugFromDescription {
    param([Parameter(Mandatory)][string]$Description)
    $slug = $Description.ToLower() -replace '[^a-z0-9]+', '-' -replace '(^-|-$)', ''
    if ([string]::IsNullOrWhiteSpace($slug)) {
        throw "Could not generate valid name from description."
    }
    if ($slug.Length -gt 60) { $slug = $slug.Substring(0, 60) -replace '-$', '' }
    return $slug
}

# ---------------------------------------------------------------------------
# Worktree core helpers
# ---------------------------------------------------------------------------

function Get-TreesDir {
    param([object]$Config)
    return [IO.Path]::Combine($Config.baseDir, ".trees")
}

function Ensure-BaseClone {
    param(
        [Parameter(Mandatory)][string]$Alias,
        [Parameter(Mandatory)][string]$Url,
        [Parameter(Mandatory)][string]$BaseBranch,
        [Parameter(Mandatory)][object]$Config
    )

    $treesDir = Get-TreesDir -Config $Config
    $baseRepoDir = [IO.Path]::Combine($treesDir, $Alias)

    if (-not (Test-Path $baseRepoDir)) {
        Write-Host "Creating base clone for '$Alias'..." -ForegroundColor Cyan
        New-Item -ItemType Directory -Path $treesDir -Force | Out-Null
        git clone --branch $BaseBranch $Url $baseRepoDir | Out-Host
        if ($LASTEXITCODE -ne 0) { throw "Failed to clone repository." }
    } else {
        Write-Host "Pulling latest $BaseBranch..." -ForegroundColor Cyan
        Push-Location $baseRepoDir
        try {
            git fetch origin "${BaseBranch}:${BaseBranch}" --force 2>$null
        }
        finally { Pop-Location }
    }

    return $baseRepoDir
}

function New-HackWorktree {
    param(
        [Parameter(Mandatory)][string]$Alias,
        [Parameter(Mandatory)][string]$Url,
        [Parameter(Mandatory)][string]$BaseBranch,
        [Parameter(Mandatory)][string]$BranchName,
        [Parameter(Mandatory)][string]$WorktreeName,
        [Parameter(Mandatory)][object]$Config
    )

    $baseRepoDir = Ensure-BaseClone -Alias $Alias -Url $Url -BaseBranch $BaseBranch -Config $Config
    $worktreeDir = [IO.Path]::Combine($Config.baseDir, $Alias, $WorktreeName)

    if (Test-Path $worktreeDir) { throw "Worktree '$worktreeDir' already exists." }

    Write-Host "Creating worktree '$WorktreeName' on branch '$BranchName'..." -ForegroundColor Cyan
    $parentDir = [IO.Path]::Combine($Config.baseDir, $Alias)
    New-Item -ItemType Directory -Path $parentDir -Force | Out-Null

    Push-Location $baseRepoDir
    try {
        git worktree add -b $BranchName $worktreeDir $BaseBranch | Out-Host
        if ($LASTEXITCODE -ne 0) { throw "Failed to create worktree." }
    }
    finally { Pop-Location }

    Write-Host "Worktree created at '$worktreeDir'" -ForegroundColor Green
    return $worktreeDir
}

function New-HackWorktreeFromBranch {
    param(
        [Parameter(Mandatory)][string]$Alias,
        [Parameter(Mandatory)][string]$Url,
        [Parameter(Mandatory)][string]$BaseBranch,
        [Parameter(Mandatory)][string]$BranchName,
        [Parameter(Mandatory)][string]$WorktreeName,
        [Parameter(Mandatory)][object]$Config
    )

    $baseRepoDir = Ensure-BaseClone -Alias $Alias -Url $Url -BaseBranch $BaseBranch -Config $Config
    $worktreeDir = [IO.Path]::Combine($Config.baseDir, $Alias, $WorktreeName)

    if (Test-Path $worktreeDir) { throw "Worktree '$worktreeDir' already exists." }

    $parentDir = [IO.Path]::Combine($Config.baseDir, $Alias)
    New-Item -ItemType Directory -Path $parentDir -Force | Out-Null

    Push-Location $baseRepoDir
    try {
        git fetch origin "${BranchName}:${BranchName}" 2>&1 | Out-Null
        git worktree add $worktreeDir $BranchName | Out-Host
        if ($LASTEXITCODE -ne 0) { throw "Failed to create worktree from branch '$BranchName'." }
    }
    finally { Pop-Location }

    Write-Host "Worktree created at '$worktreeDir'" -ForegroundColor Green
    return $worktreeDir
}

# ---------------------------------------------------------------------------
# SPEC.md generation
# ---------------------------------------------------------------------------

function New-SpecFile {
    param(
        [Parameter(Mandatory)][string]$WorktreeDir,
        [Parameter(Mandatory)][string]$Task,
        [Parameter(Mandatory)][string]$BranchName,
        [string]$RepoAlias,
        [string]$Model
    )

    $timestamp = (Get-Date).ToUniversalTime().ToString("yyyy-MM-ddTHH:mm:ssZ")
    if (-not $Model) { $Model = "claude-sonnet-4-5" }

    $spec = @"
---
task: $Task
branch: $BranchName
created: $timestamp
model: $Model
repo: $RepoAlias
---
"@

    $specPath = [IO.Path]::Combine($WorktreeDir, "SPEC.md")
    Set-Content -Path $specPath -Value $spec -Encoding UTF8
    return $specPath
}

function Get-SpecData {
    param([Parameter(Mandatory)][string]$WorktreeDir)

    $specPath = [IO.Path]::Combine($WorktreeDir, "SPEC.md")
    if (-not (Test-Path $specPath)) { return $null }

    try {
        $content = Get-Content $specPath -Raw -ErrorAction Stop
        $data = @{}
        if ($content -match '(?s)^---\s*\r?\n(.*?)\r?\n---') {
            $yaml = $matches[1]
            foreach ($line in ($yaml -split '\r?\n')) {
                if ($line -match '^(\w[\w-]*):\s*(.+)$') {
                    $data[$matches[1]] = $matches[2].Trim()
                }
            }
        }
        return $data
    }
    catch { return $null }
}

function Update-SpecField {
    param(
        [Parameter(Mandatory)][string]$WorktreeDir,
        [Parameter(Mandatory)][string]$Field,
        [Parameter(Mandatory)][string]$Value
    )

    $specPath = [IO.Path]::Combine($WorktreeDir, "SPEC.md")
    if (-not (Test-Path $specPath)) { return }

    $content = Get-Content $specPath -Raw
    if ($content -match "(?m)^${Field}:\s*.+$") {
        $content = $content -replace "(?m)^${Field}:\s*.+$", "${Field}: $Value"
    }
    elseif ($content -match '(?s)(^---\s*\r?\n)(.*?)(\r?\n---)') {
        $content = $content -replace '(?s)(^---\s*\r?\n)(.*?)(\r?\n---)', "`$1`$2`n${Field}: $Value`$3"
    }
    Set-Content -Path $specPath -Value $content -Encoding UTF8
}

# ---------------------------------------------------------------------------
# Resolve-RepoFromPwd — infer repo alias from current directory
# ---------------------------------------------------------------------------

function Resolve-RepoFromPwd {
    param([object]$Config)

    $pwd = (Get-Location).Path
    $baseDir = $Config.baseDir
    if (-not $pwd.StartsWith($baseDir)) { return $null }

    $relative = $pwd.Substring($baseDir.Length).TrimStart([IO.Path]::DirectorySeparatorChar, '/')
    $parts = $relative -split '[/\\]'
    if ($parts.Count -ge 1) {
        $candidate = $parts[0]
        if ($candidate -ne ".trees" -and $Config.repos.PSObject.Properties.Name -contains $candidate) {
            return $candidate
        }
    }
    return $null
}

# ---------------------------------------------------------------------------
# Resolve-HackRepo — alias-only resolution (no URLs)
# ---------------------------------------------------------------------------

function Resolve-HackRepo {
    param(
        [Parameter(Mandatory)][string]$RepoAlias,
        [object]$Config
    )

    if (-not ($Config.repos.PSObject.Properties.Name -contains $RepoAlias)) {
        throw "Unknown repo '$RepoAlias'. Run 'workshop add <alias> <url>' first."
    }

    $repo = $Config.repos.$RepoAlias
    $result = @{
        Url        = $repo.url
        Alias      = $RepoAlias
        BaseBranch = $repo.baseBranch ?? $Config.defaultBaseBranch ?? "develop"
        Provider   = "unknown"
    }

    if ($result.Url -match 'github\.com') { $result.Provider = "github" }
    elseif ($result.Url -match 'visualstudio\.com|dev\.azure\.com') { $result.Provider = "ado" }
    elseif ($result.Url -match 'gitlab\.com') { $result.Provider = "gitlab" }

    return $result
}

# ---------------------------------------------------------------------------
# Tab-completion: worktree names (repo/task)
# ---------------------------------------------------------------------------

$script:WorktreeCompleter = {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    $cfg = Get-HackConfig
    if (-not $cfg -or -not $cfg.baseDir -or -not (Test-Path $cfg.baseDir)) { return }

    $repoDirs = Get-ChildItem $cfg.baseDir -Directory -ErrorAction SilentlyContinue | Where-Object { $_.Name -ne ".trees" }
    foreach ($repoDir in $repoDirs) {
        $taskDirs = Get-ChildItem $repoDir.FullName -Directory -ErrorAction SilentlyContinue
        foreach ($taskDir in $taskDirs) {
            if (Test-Path ([IO.Path]::Combine($taskDir.FullName, ".git"))) {
                $label = "$($repoDir.Name)/$($taskDir.Name)"
                if ($label -like "$wordToComplete*") {
                    [System.Management.Automation.CompletionResult]::new($label, $label, 'ParameterValue', $label)
                }
            }
        }
    }
}

$script:RepoCompleter = {
    param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
    $cfg = Get-HackConfig
    if ($cfg -and $cfg.repos) {
        $cfg.repos.PSObject.Properties.Name | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
            [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
        }
    }
}

# ---------------------------------------------------------------------------
# Get-HackWorktrees — worktree enumeration with rich status
# ---------------------------------------------------------------------------

function Get-HackWorktrees {
    param(
        [string]$Repo = "all",
        [ValidateSet("all","clean","dirty","ahead","behind","stale","merged")][string]$Status = "all",
        [string]$Filter = "",
        [int]$DaysOld = 0
    )

    $config = Get-HackConfig
    if (-not $config) { return @() }

    $baseDir = $config.baseDir
    if (-not (Test-Path $baseDir)) { return @() }

    $worktrees = @()
    $repoDirs = Get-ChildItem $baseDir -Directory | Where-Object { $_.Name -ne ".trees" }

    foreach ($repoDir in $repoDirs) {
        $repoName = $repoDir.Name
        if ($Repo -ne "all" -and $repoName -ne $Repo) { continue }

        $taskDirs = Get-ChildItem $repoDir.FullName -Directory -ErrorAction SilentlyContinue

        foreach ($taskDir in $taskDirs) {
            $worktreePath = $taskDir.FullName

            if (-not (Test-Path ([IO.Path]::Combine($worktreePath, ".git")))) { continue }

            $spec = Get-SpecData -WorktreeDir $worktreePath
            $taskDesc = if ($spec -and $spec["task"]) { $spec["task"] } else { "" }
            $model = if ($spec -and $spec["model"]) { $spec["model"] } else { "" }
            $prUrl = if ($spec -and $spec["pr"]) { $spec["pr"] } else { "" }
            $created = $null
            if ($spec -and $spec["created"]) {
                try { $created = [DateTime]::Parse($spec["created"]) } catch { }
            }

            $isScratch = $taskDir.Name -match '^_scratch-'

            $originalLocation = Get-Location
            try {
                Set-Location $worktreePath

                $gitStatus = git status --porcelain 2>&1
                $isDirty = ($null -ne $gitStatus -and $gitStatus.Count -gt 0 -and $gitStatus[0] -notmatch 'fatal|error')

                $branch = git branch --show-current 2>$null

                # Check merged status
                $isMerged = $false
                $repoBaseBranch = $config.defaultBaseBranch ?? "develop"
                if ($config.repos.PSObject.Properties.Name -contains $repoName) {
                    $repoConf = $config.repos.$repoName
                    if ($repoConf.baseBranch) { $repoBaseBranch = $repoConf.baseBranch }
                }
                $treesDir = Get-TreesDir -Config $config
                $baseRepoDir = [IO.Path]::Combine($treesDir, $repoName)
                if ($branch -and (Test-Path $baseRepoDir)) {
                    Push-Location $baseRepoDir
                    try {
                        $mergedBranches = git branch --merged $repoBaseBranch 2>$null
                        if ($mergedBranches -and ($mergedBranches | Where-Object { $_.Trim() -eq $branch })) {
                            $isMerged = $true
                        }
                    }
                    catch { }
                    finally { Pop-Location }
                }

                # Ahead/behind
                $ahead = 0; $behind = 0
                if ($branch -and $branch -ne "HEAD") {
                    $remoteBranch = "origin/$repoBaseBranch"
                    $remoteExists = git rev-parse --verify $remoteBranch 2>$null
                    if ($remoteExists) {
                        $aheadRaw = git rev-list --count "${remoteBranch}..${branch}" 2>$null
                        $behindRaw = git rev-list --count "${branch}..${remoteBranch}" 2>$null
                        if ($aheadRaw) { $ahead = [int]$aheadRaw }
                        if ($behindRaw) { $behind = [int]$behindRaw }
                    }
                }

                $lastCommitRaw = git log -1 --format=%ci 2>$null
                $lastModified = if ($lastCommitRaw) {
                    [DateTime]::Parse($lastCommitRaw)
                } else { $taskDir.LastWriteTime }

                $statusFlag = if ($isMerged) { "merged" }
                              elseif ($isDirty) { "dirty" }
                              elseif ($ahead -gt 0) { "ahead" }
                              elseif ($behind -gt 0) { "behind" }
                              else { "clean" }

                $daysSinceModified = ((Get-Date) - $lastModified).Days
                if ($daysSinceModified -gt 30 -and $statusFlag -eq "clean") {
                    $statusFlag = "stale"
                }

                # Filters
                if ($Status -ne "all" -and $statusFlag -ne $Status) {
                    Set-Location $originalLocation; continue
                }
                if ($DaysOld -gt 0 -and $daysSinceModified -lt $DaysOld) {
                    Set-Location $originalLocation; continue
                }
                if (-not [string]::IsNullOrWhiteSpace($Filter)) {
                    $searchText = "$repoName/$($taskDir.Name) $taskDesc"
                    if ($searchText -notlike "*$Filter*") {
                        Set-Location $originalLocation; continue
                    }
                }

                $ageInDays = if ($created) { ((Get-Date) - $created).Days } else { $daysSinceModified }

                $worktrees += [PSCustomObject]@{
                    Repo         = $repoName
                    Task         = $taskDir.Name
                    Description  = $taskDesc
                    Status       = $statusFlag
                    Merged       = $isMerged
                    Dirty        = $isDirty
                    Ahead        = $ahead
                    Behind       = $behind
                    Model        = $model
                    PrUrl        = $prUrl
                    Age          = $ageInDays
                    LastModified = $lastModified
                    Branch       = $branch
                    Path         = $worktreePath
                    IsScratch    = $isScratch
                }
            }
            catch { }
            finally { Set-Location $originalLocation }
        }
    }

    return $worktrees
}

# ---------------------------------------------------------------------------
# Picker UI (shared by resume, go, done, clean)
# ---------------------------------------------------------------------------

function Show-WorktreePicker {
    param(
        [Parameter(Mandatory)][array]$Worktrees,
        [string]$Title = "Select worktree"
    )

    $sorted = @($Worktrees | Sort-Object LastModified -Descending)

    if ($sorted.Count -eq 0) {
        Write-Host "No worktrees found." -ForegroundColor Yellow
        return $null
    }

    if ($sorted.Count -eq 1) {
        return $sorted[0]
    }

    Write-Host "`n${Title}:" -ForegroundColor Cyan
    Write-Host ""

    $idx = 1
    foreach ($wt in $sorted) {
        $color = switch ($wt.Status) {
            "dirty"  { "Red" }
            "ahead"  { "Yellow" }
            "behind" { "Yellow" }
            "stale"  { "DarkGray" }
            "merged" { "DarkGreen" }
            "clean"  { "Green" }
            default  { "White" }
        }
        $symbol = switch ($wt.Status) {
            "dirty"  { "*" }
            "ahead"  { "^" }
            "behind" { "v" }
            "stale"  { "~" }
            "merged" { "=" }
            "clean"  { " " }
            default  { "?" }
        }

        $gitInfo = @()
        if ($wt.Ahead -gt 0) { $gitInfo += "+$($wt.Ahead)" }
        if ($wt.Behind -gt 0) { $gitInfo += "-$($wt.Behind)" }
        if ($wt.Dirty) { $gitInfo += "*" }
        $gitStr = if ($gitInfo.Count -gt 0) { $gitInfo -join " " } else { "sync" }

        $unnamedTag = if ($wt.IsScratch) { " [UNNAMED]" } else { "" }

        Write-Host "[$idx] " -NoNewline
        Write-Host "[$symbol] " -ForegroundColor $color -NoNewline
        Write-Host "$($wt.Repo)/$($wt.Task) " -NoNewline
        Write-Host "[$gitStr] " -ForegroundColor $color -NoNewline
        Write-Host "($($wt.Age)d)" -ForegroundColor Gray -NoNewline
        if ($unnamedTag) { Write-Host $unnamedTag -ForegroundColor DarkYellow -NoNewline }
        if ($wt.Description) { Write-Host " $($wt.Description)" -ForegroundColor Gray } else { Write-Host "" }

        $idx++
    }

    Write-Host ""
    $choice = Read-Host "Choice [1-$($sorted.Count)] or 'q'"
    if ($choice -eq 'q') { return $null }

    $choiceNum = [int]$choice
    if ($choiceNum -lt 1 -or $choiceNum -gt $sorted.Count) {
        Write-Host "Invalid choice." -ForegroundColor Red
        return $null
    }
    return $sorted[$choiceNum - 1]
}

# ---------------------------------------------------------------------------
# workshop — repo management
# ---------------------------------------------------------------------------

function workshop {
    param(
        [Parameter(Position=0)][string]$Action,
        [Parameter(Position=1)][string]$AliasName,
        [Parameter(Position=2)][string]$Url,
        [Parameter(Position=3)][string]$BaseBranch
    )

    # workshop (no args) → list repos
    if ([string]::IsNullOrWhiteSpace($Action)) {
        $config = Get-HackConfig
        if (-not $config -or -not $config.repos -or $config.repos.PSObject.Properties.Count -eq 0) {
            Write-Host "No repos registered. Run: workshop add <alias> <url>" -ForegroundColor Yellow
            return
        }

        Write-Host "`nRegistered repos:" -ForegroundColor Cyan
        Write-Host ""
        foreach ($prop in $config.repos.PSObject.Properties) {
            $repo = $prop.Value
            $base = $repo.baseBranch ?? $config.defaultBaseBranch ?? "develop"
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
                Write-Host "Usage: workshop add <alias> <url> [baseBranch]" -ForegroundColor Yellow
                return
            }

            $config = Get-HackConfig
            if (-not $config) {
                $config = Initialize-HackConfig
            }

            $branch = if ([string]::IsNullOrWhiteSpace($BaseBranch)) { $config.defaultBaseBranch ?? "develop" } else { $BaseBranch }

            $config.repos | Add-Member -NotePropertyName $AliasName -NotePropertyValue ([ordered]@{
                url        = $Url
                baseBranch = $branch
            }) -Force

            # Set as default if it's the first repo
            if ($config.repos.PSObject.Properties.Count -eq 1 -or -not $config.defaultRepo) {
                $config | Add-Member -NotePropertyName "defaultRepo" -NotePropertyValue $AliasName -Force
            }

            Save-HackConfig $config
            Write-Host "Registered '$AliasName' (base: $branch)" -ForegroundColor Green
        }
        "remove" {
            if ([string]::IsNullOrWhiteSpace($AliasName)) {
                Write-Host "Usage: workshop remove <alias>" -ForegroundColor Yellow
                return
            }

            $config = Get-HackConfig
            if (-not $config) {
                Write-Host "No config found." -ForegroundColor Yellow
                return
            }

            if (-not ($config.repos.PSObject.Properties.Name -contains $AliasName)) {
                Write-Host "Unknown repo '$AliasName'." -ForegroundColor Red
                return
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
                Write-Host "Usage: workshop default <alias>" -ForegroundColor Yellow
                return
            }

            $config = Get-HackConfig
            if (-not $config) {
                Write-Host "No config found." -ForegroundColor Yellow
                return
            }

            if (-not ($config.repos.PSObject.Properties.Name -contains $AliasName)) {
                Write-Host "Unknown repo '$AliasName'." -ForegroundColor Red
                return
            }

            $config | Add-Member -NotePropertyName "defaultRepo" -NotePropertyValue $AliasName -Force
            Save-HackConfig $config
            Write-Host "Default repo set to '$AliasName'." -ForegroundColor Green
        }
        default {
            Write-Host "Usage: workshop [add|remove|default] ..." -ForegroundColor Yellow
            Write-Host "  workshop              — list repos"
            Write-Host "  workshop add <alias> <url> [baseBranch]"
            Write-Host "  workshop remove <alias>"
            Write-Host "  workshop default <alias>"
        }
    }
}

# ---------------------------------------------------------------------------
# hack — subcommand dispatcher
# ---------------------------------------------------------------------------

function hack {
    param(
        [Parameter(Position=0)]
        [ArgumentCompleter({
            param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
            # Complete subcommands + repo aliases
            $subs = @("name","done","resume","go","list","clean","branch","status")
            $cfg = Get-HackConfig
            $repos = @()
            if ($cfg -and $cfg.repos) {
                $repos = @($cfg.repos.PSObject.Properties.Name)
            }
            ($subs + $repos) | Where-Object { $_ -like "$wordToComplete*" } | ForEach-Object {
                [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
            }
        })]
        [string]$Subcommand,

        [Parameter(Position=1)]
        [ArgumentCompleter({
            param($commandName, $parameterName, $wordToComplete, $commandAst, $fakeBoundParameters)
            # Context-dependent: for resume/go complete worktrees, for name complete nothing
            $sub = $fakeBoundParameters['Subcommand']
            if ($sub -eq 'resume' -or $sub -eq 'go') {
                $cfg = Get-HackConfig
                if (-not $cfg -or -not $cfg.baseDir -or -not (Test-Path $cfg.baseDir)) { return }
                $repoDirs = Get-ChildItem $cfg.baseDir -Directory -ErrorAction SilentlyContinue | Where-Object { $_.Name -ne ".trees" }
                foreach ($repoDir in $repoDirs) {
                    $taskDirs = Get-ChildItem $repoDir.FullName -Directory -ErrorAction SilentlyContinue
                    foreach ($taskDir in $taskDirs) {
                        if (Test-Path ([IO.Path]::Combine($taskDir.FullName, ".git"))) {
                            $label = "$($repoDir.Name)/$($taskDir.Name)"
                            if ($label -like "$wordToComplete*") {
                                [System.Management.Automation.CompletionResult]::new($label, $label, 'ParameterValue', $label)
                            }
                        }
                    }
                }
            }
        })]
        [string]$Arg1,

        [Parameter(Position=2)][string]$Arg2
    )

    $reservedSubcommands = @("name","done","resume","go","list","clean","branch","status")

    # No args → infer repo from PWD or defaultRepo, create scratch
    if ([string]::IsNullOrWhiteSpace($Subcommand)) {
        $config = Get-HackConfig
        if (-not $config) {
            Write-Host "No repos registered. Run: workshop add <alias> <url>" -ForegroundColor Yellow
            return
        }

        $repoAlias = Resolve-RepoFromPwd -Config $config
        if (-not $repoAlias) { $repoAlias = $config.defaultRepo }
        if (-not $repoAlias) {
            Write-Host "No repo found. Specify: hack <repo> or set a defaultRepo." -ForegroundColor Yellow
            Write-Host "Registered repos:" -ForegroundColor Gray
            $config.repos.PSObject.Properties | ForEach-Object { Write-Host "  $($_.Name)" -ForegroundColor Gray }
            return
        }

        Invoke-HackScratch -RepoAlias $repoAlias
        return
    }

    # Known subcommand → dispatch
    if ($Subcommand -in $reservedSubcommands) {
        switch ($Subcommand) {
            "name"   { Invoke-HackName -Description $Arg1 }
            "done"   { Invoke-HackDone }
            "resume" { Invoke-HackResumeOrGo -Filter $Arg1 -LaunchAgent }
            "go"     { Invoke-HackResumeOrGo -Filter $Arg1 }
            "list"   { Invoke-HackList -Repo $Arg1 }
            "clean"  { Invoke-HackClean -Repo $Arg1 }
            "branch" { Invoke-HackBranch -RepoArg $Arg1 }
            "status" { Invoke-HackStatus }
        }
        return
    }

    # Not a subcommand → treat as repo alias
    Invoke-HackScratch -RepoAlias $Subcommand
}

# ---------------------------------------------------------------------------
# hack [repo] — scratch worktree
# ---------------------------------------------------------------------------

function Invoke-HackScratch {
    param([Parameter(Mandatory)][string]$RepoAlias)

    try {
        $config = Get-HackConfig
        if (-not $config) {
            Write-Host "No config. Run: workshop add <alias> <url>" -ForegroundColor Yellow
            return
        }

        $resolved = Resolve-HackRepo -RepoAlias $RepoAlias -Config $config

        # Generate scratch name: _scratch-<4hex>
        $hex = -join ((1..4) | ForEach-Object { '{0:x}' -f (Get-Random -Maximum 16) })
        $scratchName = "_scratch-$hex"
        $scratchBranch = "_scratch/$hex"

        Write-Host ""
        Write-Host "Creating scratch worktree for '$RepoAlias'..." -ForegroundColor Cyan

        $dir = New-HackWorktree `
            -Alias $resolved.Alias `
            -Url $resolved.Url `
            -BaseBranch $resolved.BaseBranch `
            -BranchName $scratchBranch `
            -WorktreeName $scratchName `
            -Config $config

        Set-Location $dir
        Write-Host ""
        $cmd = Get-AgentCommand -PlanMode
        Write-Host "$($dir)> $cmd" -ForegroundColor DarkGray
        Invoke-Expression $cmd
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# hack name "description" — crystallize scratch → named branch
# ---------------------------------------------------------------------------

function Invoke-HackName {
    param([string]$Description)

    try {
        if ([string]::IsNullOrWhiteSpace($Description)) {
            $Description = Read-Host "Description"
            if ([string]::IsNullOrWhiteSpace($Description)) { throw "Description is required." }
        }

        $config = Get-HackConfig
        if (-not $config) { throw "No hack config found." }

        $pwd = (Get-Location).Path
        $baseDir = $config.baseDir

        # Must be in a scratch worktree
        if (-not $pwd.StartsWith($baseDir)) {
            throw "Not in a hack worktree."
        }

        $relative = $pwd.Substring($baseDir.Length).TrimStart([IO.Path]::DirectorySeparatorChar, '/')
        $parts = $relative -split '[/\\]'
        if ($parts.Count -lt 2) { throw "Not in a hack worktree." }

        $repoName = $parts[0]
        $currentDirName = $parts[1]

        if ($currentDirName -notmatch '^_scratch-') {
            throw "Not in a scratch worktree. 'hack name' only works from _scratch-* directories."
        }

        $slug = Get-SlugFromDescription -Description $Description
        $branchPrefix = $config.branchPrefix ?? "user/$($env:USERNAME ?? $env:USER)"
        $newBranch = "$branchPrefix/$slug"
        $model = $config.defaultModel ?? "claude-sonnet-4-5"

        $oldBranch = git branch --show-current 2>$null
        if (-not $oldBranch) { throw "Could not determine current branch." }

        # Rename the branch
        Write-Host "Renaming branch: $oldBranch -> $newBranch" -ForegroundColor Cyan
        git branch -m $oldBranch $newBranch
        if ($LASTEXITCODE -ne 0) { throw "Failed to rename branch." }

        # Move the worktree directory
        $oldPath = [IO.Path]::Combine($baseDir, $repoName, $currentDirName)
        $newPath = [IO.Path]::Combine($baseDir, $repoName, $slug)

        if (Test-Path $newPath) { throw "Directory '$newPath' already exists." }

        Write-Host "Moving worktree: $currentDirName -> $slug" -ForegroundColor Cyan

        $treesDir = Get-TreesDir -Config $config
        $baseRepoDir = [IO.Path]::Combine($treesDir, $repoName)

        # Step out of the directory before moving (Windows locks PWD)
        Set-Location $baseDir
        Move-Item -Path $oldPath -Destination $newPath -Force
        Push-Location $baseRepoDir
        try {
            git worktree repair 2>&1 | Out-Null
        }
        finally { Pop-Location }

        # Write SPEC.md
        New-SpecFile -WorktreeDir $newPath -Task $Description -BranchName $newBranch `
            -RepoAlias $repoName -Model $model | Out-Null

        Set-Location $newPath

        Write-Host ""
        Write-Host "Branch: $newBranch" -ForegroundColor Green
        Write-Host "Path: $newPath" -ForegroundColor Green
        Write-Host "SPEC.md written." -ForegroundColor Gray
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# hack done — push + PR
# ---------------------------------------------------------------------------

function Invoke-HackDone {
    try {
        $config = Get-HackConfig
        if (-not $config) { throw "No hack config found." }

        $pwd = (Get-Location).Path
        $baseDir = $config.baseDir

        # If not in a worktree, show picker (exclude scratches)
        if (-not $pwd.StartsWith($baseDir)) {
            $worktrees = @(Get-HackWorktrees | Where-Object { -not $_.IsScratch })
            $selected = Show-WorktreePicker -Worktrees $worktrees -Title "Select worktree to finalize"
            if (-not $selected) { return }
            Set-Location $selected.Path
            $pwd = $selected.Path
        }

        $spec = Get-SpecData -WorktreeDir $pwd
        $task = if ($spec -and $spec["task"]) { $spec["task"] } else { "" }
        $branch = git branch --show-current 2>$null
        if (-not $branch) { throw "Could not determine current branch." }

        if ($branch -match '^_scratch/') {
            Write-Host "This is an unnamed scratch worktree. Run 'hack name `"description`"' first." -ForegroundColor Yellow
            return
        }

        # Push
        Write-Host "Pushing branch '$branch'..." -ForegroundColor Cyan
        git push -u origin $branch | Out-Host
        if ($LASTEXITCODE -ne 0) { throw "Failed to push branch." }

        # Detect provider
        $remoteUrl = git remote get-url origin 2>$null
        $provider = "unknown"
        if ($remoteUrl -match 'github\.com') { $provider = "github" }
        elseif ($remoteUrl -match 'visualstudio\.com|dev\.azure\.com') { $provider = "ado" }

        # Detect base branch
        $repoName = $null
        $relative = $pwd.Substring($baseDir.Length).TrimStart([IO.Path]::DirectorySeparatorChar, '/')
        $parts = $relative -split '[/\\]'
        if ($parts.Count -ge 1) { $repoName = $parts[0] }

        $baseBranch = $config.defaultBaseBranch ?? "develop"
        if ($repoName -and $config.repos.PSObject.Properties.Name -contains $repoName) {
            $repoConf = $config.repos.$repoName
            if ($repoConf.baseBranch) { $baseBranch = $repoConf.baseBranch }
        }

        $prTitle = if ($task) { $task } else { $branch -replace '^.+/', '' -replace '-', ' ' }

        $prUrl = $null
        switch ($provider) {
            "github" {
                Write-Host "Creating GitHub PR..." -ForegroundColor Cyan
                $body = "## Summary`n`n$task`n`n---`n*Created with hack*"
                $result = gh pr create --title $prTitle --body $body --base $baseBranch 2>&1
                if ($LASTEXITCODE -eq 0) {
                    $prUrl = ($result | Select-String 'https://').ToString().Trim()
                    Write-Host "PR created: $prUrl" -ForegroundColor Green
                } else {
                    $existing = gh pr view --json url --jq '.url' 2>$null
                    if ($existing) {
                        $prUrl = $existing.Trim()
                        Write-Host "PR already exists: $prUrl" -ForegroundColor Yellow
                        Write-Host "Pushed latest changes." -ForegroundColor Green
                    } else {
                        Write-Host "gh pr create output: $result" -ForegroundColor Red
                    }
                }
            }
            "ado" {
                if (Get-Command az -ErrorAction SilentlyContinue) {
                    Write-Host "Creating Azure DevOps PR..." -ForegroundColor Cyan
                    $result = az repos pr create --title $prTitle --source-branch $branch --target-branch $baseBranch --auto-complete false 2>&1
                    if ($LASTEXITCODE -eq 0) {
                        try {
                            $prObj = $result | ConvertFrom-Json
                            $prUrl = $prObj.url ?? $prObj.repository.webUrl
                            Write-Host "PR created: $prUrl" -ForegroundColor Green
                        }
                        catch {
                            Write-Host "PR created (check Azure DevOps)" -ForegroundColor Green
                        }
                    } else {
                        Write-Host "az repos pr create failed. Opening browser..." -ForegroundColor Yellow
                        $adoUrl = $null
                        if ($remoteUrl -match 'dev\.azure\.com/([^/]+)/([^/]+)/_git/(.+)') {
                            $adoUrl = "https://dev.azure.com/$($matches[1])/$($matches[2])/_git/$($matches[3])/pullrequestcreate?sourceRef=$branch&targetRef=$baseBranch"
                        }
                        elseif ($remoteUrl -match 'visualstudio\.com/([^/]+)/_git/(.+)') {
                            $adoUrl = "$remoteUrl/pullrequestcreate?sourceRef=$branch&targetRef=$baseBranch"
                        }
                        if ($adoUrl) {
                            Start-Process $adoUrl
                            $prUrl = $adoUrl
                        }
                    }
                } else {
                    Write-Host "az CLI not found. Opening browser..." -ForegroundColor Yellow
                    $adoUrl = $null
                    if ($remoteUrl -match 'dev\.azure\.com/([^/]+)/([^/]+)/_git/(.+)') {
                        $adoUrl = "https://dev.azure.com/$($matches[1])/$($matches[2])/_git/$($matches[3])/pullrequestcreate?sourceRef=$branch&targetRef=$baseBranch"
                    }
                    elseif ($remoteUrl -match 'visualstudio\.com/([^/]+)/_git/(.+)') {
                        $adoUrl = "$remoteUrl/pullrequestcreate?sourceRef=$branch&targetRef=$baseBranch"
                    }
                    if ($adoUrl) {
                        Start-Process $adoUrl
                        $prUrl = $adoUrl
                    }
                }
            }
            default {
                Write-Host "Unknown provider. Push completed; create PR manually." -ForegroundColor Yellow
            }
        }

        if ($prUrl) {
            Update-SpecField -WorktreeDir $pwd -Field "pr" -Value $prUrl
            Write-Host "PR URL saved to SPEC.md" -ForegroundColor Gray
        }
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# hack resume [filter] / hack go [filter]
# ---------------------------------------------------------------------------

function Invoke-HackResumeOrGo {
    param(
        [string]$Filter,
        [switch]$LaunchAgent
    )

    try {
        $config = Get-HackConfig
        if (-not $config) { throw "No hack config found." }

        $worktrees = Get-HackWorktrees -Filter $Filter

        # Exact single match on filter → skip picker
        if ($worktrees.Count -eq 1) {
            $selected = $worktrees[0]
        } else {
            $title = if ($LaunchAgent) { "Select worktree to resume" } else { "Select worktree" }
            $selected = Show-WorktreePicker -Worktrees $worktrees -Title $title
        }

        if (-not $selected) { return }

        $label = "$($selected.Repo)/$($selected.Task)"
        if ($LaunchAgent) {
            Write-Host "`nResuming: $label" -ForegroundColor Green
        } else {
            Write-Host "`nGoing to: $label" -ForegroundColor Green
        }
        Write-Host "Path: $($selected.Path)" -ForegroundColor Gray

        Set-Location $selected.Path

        if ($LaunchAgent) {
            $spec = Get-SpecData -WorktreeDir $selected.Path
            $model = if ($spec -and $spec["model"]) { $spec["model"] } else { $null }
            $mode = Get-AgentMode

            if ($mode -eq "claude") {
                $cmd = Get-AgentCommand -Resume -Model $model
            } else {
                $taskTitle = if ($spec -and $spec["task"]) { $spec["task"] } else { $selected.Task }
                $cmd = Get-AgentCommand -Prompt "Continue working on: $taskTitle"
            }

            Write-Host "$($selected.Path)> $cmd" -ForegroundColor DarkGray
            Invoke-Expression $cmd
        }
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# hack list — status dashboard (grouped by repo)
# ---------------------------------------------------------------------------

function Invoke-HackList {
    param([string]$Repo = "all")

    if ([string]::IsNullOrWhiteSpace($Repo)) { $Repo = "all" }

    $worktrees = Get-HackWorktrees -Repo $Repo

    if ($worktrees.Count -eq 0) {
        Write-Host "No worktrees found." -ForegroundColor Yellow
        return
    }

    $grouped = $worktrees | Group-Object Repo

    foreach ($group in $grouped) {
        Write-Host "`n  $($group.Name)" -ForegroundColor Cyan

        foreach ($wt in ($group.Group | Sort-Object LastModified -Descending)) {
            $color = switch ($wt.Status) {
                "dirty"  { "Red" }
                "ahead"  { "Yellow" }
                "behind" { "Yellow" }
                "stale"  { "DarkGray" }
                "merged" { "DarkGreen" }
                "clean"  { "Green" }
                default  { "White" }
            }
            $symbol = switch ($wt.Status) {
                "dirty"  { "*" }
                "ahead"  { "^" }
                "behind" { "v" }
                "stale"  { "~" }
                "merged" { "=" }
                "clean"  { " " }
                default  { "?" }
            }

            $gitInfo = @()
            if ($wt.Ahead -gt 0) { $gitInfo += "+$($wt.Ahead)" }
            if ($wt.Behind -gt 0) { $gitInfo += "-$($wt.Behind)" }
            if ($wt.Dirty) { $gitInfo += "*" }
            $gitStr = if ($gitInfo.Count -gt 0) { $gitInfo -join " " } else { "sync" }

            $unnamedTag = if ($wt.IsScratch) { " [UNNAMED]" } else { "" }

            Write-Host "  [$symbol] " -ForegroundColor $color -NoNewline
            Write-Host "$($wt.Task) " -NoNewline
            Write-Host "[$gitStr] " -ForegroundColor $color -NoNewline
            Write-Host "($($wt.Age)d)" -ForegroundColor Gray -NoNewline
            if ($unnamedTag) { Write-Host $unnamedTag -ForegroundColor DarkYellow -NoNewline }
            if ($wt.Description) { Write-Host " $($wt.Description)" -ForegroundColor Gray } else { Write-Host "" }

            if ($wt.Merged) {
                Write-Host "      MERGED - safe to remove" -ForegroundColor DarkGreen
            }
            if ($wt.PrUrl) {
                Write-Host "      PR: $($wt.PrUrl)" -ForegroundColor DarkGray
            }
        }
    }

    # Summary
    Write-Host ""
    Write-Host "Total: $($worktrees.Count) worktrees" -ForegroundColor Cyan
    $dirtyCount = ($worktrees | Where-Object { $_.Dirty }).Count
    $mergedCount = ($worktrees | Where-Object { $_.Merged }).Count
    $scratchCount = ($worktrees | Where-Object { $_.IsScratch }).Count
    if ($dirtyCount -gt 0) { Write-Host "  $dirtyCount with uncommitted changes" -ForegroundColor Red }
    if ($mergedCount -gt 0) { Write-Host "  $mergedCount merged (safe to clean)" -ForegroundColor DarkGreen }
    if ($scratchCount -gt 0) { Write-Host "  $scratchCount unnamed scratches" -ForegroundColor DarkYellow }
}

# ---------------------------------------------------------------------------
# hack clean — interactive cleanup
# ---------------------------------------------------------------------------

function Invoke-HackClean {
    param([string]$Repo = "all")

    if ([string]::IsNullOrWhiteSpace($Repo)) { $Repo = "all" }

    try {
        $config = Get-HackConfig
        if (-not $config) { throw "No hack config found." }

        $worktrees = Get-HackWorktrees -Repo $Repo

        if ($worktrees.Count -eq 0) {
            Write-Host "No worktrees found." -ForegroundColor Yellow
            return
        }

        $sorted = @($worktrees | Sort-Object @{Expression={$_.Merged}; Descending=$true}, @{Expression={$_.IsScratch}; Descending=$true}, LastModified)

        Write-Host "`nWorktrees available for cleanup:" -ForegroundColor Cyan
        Write-Host ""

        $idx = 1
        foreach ($wt in $sorted) {
            $mergedTag = if ($wt.Merged) { " [MERGED - safe]" } else { "" }
            $dirtyTag = if ($wt.Dirty) { " [DIRTY]" } else { "" }
            $unnamedTag = if ($wt.IsScratch) { " [UNNAMED]" } else { "" }

            $color = if ($wt.Merged) { "DarkGreen" }
                     elseif ($wt.IsScratch) { "DarkYellow" }
                     elseif ($wt.Dirty) { "Red" }
                     else { "White" }

            Write-Host "[$idx] " -NoNewline
            Write-Host "$($wt.Repo)/$($wt.Task)" -ForegroundColor $color -NoNewline
            Write-Host "$mergedTag" -ForegroundColor DarkGreen -NoNewline
            Write-Host "$unnamedTag" -ForegroundColor DarkYellow -NoNewline
            Write-Host "$dirtyTag" -ForegroundColor Red -NoNewline
            Write-Host " ($($wt.Age)d)" -ForegroundColor Gray
            if ($wt.Description) { Write-Host "    $($wt.Description)" -ForegroundColor Gray }

            $idx++
        }

        Write-Host ""
        $userInput = Read-Host "Remove which? (numbers comma-separated, 'all', or 'q')"
        if ($userInput -eq 'q') { return }

        $toRemove = @()
        if ($userInput -eq 'all') {
            $toRemove = $sorted
        }
        else {
            $nums = $userInput -split ',' | ForEach-Object { [int]$_.Trim() }
            foreach ($n in $nums) {
                if ($n -ge 1 -and $n -le $sorted.Count) {
                    $toRemove += $sorted[$n - 1]
                }
            }
        }

        if ($toRemove.Count -eq 0) {
            Write-Host "Nothing selected." -ForegroundColor Yellow
            return
        }

        foreach ($wt in $toRemove) {
            Write-Host ""
            if ($wt.Dirty) {
                $confirm = Read-Host "$($wt.Repo)/$($wt.Task) has uncommitted changes. Remove anyway? [y/N]"
                if ($confirm -ne 'y' -and $confirm -ne 'Y') {
                    Write-Host "Skipped." -ForegroundColor Yellow
                    continue
                }
            }

            Write-Host "Removing $($wt.Repo)/$($wt.Task)..." -ForegroundColor Cyan

            $currentDir = (Get-Location).Path
            if ($currentDir.StartsWith($wt.Path)) {
                Set-Location $config.baseDir
            }

            $treesDir = Get-TreesDir -Config $config
            $baseRepoDir = [IO.Path]::Combine($treesDir, $wt.Repo)
            if (Test-Path $baseRepoDir) {
                Push-Location $baseRepoDir
                try {
                    git worktree remove $wt.Path --force 2>&1 | Out-Host
                    git worktree prune 2>&1 | Out-Null
                }
                finally { Pop-Location }
            }

            if (Test-Path $wt.Path) {
                Remove-Item -Path $wt.Path -Recurse -Force -ErrorAction SilentlyContinue
            }

            # Only ask about remote branch for non-scratch worktrees
            if (-not $wt.IsScratch -and $wt.Branch) {
                $delRemote = Read-Host "Delete remote branch '$($wt.Branch)'? [y/N]"
                if ($delRemote -eq 'y' -or $delRemote -eq 'Y') {
                    if (Test-Path $baseRepoDir) {
                        Push-Location $baseRepoDir
                        try {
                            git push origin --delete $wt.Branch 2>&1 | Out-Host
                        }
                        finally { Pop-Location }
                    }
                }
            }

            Write-Host "Removed." -ForegroundColor Green
        }
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# hack branch — worktree from existing remote branch
# ---------------------------------------------------------------------------

function Invoke-HackBranch {
    param([string]$RepoArg)

    try {
        $config = Get-HackConfig
        if (-not $config) { $config = Initialize-HackConfig }

        if ([string]::IsNullOrWhiteSpace($RepoArg)) {
            $RepoArg = Resolve-RepoFromPwd -Config $config
            if (-not $RepoArg) { $RepoArg = $config.defaultRepo }
            if (-not $RepoArg) {
                $RepoArg = Read-Host "Repository alias"
                if ([string]::IsNullOrWhiteSpace($RepoArg)) { throw "Repository is required." }
            }
        }

        $resolved = Resolve-HackRepo -RepoAlias $RepoArg -Config $config
        $baseRepoDir = Ensure-BaseClone -Alias $resolved.Alias -Url $resolved.Url `
            -BaseBranch $resolved.BaseBranch -Config $config

        $prefix = $config.branchPrefix ?? "user/$($env:USERNAME ?? $env:USER)"
        Write-Host "Fetching branches matching '$prefix/*'..." -ForegroundColor Cyan
        Push-Location $baseRepoDir
        try {
            git fetch origin 2>&1 | Out-Null
            $branches = git branch -r | ForEach-Object { $_.Trim() } |
                Where-Object { $_ -match "origin/$prefix/" } |
                ForEach-Object { $_ -replace '^origin/', '' }
        }
        finally { Pop-Location }

        if (-not $branches -or $branches.Count -eq 0) {
            Write-Host "No branches found matching '$prefix/*'." -ForegroundColor Yellow
            return
        }

        Write-Host "`nSelect branch:" -ForegroundColor Cyan
        $idx = 1
        foreach ($b in $branches) {
            Write-Host "[$idx] $b"
            $idx++
        }

        $choice = Read-Host "`nChoice [1-$($branches.Count)] or 'q'"
        if ($choice -eq 'q') { return }

        $choiceNum = [int]$choice
        if ($choiceNum -lt 1 -or $choiceNum -gt $branches.Count) { throw "Invalid choice." }
        $selectedBranch = $branches[$choiceNum - 1]

        $wtName = $selectedBranch -replace "^$([regex]::Escape($prefix))/", ''

        Write-Host "`nCreating worktree from branch '$selectedBranch'..." -ForegroundColor Cyan
        $dir = New-HackWorktreeFromBranch `
            -Alias $resolved.Alias `
            -Url $resolved.Url `
            -BaseBranch $resolved.BaseBranch `
            -BranchName $selectedBranch `
            -WorktreeName $wtName `
            -Config $config

        Set-Location $dir
        $cmd = Get-AgentCommand -Resume
        Write-Host "`n$($dir)> $cmd" -ForegroundColor DarkGray
        Invoke-Expression $cmd
    }
    catch {
        Write-Host "Error: $_" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# hack status — show current hack info + toggle agent
# ---------------------------------------------------------------------------

function Invoke-HackStatus {
    $config = Get-HackConfig
    if (-not $config) {
        Write-Host "No hack config found." -ForegroundColor Yellow
        return
    }

    $mode = Get-AgentMode
    Write-Host "`nAgent: $mode" -ForegroundColor Cyan

    $repoAlias = Resolve-RepoFromPwd -Config $config
    if ($repoAlias) {
        $pwd = (Get-Location).Path
        $relative = $pwd.Substring($config.baseDir.Length).TrimStart([IO.Path]::DirectorySeparatorChar, '/')
        $parts = $relative -split '[/\\]'
        $taskName = if ($parts.Count -ge 2) { $parts[1] } else { "?" }

        Write-Host "Repo: $repoAlias" -ForegroundColor White
        Write-Host "Task: $taskName" -ForegroundColor White

        $spec = Get-SpecData -WorktreeDir $pwd
        if ($spec) {
            if ($spec["task"]) { Write-Host "Description: $($spec["task"])" -ForegroundColor Gray }
            if ($spec["branch"]) { Write-Host "Branch: $($spec["branch"])" -ForegroundColor Gray }
            if ($spec["pr"]) { Write-Host "PR: $($spec["pr"])" -ForegroundColor Gray }
        }

        $branch = git branch --show-current 2>$null
        if ($branch -match '^_scratch/') {
            Write-Host "Status: UNNAMED scratch — run 'hack name `"description`"' to crystallize" -ForegroundColor DarkYellow
        }
    } else {
        Write-Host "Not in a hack worktree." -ForegroundColor Gray
    }

    Write-Host ""
    $toggle = Read-Host "Toggle agent mode? [y/N]"
    if ($toggle -eq 'y' -or $toggle -eq 'Y') {
        $new = if ($mode -eq "claude") { "copilot" } else { "claude" }
        Set-AgentMode -Mode $new
        Write-Host "Switched to $new mode" -ForegroundColor Green
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
    if ($parts.Count -ge 2) {
        return "$($parts[0])/$($parts[1])"
    }
    return ""
}

# ---------------------------------------------------------------------------
# Aliases
# ---------------------------------------------------------------------------

function resume { hack resume @args }
function goto-hack { hack go @args }

Set-Alias -Name r -Value resume
Set-Alias -Name g -Value goto-hack
Set-Alias -Name lh -Value list-hacks

function list-hacks { hack list @args }
function clean-hack { hack clean @args }
function toggle-agent { Invoke-HackStatus }
