# install-hack.ps1 — Adds hack.ps1 dot-source to PowerShell profile + installs fzf

$dotfilesDir = Split-Path -Parent $PSScriptRoot
$hackScript = Join-Path (Join-Path $dotfilesDir "shell") "hack.ps1"

if (-not (Test-Path $hackScript)) {
    Write-Host "Error: hack.ps1 not found at $hackScript" -ForegroundColor Red
    exit 1
}

# Install dependencies
foreach ($dep in @(
    @{ Cmd = "fzf";    WingetId = "junegunn.fzf" },
    @{ Cmd = "python"; WingetId = "Python.Python.3.13" }
)) {
    if (-not (Get-Command $dep.Cmd -ErrorAction SilentlyContinue)) {
        Write-Host "Installing $($dep.Cmd)..." -ForegroundColor Cyan
        winget install --id $dep.WingetId --accept-source-agreements --accept-package-agreements
    } else {
        Write-Host "$($dep.Cmd) already installed" -ForegroundColor Gray
    }
}

# Determine profile path - use local (non-roaming) profile to avoid OneDrive sync.
# Run scripts/setup-profile.ps1 first to set up the stub/local split.
if ($PSVersionTable.PSVersion.Major -ge 7) {
    $profilePath = Join-Path $env:LOCALAPPDATA 'PowerShell\7\profile.ps1'
} else {
    $profilePath = Join-Path $env:LOCALAPPDATA 'PowerShell\5\profile.ps1'
}

# Fall back to default $PROFILE if local profile dir doesn't exist yet
# (setup-profile.ps1 hasn't been run)
if (-not (Test-Path (Split-Path $profilePath))) {
    Write-Host "Local profile dir not found - falling back to `$PROFILE" -ForegroundColor Yellow
    Write-Host "Run scripts/setup-profile.ps1 first to avoid OneDrive sync" -ForegroundColor Yellow
    $profilePath = $PROFILE.CurrentUserCurrentHost
}

if (-not (Test-Path $profilePath)) {
    New-Item -ItemType File -Path $profilePath -Force | Out-Null
    Write-Host "Created profile at $profilePath" -ForegroundColor Gray
}

$dotSourceLine = ". `"$hackScript`""
$profileContent = Get-Content $profilePath -Raw -ErrorAction SilentlyContinue

if ($profileContent -and $profileContent.Contains($hackScript)) {
    Write-Host "hack.ps1 already sourced in profile" -ForegroundColor Gray
} else {
    # Append dot-source
    Add-Content -Path $profilePath -Value "`n# Hack worktree tooling`n$dotSourceLine"
    Write-Host "Added hack.ps1 to PowerShell profile: $profilePath" -ForegroundColor Green
}

# Bootstrap hack config if it doesn't exist yet
$hackConfigPath = Join-Path (Join-Path $env:USERPROFILE ".config") "hack\config.json"
if (-not (Test-Path $hackConfigPath)) {
    Write-Host ""
    Write-Host "Setting up worktree configuration..." -ForegroundColor Cyan

    # Discover available drives (real drive letters only, non-system first)
    $drives = @(Get-PSDrive -PSProvider FileSystem | Where-Object {
        $_.Name.Length -eq 1 -and $_.Free -gt 0
    } | Sort-Object { if ($_.Name -eq "C") { 1 } else { 0 } } | ForEach-Object { "$($_.Name):\" })

    Write-Host ""
    Write-Host "Available drives:" -ForegroundColor Cyan
    for ($i = 0; $i -lt $drives.Count; $i++) {
        $drv = Get-PSDrive -Name $drives[$i].Substring(0,1)
        $freeGB = [math]::Round($drv.Free / 1GB, 1)
        $usedGB = [math]::Round($drv.Used / 1GB, 1)
        Write-Host "  [$($i + 1)] $($drives[$i])  ($usedGB GB used, $freeGB GB free)"
    }
    Write-Host ""

    $defaultIdx = 1
    do {
        $choice = Read-Host "Select drive for worktrees [$defaultIdx]"
        if ([string]::IsNullOrWhiteSpace($choice)) { $choice = "$defaultIdx" }
        $idx = 0
        $valid = [int]::TryParse($choice, [ref]$idx) -and $idx -ge 1 -and $idx -le $drives.Count
        if (-not $valid) { Write-Host "  Enter a number between 1 and $($drives.Count)" -ForegroundColor Yellow }
    } while (-not $valid)

    $selectedDrive = $drives[$idx - 1]
    $defaultBase = "${selectedDrive}worktree"
    $baseDir = Read-Host "Worktree base directory [$defaultBase]"
    if ([string]::IsNullOrWhiteSpace($baseDir)) { $baseDir = $defaultBase }

    $defaultPrefix = "user/$env:USERNAME"
    $branchPrefix = Read-Host "Branch prefix [$defaultPrefix]"
    if ([string]::IsNullOrWhiteSpace($branchPrefix)) { $branchPrefix = $defaultPrefix }

    $hackConfigDir = Split-Path $hackConfigPath
    if (-not (Test-Path $hackConfigDir)) {
        New-Item -ItemType Directory -Path $hackConfigDir -Force | Out-Null
    }

    $config = [ordered]@{
        baseDir           = $baseDir
        branchPrefix      = $branchPrefix
        defaultBaseBranch = "develop"
        defaultRepo       = $null
        repos             = [ordered]@{}
    }
    $config | ConvertTo-Json -Depth 10 | Set-Content -Path $hackConfigPath -Encoding UTF8
    Write-Host "Saved hack config to $hackConfigPath" -ForegroundColor Green
} else {
    Write-Host "Hack config already exists at $hackConfigPath" -ForegroundColor Gray
}

Write-Host "Restart your shell or run: $dotSourceLine" -ForegroundColor Gray
