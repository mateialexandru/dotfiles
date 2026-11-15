$script:UvMinimumVersion = [version]'0.13.4'

function Get-CheckedUvVersion([string]$Executable) {
    $output = & $Executable --version
    if ($LASTEXITCODE -ne 0 -or "$output" -notmatch '^uvx? (\d+\.\d+\.\d+)(?:\s|$)') {
        throw "Cannot verify uv executable: $Executable ($output)"
    }
    return [version]$Matches[1]
}

function Assert-UvSecurity {
    foreach ($tool in @('uv', 'uvx')) {
        $command = Get-Command $tool -CommandType Application -ErrorAction Stop | Select-Object -First 1
        $version = Get-CheckedUvVersion $command.Source
        if ($version -lt $script:UvMinimumVersion) {
            throw "$tool $version at $($command.Source) is below the security baseline $script:UvMinimumVersion. Review PATH shadowing and use a patched approved installation."
        }
    }
}

function Repair-UvWindowsSelection {
    $moves = @()
    foreach ($tool in @('uv', 'uvx')) {
        $command = Get-Command $tool -CommandType Application -ErrorAction Stop | Select-Object -First 1
        $version = Get-CheckedUvVersion $command.Source
        if ($version -ge $script:UvMinimumVersion) { continue }
        $legacy = Join-Path $env:USERPROFILE ".local\bin\$tool.exe"
        $managed = Join-Path $env:LOCALAPPDATA "Microsoft\WinGet\Links\$tool.exe"
        if ([IO.Path]::GetFullPath($command.Source) -ne [IO.Path]::GetFullPath($legacy)) {
            throw "Refusing to relocate a custom $tool installation: $($command.Source). Update it through its approved owner."
        }
        if (-not (Test-Path -LiteralPath $managed -PathType Leaf) -or
            (Get-CheckedUvVersion $managed) -lt $script:UvMinimumVersion) {
            throw "No patched WinGet $tool is available. Update astral-sh.uv through the approved package manager first."
        }
        $hash = (Get-FileHash -LiteralPath $legacy -Algorithm SHA256).Hash
        $directory = Join-Path $env:LOCALAPPDATA "dotfiles\retired-tools\$tool-$version-$hash"
        $destination = Join-Path $directory "$tool.exe"
        if (Test-Path -LiteralPath $destination) {
            throw "Retirement destination already exists; inspect it before proceeding: $destination"
        }
        $moves += @{ Source = $legacy; Destination = $destination; Directory = $directory }
    }
    $completed = @()
    try {
        foreach ($move in $moves) {
            New-Item -ItemType Directory -Path $move.Directory -Force | Out-Null
            Move-Item -LiteralPath $move.Source -Destination $move.Destination -ErrorAction Stop
            $completed += $move
            Write-Host "Retired stale executable with rollback copy: $($move.Destination)"
        }
        Assert-UvSecurity
    } catch {
        foreach ($move in $completed) {
            Move-Item -LiteralPath $move.Destination -Destination $move.Source -ErrorAction Stop
        }
        throw
    }
}
