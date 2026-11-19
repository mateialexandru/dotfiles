#Requires -Version 7.3
[CmdletBinding()]
param(
    [switch]$UpdateTools,
    [switch]$Rebuild,
    [ValidateRange(1, 64)][int]$Jobs = 2
)

function Invoke-EmacsBuild {
    param([switch]$UpdateTools, [switch]$Rebuild, [int]$Jobs = 2)
    $ErrorActionPreference = 'Stop'
    $PSNativeCommandArgumentPassing = 'Standard'
    $configuration = @{
        recipeVersion = 1 # Bump when compile/install semantics change, not for orchestration or tests.
        sourceRevision = 'a5ae66f425752ceb65d66091f5e75d935e9b1000'
        msys2Version = '20260927'
        msys2ArchiveSha256 = 'ea2f31a0b6ade63914ce441ffb022f0f6aa96982bfefa2326460a26d5fb01322'
        configureOptions = @(
            '--with-native-compilation=aot', '--with-modules', '--with-tree-sitter',
            '--with-gnutls', '--with-xml2', '--with-sqlite3', '--with-rsvg', '--with-webp',
            '--with-png', '--with-jpeg', '--with-tiff', '--with-gif', '--with-lcms2',
            '--with-harfbuzz', '--without-dbus', '--without-compress-install'
        )
    }
    $repo = Split-Path $PSScriptRoot -Parent
    $root = Join-Path $repo '.build\emacs'
    $msys = Join-Path $root 'msys64'
    $bash = Join-Path $msys 'usr\bin\bash.exe'
    $compilerRoot = Join-Path $msys 'ucrt64'
    if ($root -notmatch '^[A-Za-z]:\\[ -~]+$' -or $root -match '[\s''"$`;]') {
        throw 'The Windows Emacs build requires a local ASCII path without spaces or shell metacharacters.'
    }
    New-Item -ItemType Directory -Path (Join-Path $root 'logs') -Force | Out-Null
    $helperHash = (Get-FileHash -LiteralPath (Join-Path $PSScriptRoot 'build-emacs-windows.sh') -Algorithm SHA256).Hash
    $buildHelper = Join-Path $root "recipe-$helperHash.sh"
    if (-not (Test-Path -LiteralPath $buildHelper)) {
        Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'build-emacs-windows.sh') -Destination $buildHelper
    }
    $log = Join-Path $root ("logs\build-{0}.log" -f (Get-Date -Format 'yyyyMMdd-HHmmss'))
    $lock = $null
    $names = @('PATH', 'MSYSTEM', 'CHERE_INVOKING', 'EMACS_NATIVE_COMP_ROOT', 'DOTFILES_EMACS_BUILD')
    $saved = @{}
    foreach ($name in $names) { $saved[$name] = [Environment]::GetEnvironmentVariable($name, 'Process') }

    try {
        $lock = [IO.File]::Open((Join-Path $root 'build.lock'), 'OpenOrCreate', 'ReadWrite', 'None')
        $env:MSYSTEM = 'UCRT64'
        $env:CHERE_INVOKING = '1'
        if (-not (Test-Path -LiteralPath $bash)) {
            if (Test-Path -LiteralPath $msys) {
                throw "Incomplete MSYS2 installation at $msys. Repair it explicitly before rerunning."
            }
            $archive = Join-Path $root "msys2-base-x86_64-$($configuration.msys2Version).tar.xz"
            if (-not (Test-Path -LiteralPath $archive)) {
                $date = [datetime]::ParseExact($configuration.msys2Version, 'yyyyMMdd', [Globalization.CultureInfo]::InvariantCulture).ToString('yyyy-MM-dd')
                $uri = "https://github.com/msys2/msys2-installer/releases/download/$date/$(Split-Path $archive -Leaf)"
                Write-Host "Downloading official isolated MSYS2 bootstrap: $uri"
                Invoke-WebRequest -Uri $uri -OutFile "$archive.partial"
                if ((Get-FileHash -LiteralPath "$archive.partial" -Algorithm SHA256).Hash -ne $configuration.msys2ArchiveSha256) {
                    throw 'MSYS2 archive SHA-256 mismatch; refusing to extract it.'
                }
                Move-Item -LiteralPath "$archive.partial" -Destination $archive
            }
            if ((Get-FileHash -LiteralPath $archive -Algorithm SHA256).Hash -ne $configuration.msys2ArchiveSha256) {
                throw 'Cached MSYS2 archive SHA-256 mismatch; refusing to extract it.'
            }
            Invoke-BuildNative 'tar.exe' @('-xf', $archive, '-C', $root)
            if (-not (Test-Path -LiteralPath $bash)) { throw "MSYS2 did not install at $msys." }
        }
        $initialized = Join-Path $msys '.dotfiles-initialized'
        $toolsUpdated = $false
        if ($UpdateTools -or -not (Test-Path -LiteralPath $initialized)) {
            $inUse = @(Get-CimInstance Win32_Process -Filter "Name='emacs.exe'" |
                Where-Object { $_.ExecutablePath -and $_.ExecutablePath.StartsWith($root, [StringComparison]::OrdinalIgnoreCase) })
            if ($inUse.Count) {
                throw 'A source Emacs is running. Stop it cleanly before -UpdateTools; no processes were stopped.'
            }
            if (Test-Path -LiteralPath $initialized) { Remove-Item -LiteralPath $initialized -Force }
            $toolsUpdated = $true
            Invoke-EmacsBuildStage 'update'
            Invoke-EmacsBuildStage 'update'
            if ($script:msysRestartRequested) {
                Invoke-EmacsBuildStage 'update'
                if ($script:msysRestartRequested) { throw 'MSYS2 still requires a core restart; initialization is incomplete.' }
            }
            New-Item -ItemType File -Path $initialized -Force | Out-Null
        }
        Invoke-EmacsBuildStage 'dependencies'
        $inventory = & $bash --login -c 'pacman -Q'
        if ($LASTEXITCODE -ne 0) { throw 'Could not inventory MSYS2 packages.' }
        $recipe = @(
            $configuration.sourceRevision
            $configuration.configureOptions
            $inventory
            $compilerRoot.ToLowerInvariant()
            $configuration.recipeVersion
        ) -join "`n"
        $validationHash = (Get-FileHash -LiteralPath (Join-Path $PSScriptRoot 'test-emacs-windows.el') -Algorithm SHA256).Hash
        $sha = [Security.Cryptography.SHA256]::Create()
        try { $identity = [BitConverter]::ToString($sha.ComputeHash([Text.Encoding]::UTF8.GetBytes($recipe))).Replace('-', '').ToLowerInvariant() }
        finally { $sha.Dispose() }
        $acceptedPath = Join-Path $root 'accepted.json'
        $pendingPath = Join-Path $root 'pending.json'
        $reuse = $false
        if (-not $Rebuild -and -not $toolsUpdated -and (Test-Path -LiteralPath $acceptedPath)) {
            $accepted = Get-Content -LiteralPath $acceptedPath -Raw | ConvertFrom-Json
            $acceptedRecipe = @(
                $accepted.sourceRevision
                $accepted.configureOptions
                $accepted.packages
                ([string]$accepted.compilerRoot).ToLowerInvariant()
                # Receipts from the original builder used the same version-1 compile recipe.
                if ($null -ne $accepted.recipeVersion) { $accepted.recipeVersion } else { 1 }
            ) -join "`n"
            if ($acceptedRecipe -ceq $recipe -and (Test-Path -LiteralPath $accepted.executable) -and
                (Get-Item -LiteralPath $initialized).LastWriteTimeUtc -le ([datetime]$accepted.validatedAt).ToUniversalTime() -and
                $accepted.executableSha256 -eq (Get-FileHash -LiteralPath $accepted.executable -Algorithm SHA256).Hash) {
                if ($accepted.validationHash -eq $validationHash) {
                    Write-Host "Reusing verified Emacs: $($accepted.executable)"
                    Write-Host "Build report: $acceptedPath"
                    return
                }
                $reuse = $true
                $executable = $accepted.executable
                $candidate = Split-Path (Split-Path (Split-Path $executable))
                Write-Host "Checks changed; revalidating existing Emacs without rebuilding: $executable"
            }
        }
        $pending = $null
        if (-not $reuse -and -not $Rebuild -and -not $toolsUpdated -and
            (Test-Path -LiteralPath $pendingPath)) {
            $pending = Get-Content -LiteralPath $pendingPath -Raw | ConvertFrom-Json
            if ($pending.identity -eq $identity -and
                (Test-Path -LiteralPath $pending.executable) -and
                (Get-Item -LiteralPath $initialized).LastWriteTimeUtc -le ([datetime]$pending.completedAt).ToUniversalTime() -and
                $pending.executableSha256 -eq (Get-FileHash -LiteralPath $pending.executable -Algorithm SHA256).Hash) {
                $executable = $pending.executable
                $candidate = Split-Path (Split-Path (Split-Path $executable))
                Write-Host "Retrying validation of completed Emacs without rebuilding: $executable"
            } else {
                $pending = $null
            }
        }
        if (-not $reuse -and -not $pending) {
            Invoke-EmacsBuildStage 'source'
            $candidate = Join-Path $root ("builds\{0}-{1}" -f $identity.Substring(0, 12), (Get-Date -Format 'yyyyMMdd-HHmmss'))
            Invoke-EmacsBuildStage 'build' (@($candidate.Replace('\', '/')) + @($configuration.configureOptions))
            $executable = Join-Path $candidate 'install\bin\emacs.exe'
            if (-not (Test-Path -LiteralPath $executable)) { throw "Build produced no Emacs executable at $executable." }
            @{
                identity = $identity
                executable = $executable
                executableSha256 = (Get-FileHash -LiteralPath $executable -Algorithm SHA256).Hash
                completedAt = [datetime]::UtcNow.ToString('o')
            } | ConvertTo-Json | Set-Content -LiteralPath $pendingPath -Encoding UTF8
        }
        $env:PATH = "$(Join-Path $compilerRoot 'bin');$env:PATH"
        $env:EMACS_NATIVE_COMP_ROOT = $compilerRoot
        $env:DOTFILES_EMACS_BUILD = $candidate
        foreach ($name in @('MSYSTEM', 'CHERE_INVOKING')) {
            Set-Item -LiteralPath "Env:$name" -Value $saved[$name]
        }
        Invoke-BuildNative $executable @('-Q', '--batch', '-l', (Join-Path $PSScriptRoot 'test-emacs-windows.el'),
            '-f', 'ert-run-tests-batch-and-exit')
        if ($reuse) {
            $accepted | Add-Member -NotePropertyName validationHash -NotePropertyValue $validationHash -Force
            $accepted | Add-Member -NotePropertyName revalidatedAt -NotePropertyValue ([datetime]::UtcNow.ToString('o')) -Force
            Set-AcceptedEmacs $accepted
            Write-Host "Revalidated existing Emacs: $executable"
            return
        }
        $version = & $executable -Q --batch --eval '(princ emacs-version)'
        if ($LASTEXITCODE -ne 0) { throw 'Could not read the built Emacs version.' }
        $report = [ordered]@{
            identity = $identity
            recipeVersion = $configuration.recipeVersion
            recipeSha256 = $helperHash
            validationHash = $validationHash
            sourceRevision = $configuration.sourceRevision
            executable = $executable
            executableSha256 = (Get-FileHash -LiteralPath $executable -Algorithm SHA256).Hash
            compilerRoot = $compilerRoot
            version = "$version"
            configureOptions = @($configuration.configureOptions)
            packages = @($inventory)
            validatedAt = (Get-Date).ToUniversalTime().ToString('o')
            log = $log
        }
        $reportPath = Join-Path $candidate 'build.json'
        $report | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $reportPath -Encoding UTF8
        Set-AcceptedEmacs $report
        if (Test-Path -LiteralPath $pendingPath) { Remove-Item -LiteralPath $pendingPath }
        Write-Host "Validated source Emacs: $executable"
        Write-Host "Build report: $reportPath"
    } finally {
        if ($lock) { $lock.Dispose() }
        foreach ($name in $names) { Set-Item -LiteralPath "Env:$name" -Value $saved[$name] }
    }
}

function Set-AcceptedEmacs {
    param($Report)
    $Report | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath "$acceptedPath.new" -Encoding UTF8
    if (Test-Path -LiteralPath $acceptedPath) {
        [IO.File]::Replace("$acceptedPath.new", $acceptedPath, "$acceptedPath.previous")
    } else {
        [IO.File]::Move("$acceptedPath.new", $acceptedPath)
    }
}

function Invoke-BuildNative {
    param([string]$Executable, [string[]]$Arguments, [switch]$AllowMsysRestart)
    $script:msysRestartRequested = $false
    $global:LASTEXITCODE = $null
    & $Executable @Arguments 2>&1 | ForEach-Object {
        if ("$_" -match 'To complete this update all MSYS2 processes including this terminal will be closed') {
            $script:msysRestartRequested = $true
        }
        $_
    } | Tee-Object -FilePath $log -Append | Write-Host
    if ($null -eq $LASTEXITCODE -or $LASTEXITCODE -ne 0) {
        if ($AllowMsysRestart -and $LASTEXITCODE -eq 1 -and $script:msysRestartRequested) {
            Write-Host 'MSYS2 requested its core-runtime restart; continuing in a fresh shell.'
        } else { throw "$Executable failed (exit $LASTEXITCODE). See $log" }
    }
}

function Invoke-EmacsBuildStage {
    param([string]$Stage, [string[]]$Extra = @())
    Invoke-BuildNative $bash (@('--login', $buildHelper.Replace('\', '/'), $Stage,
        $root.Replace('\', '/'), $configuration.sourceRevision, "$Jobs") + $Extra) `
        -AllowMsysRestart:($Stage -eq 'update')
}

if ($MyInvocation.InvocationName -ne '.') { Invoke-EmacsBuild @PSBoundParameters }
