# setup-profile.ps1 - Migrate PowerShell profiles from OneDrive to local (non-roaming) paths
#
# Creates stub profiles at the OneDrive-synced locations that dot-source
# from $env:LOCALAPPDATA\PowerShell\{5,7}\profile.ps1.
# Existing profile content is moved to the local path on first run.

param(
    [switch]$Force
)

$ErrorActionPreference = 'Stop'

function Move-ProfileLocal {
    param(
        [string]$RoamingProfile,
        [string]$LocalProfile,
        [string]$Label
    )

    $localDir = Split-Path $LocalProfile
    if (-not (Test-Path $localDir)) {
        New-Item -ItemType Directory -Path $localDir -Force | Out-Null
    }

    # Already a stub - nothing to do
    if ((Test-Path $RoamingProfile) -and -not $Force) {
        $content = Get-Content $RoamingProfile -Raw -ErrorAction SilentlyContinue
        if ($content -and $content.Contains($LocalProfile)) {
            Write-Host "${Label}: already using local profile" -ForegroundColor Gray
            return
        }
    }

    # Migrate existing content to local profile
    if (Test-Path $RoamingProfile) {
        $existing = Get-Content $RoamingProfile -Raw -ErrorAction SilentlyContinue
        if ($existing -and $existing.Trim().Length -gt 0) {
            if (Test-Path $LocalProfile) {
                $localContent = Get-Content $LocalProfile -Raw -ErrorAction SilentlyContinue
                if ($localContent -and $localContent.Trim().Length -gt 0) {
                    Write-Host "${Label}: WARNING - both roaming and local profiles have content" -ForegroundColor Yellow
                    Write-Host "  Roaming: $RoamingProfile" -ForegroundColor Yellow
                    Write-Host "  Local:   $LocalProfile" -ForegroundColor Yellow
                    Write-Host "  Appending roaming content to local profile" -ForegroundColor Yellow
                    Add-Content -Path $LocalProfile -Value "`n# --- migrated from roaming profile ---`n$existing"
                } else {
                    Set-Content -Path $LocalProfile -Value $existing -NoNewline
                }
            } else {
                Set-Content -Path $LocalProfile -Value $existing -NoNewline
            }
            Write-Host "${Label}: migrated content to $LocalProfile" -ForegroundColor Green
        }
    }

    # Ensure local profile exists
    if (-not (Test-Path $LocalProfile)) {
        New-Item -ItemType File -Path $LocalProfile -Force | Out-Null
    }

    # Write stub at roaming path
    $stub = "# Stub - real profile lives outside OneDrive to avoid sync overhead.`n" +
            "# Edit: $LocalProfile`n" +
            "if (Test-Path `"$LocalProfile`") { . `"$LocalProfile`" }"
    Set-Content -Path $RoamingProfile -Value $stub
    Write-Host "${Label}: stub written at $RoamingProfile" -ForegroundColor Green
}

# --- PowerShell 5.1 (WindowsPowerShell) ---
$roaming5 = [System.IO.Path]::Combine(
    [Environment]::GetFolderPath('MyDocuments'),
    'WindowsPowerShell',
    'Microsoft.PowerShell_profile.ps1'
)
$local5 = Join-Path $env:LOCALAPPDATA 'PowerShell\5\profile.ps1'
Move-ProfileLocal -RoamingProfile $roaming5 -LocalProfile $local5 -Label 'PS 5.1'

# --- PowerShell 7+ (PowerShell) ---
$roaming7 = [System.IO.Path]::Combine(
    [Environment]::GetFolderPath('MyDocuments'),
    'PowerShell',
    'Microsoft.PowerShell_profile.ps1'
)
$local7 = Join-Path $env:LOCALAPPDATA 'PowerShell\7\profile.ps1'
Move-ProfileLocal -RoamingProfile $roaming7 -LocalProfile $local7 -Label 'PS 7  '

Write-Host "`nDone. Local profiles:" -ForegroundColor Cyan
Write-Host "  PS 5.1: $local5"
Write-Host "  PS 7+ : $local7"
