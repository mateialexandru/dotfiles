# Add the public and optional private Nu configuration through user autoload.
# Deliberately does not edit config.nu, PowerShell profiles, or terminal defaults.

$dotfilesDir = Split-Path -Parent $PSScriptRoot
$nu = Get-Command nu -ErrorAction SilentlyContinue
if (-not $nu) {
    Write-Error "nu is not installed"
    exit 1
}

$autoloadDir = (& $nu.Source -n -c '$nu.user-autoload-dirs | first').Trim()
New-Item -ItemType Directory -Force -Path $autoloadDir | Out-Null

function Set-ConfigLink($source, $target) {
    if (Test-Path $target) {
        $item = Get-Item $target -Force
        if ($item.LinkType -eq "SymbolicLink") {
            Remove-Item $target -Force
            New-Item -ItemType SymbolicLink -Path $target -Target $source | Out-Null
            Write-Host "Relinked $target -> $source" -ForegroundColor Gray
        } else {
            Write-Warning "$target exists and is not a symlink; leaving it alone."
        }
    } else {
        New-Item -ItemType SymbolicLink -Path $target -Target $source | Out-Null
        Write-Host "Linked $target -> $source" -ForegroundColor Green
    }
}

$publicConfig = Join-Path $dotfilesDir "config\nushell\dotfiles.nu"
$publicTarget = Join-Path $autoloadDir "10-dotfiles.nu"
Set-ConfigLink $publicConfig $publicTarget

if (Get-Command zoxide -ErrorAction SilentlyContinue) {
    zoxide init nushell | Set-Content -Path (Join-Path $autoloadDir "20-dotfiles-zoxide.nu") -Encoding utf8
    Write-Host "Generated Nushell zoxide integration." -ForegroundColor Green
}

if (Get-Command fzf -ErrorAction SilentlyContinue) {
    fzf --nushell | Set-Content -Path (Join-Path $autoloadDir "30-dotfiles-fzf.nu") -Encoding utf8
    Write-Host "Generated Nushell fzf integration." -ForegroundColor Green
}

$layersDir = Join-Path $env:USERPROFILE ".config\dotfiles\layers.d"
Get-ChildItem -Path $autoloadDir -Filter "50-layer-*.nu" -Force -ErrorAction SilentlyContinue |
    Where-Object { $_.LinkType -eq "SymbolicLink" } |
    Remove-Item -Force
if (Test-Path $layersDir) {
    foreach ($layer in Get-ChildItem -Path $layersDir -Directory) {
        $privateConfig = Join-Path $layer.FullName "shell\init.nu"
        if (Test-Path $privateConfig) {
            Set-ConfigLink $privateConfig (Join-Path $autoloadDir "50-layer-$($layer.Name).nu")
        }
    }
}

Write-Host "Nushell config ready. Run 'nu' to try it; your default shell is unchanged." -ForegroundColor Green
