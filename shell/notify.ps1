# notify.ps1 — Windows toast helper (dot-sourced by shell scripts)
#   . "<dotfiles>/shell/notify.ps1"
#
# Requires: Install-PSResource BurntToast

if ($env:OS -eq "Windows_NT") {
    Import-Module BurntToast -ErrorAction SilentlyContinue
}

function Send-Toast {
    param([string]$Title, [string]$Body)
    if ($env:OS -ne "Windows_NT") { return }
    New-BurntToastNotification -Text $Title, $Body
}

function Invoke-ClaudeNotification {
    <#
    .SYNOPSIS
    Claude Code Notification hook handler. Reads JSON from stdin, shows toast with branch context.
    #>
    $ctx = $input | Out-String | ConvertFrom-Json -ErrorAction SilentlyContinue
    $dir = if ($ctx.cwd) { $ctx.cwd } else { Get-Location }
    $root = git -C $dir rev-parse --show-toplevel 2>$null
    $repo = if ($root) { Split-Path $root -Leaf } else { Split-Path $dir -Leaf }
    $branch = git -C $dir rev-parse --abbrev-ref HEAD 2>$null
    if (-not $branch) { $branch = "?" }
    Send-Toast "Claude Code" "$repo/$branch - needs your attention"
}
