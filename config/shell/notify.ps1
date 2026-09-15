# notify.ps1 — Windows toast helper (dot-sourced by shell scripts)
#   . "<dotfiles>/config/shell/notify.ps1"
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
