# Shared interactive Nushell configuration. Loaded from Nu's user autoload
# directory by scripts/install-nushell.sh / .ps1. It does not replace config.nu
# and does not make Nu the login or default shell.

use std/util "path add"

$env.EDITOR = "emacsclient -c"
$env.VISUAL = "emacsclient -c"
$env.ALTERNATE_EDITOR = "emacs"
$env.EMACS_SERVER_FILE = ($nu.home-dir | path join ".config" "emacs" "server" "server")
$env.config.buffer_editor = ["emacsclient", "-c"]

path add ($nu.home-dir | path join ".local" "bin")
path add ($nu.home-dir | path join ".dotnet" "tools")

let lmstudio = ($nu.home-dir | path join ".lmstudio" "bin")
if ($lmstudio | path exists) {
    path add $lmstudio
}

if $nu.os-info.name != "windows" and (which brew | is-not-empty) {
    let dotnet = ((^brew --prefix dotnet | str trim) | path join "libexec")
    if ($dotnet | path exists) {
        $env.DOTNET_ROOT = $dotnet
    }
}

alias e = emacsclient -n
alias et = emacsclient -t
alias g = git
alias gs = git status
alias gd = git diff
alias gl = git log --oneline --graph --all
