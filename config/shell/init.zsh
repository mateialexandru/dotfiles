# init.zsh --- Common Zsh configuration for dotfiles

# Environment Variables
export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR="emacs"
export VISUAL="emacsclient -c"

# Emacs server runs over TCP (server-use-tcp t) so the sandboxed Scrim
# org-protocol proxy can connect. Point emacsclient at the TCP auth file so
# EDITOR/e/et keep working without a unix socket.
export EMACS_SERVER_FILE="$HOME/.config/emacs/server/server"

# Add brew to PATH if on macOS
if [[ "$OSTYPE" == "darwin"* ]]; then
    if [[ -f /opt/homebrew/bin/brew ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [[ -f /usr/local/bin/brew ]]; then
        eval "$(/usr/local/bin/brew shellenv)"
    fi
fi

# libgccjit (native-comp) on Apple Silicon: the JIT's child gcc can't find
# libgcc's libemutls_w.a, which lives under a target-tuple subdir. Emacs picks
# this up via exec-path-from-shell (see config/doom/config-macos.el, ADR-009).
if [[ "$OSTYPE" == "darwin"* ]]; then
    for _libemutls in /opt/homebrew/lib/gcc/current/gcc/*/*/libemutls_w.a(N); do
        export LIBRARY_PATH="${_libemutls:h}${LIBRARY_PATH:+:$LIBRARY_PATH}"
    done
    unset _libemutls
fi

# dotnet (brew-managed runtime + global tools)
if command -v brew &>/dev/null && [[ -d "$(brew --prefix dotnet)/libexec" ]]; then
    export DOTNET_ROOT="$(brew --prefix dotnet)/libexec"
fi
export PATH="$HOME/.dotnet/tools:$PATH"

# Linuxbrew's OpenJDK is keg-only, so expose it explicitly on Linux.
if [[ "$OSTYPE" == "linux-gnu"* ]] && command -v brew &>/dev/null \
    && [[ -d "$(brew --prefix openjdk)/bin" ]]; then
    export PATH="$(brew --prefix openjdk)/bin:$PATH"
fi

# uv tools (pyright, black, ruff, isort, pytest) + TLA+ wrappers land here
export PATH="$HOME/.local/bin:$PATH"

# LM Studio CLI (`lms`) — local LLM control (ADR-013)
[[ -d "$HOME/.lmstudio/bin" ]] && export PATH="$HOME/.lmstudio/bin:$PATH"

# Zoxide initialization
if command -v zoxide &>/dev/null; then
    eval "$(zoxide init zsh)"
fi

# FZF initialization
if command -v fzf &>/dev/null; then
    source <(fzf --zsh)
fi

# Aliases
alias e="emacsclient -n"
alias et="emacsclient -t"
alias g="git"
alias gs="git status"
alias gd="git diff"
alias gl="git log --oneline --graph --all"

# keeper — global entry to the dotfiles justfile (see `keeper --list`)
keeper() { just -f "$HOME/Source/dotfiles/justfile" -d "$HOME/Source/dotfiles" "$@"; }

# Load shell extensions from each optional private layer in lexical order.
for _dotfiles_layer in "$HOME/.config/dotfiles/layers.d"/*(N-/); do
    [[ -r "$_dotfiles_layer/shell/init.zsh" ]] && source "$_dotfiles_layer/shell/init.zsh"
done
unset _dotfiles_layer

# Prompt (Simple)
PROMPT='%F{blue}%~%f %F{yellow}❯%f '
