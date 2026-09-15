#!/usr/bin/env bash
# Dotfiles Install Script for macOS and Linux
# Installs Gemini CLI, Doom Emacs, and essential tools

set -euo pipefail

TARGET_DIR="$HOME/Source/dotfiles"
REPO_URL="https://github.com/mateialexandru/dotfiles"
ORIGINAL_ARGS=("$@")
INSTALL_LLM=true

usage() {
    cat <<'EOF'
Usage: install.sh [--skip-llm]

  --skip-llm  Install Ollama and LM Studio later; skip the ~32 GB model pull.
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --skip-llm) INSTALL_LLM=false ;;
        -h|--help) usage; exit 0 ;;
        *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
    esac
    shift
done

# --- Bootstrapper Mode ---
# Determine where this script is currently located
SCRIPT_PATH="${BASH_SOURCE[0]:-$0}"
IS_LOCAL=false

if [[ -f "$SCRIPT_PATH" ]]; then
    # The script exists as a file. Check if it's inside the dotfiles repository.
    TEMP_DIR="$(cd "$(dirname "$SCRIPT_PATH")" &>/dev/null && pwd || echo "")"
    if [[ -n "$TEMP_DIR" ]]; then
        if [[ -f "$TEMP_DIR/doom/init.el" ]]; then
            IS_LOCAL=true
            DOTFILES_DIR="$TEMP_DIR"
        elif [[ -f "$TEMP_DIR/../doom/init.el" ]]; then
            IS_LOCAL=true
            DOTFILES_DIR="$(cd "$TEMP_DIR/.." && pwd)"
        fi
    fi
fi

if ! $IS_LOCAL; then
    echo "=> Bootstrapping dotfiles installation..."

    # 1. Install prerequisites (git, curl, unzip)
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if ! command -v git &>/dev/null; then
            echo "=> Installing macOS Command Line Tools (required for Git)..."
            xcode-select --install
            echo "=> Please run this script again after the installation finishes."
            exit 1
        fi
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if ! command -v git &>/dev/null || ! command -v curl &>/dev/null || ! command -v unzip &>/dev/null; then
            echo "=> Installing prerequisites (git, curl, unzip)..."
            if command -v apt-get &>/dev/null; then
                sudo apt-get update && sudo apt-get install -y git curl unzip
            elif command -v dnf &>/dev/null; then
                sudo dnf install -y git curl unzip
            elif command -v pacman &>/dev/null; then
                sudo pacman -Sy --noconfirm git curl unzip
            else
                echo "=> Please install git, curl, and unzip manually before continuing."
                exit 1
            fi
        fi
    fi

    # 2. Clone or update repository
    if [ ! -d "$TARGET_DIR/.git" ]; then
        echo "=> Cloning $REPO_URL to $TARGET_DIR..."
        mkdir -p "$(dirname "$TARGET_DIR")"
        git clone "$REPO_URL" "$TARGET_DIR"
    else
        echo "=> Repository already exists at $TARGET_DIR. Pulling latest changes..."
        git -C "$TARGET_DIR" pull
    fi

    # 3. Hand off to the local install.sh
    echo "=> Executing local install.sh..."
    cd "$TARGET_DIR"
    exec ./install.sh "${ORIGINAL_ARGS[@]}"
fi

# --- Main Installation Logic ---
# Ensure we are executing from the repository root
cd "$DOTFILES_DIR"

# Print a numbered marker before each major phase so progress is easy to track.
STEP_NUM=0
step() {
    STEP_NUM=$((STEP_NUM + 1))
    echo
    echo "==> [step $STEP_NUM] $*"
}

# --- Homebrew (Install if missing) ---

step "Homebrew"
if ! command -v brew &>/dev/null; then
    echo "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add brew to PATH for the current session
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if [[ -x /opt/homebrew/bin/brew ]]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        elif [[ -x /usr/local/bin/brew ]]; then
            eval "$(/usr/local/bin/brew shellenv)"
        else
            echo "Homebrew installed, but brew was not found in a standard macOS prefix." >&2
            exit 1
        fi
    else
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    fi
fi

# --- Packages ---

step "Core packages, tools, Java, and fonts (Homebrew)"

# Core Runtimes & Build Tools
brew install node dotnet cmake ninja llvm devcontainer just libtool powershell

# Everyday tools & Utilities
brew install fzf zoxide gh git ripgrep fd jq universal-ctags poppler pandoc grip gnuplot shellcheck rust rust-analyzer \
    lua-language-server wordnet shfmt graphviz dockerfmt clang-format

# Brave browser (macOS cask only — not available via Linuxbrew)
if [[ "$OSTYPE" == "darwin"* ]]; then
    brew install --cask brave-browser
fi

# Java (Temurin LTS on macOS — registers with /usr/libexec/java_home; openjdk on Linux)
if [[ "$OSTYPE" == "darwin"* ]]; then
    brew install --cask temurin@21
else
    brew install openjdk
    # Homebrew's OpenJDK is keg-only. Make it available to the remaining install
    # steps (TLA+ and PlantUML) and to Doom's dependency checks.
    OPENJDK_PREFIX="$(brew --prefix openjdk)"
    export PATH="$OPENJDK_PREFIX/bin:$PATH"
fi

# Fonts (cross-platform: brew casks on macOS, dnf + Nerd Font downloader on Linux)
echo "Installing fonts..."
if [[ "$OSTYPE" == "darwin"* ]]; then
    brew install --cask \
        font-jetbrains-mono-nerd-font \
        font-iosevka-nerd-font \
        font-iosevka-aile \
        font-iosevka-etoile \
        font-fira-code-nerd-font \
        font-fira-sans \
        font-caskaydia-cove-nerd-font \
        font-victor-mono-nerd-font \
        font-monaspace-nf \
        font-maple-mono-nf \
        font-ibm-plex-mono \
        font-ibm-plex-sans \
        font-inter \
        font-symbols-only-nerd-font
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Userspace install into ~/.local/share/fonts — works on immutable hosts
    # (Bluefin/rpm-ostree) without root or reboot. Idempotent.
    bash "$DOTFILES_DIR/scripts/install-fonts-linux.sh"
fi

# Emacs
step "Emacs"
if [[ "$OSTYPE" == "darwin"* ]]; then
    bash "$DOTFILES_DIR/scripts/install-emacs-mac.sh"
    # duti pins URL-scheme handlers. emacs-plus's "Emacs Client.app" claims the
    # org-protocol:// scheme, which collides with Scrim (the Captee capture proxy)
    # and steals captures — emacsclient then spawns a stray Emacs. The pin itself
    # lives in scripts/install-scrim-captee-mac.sh (it needs Scrim installed first);
    # here we just ensure the tool is present. See decisions/010-safari-org-capture.md.
    brew install duti
else
    if ! command -v emacs &>/dev/null; then
        echo "Installing Emacs..."
        brew install emacs
    fi
fi

# Excalidraw toolchain (macOS: Chrome PWA editor + excalidraw_export + fonts)
if [[ "$OSTYPE" == "darwin"* ]]; then
    step "Excalidraw prereqs"
    bash "$DOTFILES_DIR/scripts/install-excalidraw-mac.sh"
fi

# Local LLM layer (Ollama managed GGUF endpoint + LM Studio GUI, user-managed MLX)
step "Local LLM (Ollama + LM Studio)"
if ! $INSTALL_LLM; then
    echo "Skipping local LLM installation (--skip-llm)."
elif [[ "$OSTYPE" == "darwin"* ]]; then
    bash "$DOTFILES_DIR/scripts/install-llm-mac.sh"
else
    echo "Local LLM: install Ollama from https://ollama.com and LM Studio from https://lmstudio.ai (no linuxbrew formulae)."
fi

# Global NPM Packages (AI & Language Servers)
step "NPM global packages"
npm install -g \
    @google/gemini-cli @anthropic-ai/claude-code yaml-language-server \
    @mermaid-js/mermaid-cli @github/copilot markdownlint-cli \
    vscode-langservers-extracted bash-language-server \
    dockerfile-language-server-nodejs typescript-language-server

# C# Tooling
step "C# tools (csharpier, Roslyn LSP)"
DOTNET_ROOT="$(brew --prefix dotnet)/libexec"
export DOTNET_ROOT
export PATH="$HOME/.dotnet/tools:$PATH"
dotnet tool install -g csharpier || dotnet tool update -g csharpier
# Install Roslyn LSP DLL
bash "$DOTFILES_DIR/scripts/install-roslyn-lsp.sh"

# Symlink dockerfmt as dockfmt (what Doom's docker module expects)
ln -sf "$(brew --prefix)/bin/dockerfmt" "$(brew --prefix)/bin/dockfmt"

# --- TLA+ Tooling (tla2tools.jar + tla-bin wrappers) ---
echo "Installing TLA+ tools..."
bash "$DOTFILES_DIR/scripts/install-tlaplus.sh"

# --- Python Tooling (uv owns interpreters/venvs; ruff + pyright are global) ---
# Per-project tools (pytest, mypy, ...) come from the project's own .venv and are
# invoked with `uv run'. See decisions/015-python-uv.md.
echo "Installing uv and Python editor tools..."
brew install uv
for pkg in pyright ruff; do
    uv tool install --force "$pkg"
done
for pkg in black isort pyflakes pytest; do
    uv tool uninstall "$pkg" >/dev/null 2>&1 || true
done

# --- Editor Setup ---

step "Doom Emacs"
bash "$DOTFILES_DIR/scripts/install-doom.sh"
bash "$DOTFILES_DIR/scripts/install-plantuml.sh"

# --- Shell init (zsh) ---
# init.zsh holds shared aliases, $EDITOR, zoxide/fzf, and the `keeper` command.
# Source it from ~/.zshrc so every shell picks it up.
step "Shell init (source init.zsh from ~/.zshrc)"
ZSHRC="$HOME/.zshrc"
ZSH_SOURCE_LINE='source "$HOME/Source/dotfiles/shell/init.zsh"'
if grep -qF "$ZSH_SOURCE_LINE" "$ZSHRC" 2>/dev/null; then
    echo "init.zsh already sourced from $ZSHRC; skipping."
else
    echo "Adding init.zsh source line to $ZSHRC"
    echo "$ZSH_SOURCE_LINE" >> "$ZSHRC"
fi

step "Hack worktree tooling"
bash "$DOTFILES_DIR/scripts/install-hack.sh"

# Start only after Doom and the login-shell environment are ready. Do not hide
# failures: a successful install must leave Emacs Client.app able to connect.
if [[ "$OSTYPE" == "darwin"* ]]; then
    step "Emacs daemon"
    brew services restart d12frosted/emacs-plus/emacs-plus@30 || \
        brew services start d12frosted/emacs-plus/emacs-plus@30
fi

# Universal Ctags preloads *.ctags files from this XDG directory.
step "Universal Ctags config (~/.config/ctags)"
CTAGS_CONFIG_SRC="$DOTFILES_DIR/ctags.d"
CTAGS_CONFIG_DST="$HOME/.config/ctags"
mkdir -p "$(dirname "$CTAGS_CONFIG_DST")"
if [[ -L "$CTAGS_CONFIG_DST" ]]; then
    ln -sfn "$CTAGS_CONFIG_SRC" "$CTAGS_CONFIG_DST"
    echo "Relinked $CTAGS_CONFIG_DST -> $CTAGS_CONFIG_SRC"
elif [[ -e "$CTAGS_CONFIG_DST" ]]; then
    echo "WARNING: $CTAGS_CONFIG_DST exists and is not a symlink; leaving it alone."
else
    ln -s "$CTAGS_CONFIG_SRC" "$CTAGS_CONFIG_DST"
    echo "Linked $CTAGS_CONFIG_DST -> $CTAGS_CONFIG_SRC"
fi

# --- SSH ControlMaster for tailnet hosts ---
# One wildcard block multiplexes connections to every current/future *.ts.net
# host, so TRAMP and the Emacs vterm->tmux helper reconnect near-instantly.
step "SSH ControlMaster (*.ts.net)"
SSH_CONFIG="$HOME/.ssh/config"
mkdir -p "$HOME/.ssh" && chmod 700 "$HOME/.ssh"
if [[ ! -f "$SSH_CONFIG" ]] || ! grep -q 'Host \*.ts.net' "$SSH_CONFIG"; then
    echo "Adding 'Host *.ts.net' ControlMaster block to $SSH_CONFIG"
    cat >> "$SSH_CONFIG" <<'EOF'

Host *.ts.net
    ControlMaster auto
    ControlPath ~/.ssh/cm-%r@%h:%p
    ControlPersist 10m
    ServerAliveInterval 60
    ServerAliveCountMax 3
EOF
    chmod 600 "$SSH_CONFIG"
else
    echo "ControlMaster block already present; skipping."
fi

# --- Global gitignore ---
# ~/.config/git/ignore is git's XDG default, so linking the repo's copy there
# needs no core.excludesfile setting. It keeps agent scratch (.gptel/, .claude/)
# out of every repo without editing each one's .gitignore. See ADR-014.
step "Global gitignore (~/.config/git/ignore)"
GIT_IGNORE_SRC="$DOTFILES_DIR/git/ignore"
GIT_IGNORE_DST="$HOME/.config/git/ignore"
mkdir -p "$(dirname "$GIT_IGNORE_DST")"
if [[ -L "$GIT_IGNORE_DST" ]]; then
    ln -sfn "$GIT_IGNORE_SRC" "$GIT_IGNORE_DST"
    echo "Relinked $GIT_IGNORE_DST -> $GIT_IGNORE_SRC"
elif [[ -e "$GIT_IGNORE_DST" ]]; then
    echo "WARNING: $GIT_IGNORE_DST exists and is not a symlink; leaving it alone."
    echo "         Merge $GIT_IGNORE_SRC into it by hand, or move it aside and re-run."
else
    ln -s "$GIT_IGNORE_SRC" "$GIT_IGNORE_DST"
    echo "Linked $GIT_IGNORE_DST -> $GIT_IGNORE_SRC"
fi

echo
echo "==> Done! Dotfiles installed."
