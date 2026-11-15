#!/usr/bin/env bash
# Install Emacs on macOS via the d12frosted/emacs-plus tap.
#
# Workflow: daemon + Emacs Client.app
#   - `install.sh` starts `emacs --fg-daemon` after Doom is installed and synced.
#   - Only Emacs Client.app is placed in /Applications (copied, so Spotlight indexes it).
#   - New frames open via `emacsclient -c -n` in ~50ms.
#
# See docs/decisions/009-macos-emacs-plus.md for the full rationale.

set -euo pipefail

# Remove the old --cask emacs if present (we are replacing it with emacs-plus@31).
# Only the .app is uninstalled; ~/.config/emacs and ~/.emacs.d are untouched.
if brew list --cask emacs &>/dev/null; then
    echo "Removing old 'brew --cask emacs' (replacing with emacs-plus@31)..."
    brew uninstall --cask emacs
fi
# Clear a stale /Applications/Emacs.app symlink if one is left over (leaves a real .app alone).
if [ -L /Applications/Emacs.app ]; then
    rm /Applications/Emacs.app
fi

if ! brew list emacs-plus@31 &>/dev/null; then
    echo "Installing emacs-plus@31 (native-comp, ImageMagick, retro-gnu-meditate-levitate icon)..."
    # Icon is configured via build.yml (the --with-*-icon brew options are deprecated).
    if [ ! -e "$HOME/.config/emacs-plus/build.yml" ]; then
        mkdir -p "$HOME/.config/emacs-plus"
        cat > "$HOME/.config/emacs-plus/build.yml" <<'EOF'
icon: retro-gnu-meditate-levitate
EOF
    fi
    brew tap d12frosted/emacs-plus
    brew install d12frosted/emacs-plus/emacs-plus@31 --with-imagemagick
fi

# Keep the previous keg installed for rollback, but use the new CLI binaries.
if [ -L "$(brew --prefix)/var/homebrew/linked/emacs-plus@30" ]; then
    brew unlink emacs-plus@30
fi
brew link emacs-plus@31

# Daemon + client workflow: drop /Applications/Emacs.app if present and only install Emacs Client.app.
if [ -e /Applications/Emacs.app ] || [ -L /Applications/Emacs.app ]; then
    echo "Removing /Applications/Emacs.app (daemon + client workflow)..."
    rm -rf /Applications/Emacs.app
fi

EMACS_KEG="$(brew --prefix)/opt/emacs-plus@31"
# Homebrew 7 can require a plist in the keg even when the formula defines a
# service. Some Emacs Plus builds omit it. Keep brew services in charge.
SERVICE_PLIST="$EMACS_KEG/sh.brew.emacs-plus@31.plist"
if [ ! -f "$SERVICE_PLIST" ] && [ ! -f "$EMACS_KEG/homebrew.mxcl.emacs-plus@31.plist" ]; then
    plist="$(mktemp)"
    trap 'rm -f "$plist"' EXIT
    plutil -create xml1 "$plist"
    plutil -insert Label -string sh.brew.emacs-plus@31 "$plist"
    plutil -insert ProgramArguments -array "$plist"
    plutil -insert ProgramArguments.0 -string "$EMACS_KEG/bin/emacs" "$plist"
    plutil -insert ProgramArguments.1 -string --fg-daemon "$plist"
    plutil -insert RunAtLoad -bool YES "$plist"
    plutil -insert KeepAlive -bool YES "$plist"
    plutil -insert StandardOutPath -string "$(brew --prefix)/var/log/emacs-plus@31.stdout.log" "$plist"
    plutil -insert StandardErrorPath -string "$(brew --prefix)/var/log/emacs-plus@31.stderr.log" "$plist"
    install -m 644 "$plist" "$SERVICE_PLIST"
fi

# Drop a stale symlink if one is still there from older install logic.
if [ -L "/Applications/Emacs Client.app" ]; then
    rm "/Applications/Emacs Client.app"
fi
if [ ! -e "/Applications/Emacs Client.app" ]; then
    echo "Copying Emacs Client.app to /Applications..."
    cp -R "$EMACS_KEG/Emacs Client.app" "/Applications/"
fi
# Replace Homebrew's launcher: it invokes `open -a Emacs' even after a
# successful emacsclient call, creating an independent full Emacs process.
CLIENT_APP="/Applications/Emacs Client.app"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
osacompile -o "$CLIENT_APP/Contents/Resources/Scripts/main.scpt" \
    "$SCRIPT_DIR/emacs-client-mac.applescript"
codesign --force --deep --sign - "$CLIENT_APP" >/dev/null
# Force LaunchServices to re-register so the app appears in Cmd+Tab / `open -a`.
LS_REGISTER="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"
if [ -x "$LS_REGISTER" ]; then
    "$LS_REGISTER" -f "/Applications/Emacs Client.app" || true
fi
# Re-index for Spotlight so it shows up in search.
if command -v mdimport &>/dev/null; then
    mdimport "/Applications/Emacs Client.app" || true
fi

# Daemon startup deliberately happens in install.sh after Doom has been cloned,
# synced, and the shell environment has been configured. Starting it here on a
# clean machine would leave Emacs Client.app connected to a vanilla daemon.
