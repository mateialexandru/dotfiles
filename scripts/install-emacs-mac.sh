#!/usr/bin/env bash
# Install Emacs on macOS via the d12frosted/emacs-plus tap.
#
# Workflow: daemon + Emacs Client.app
#   - `brew services` runs `emacs --fg-daemon` at login.
#   - Only Emacs Client.app is placed in /Applications (copied, so Spotlight indexes it).
#   - New frames open via `emacsclient -c -n` in ~50ms.
#
# See decisions/009-macos-emacs-plus.md for the full rationale.

set -euo pipefail

# Remove the old --cask emacs if present (we are replacing it with emacs-plus@30).
# Only the .app is uninstalled; ~/.config/emacs and ~/.emacs.d are untouched.
if brew list --cask emacs &>/dev/null; then
    echo "Removing old 'brew --cask emacs' (replacing with emacs-plus@30)..."
    brew uninstall --cask emacs
fi
# Clear a stale /Applications/Emacs.app symlink if one is left over (leaves a real .app alone).
if [ -L /Applications/Emacs.app ]; then
    rm /Applications/Emacs.app
fi

if ! brew list emacs-plus@30 &>/dev/null; then
    echo "Installing emacs-plus@30 (native-comp, ImageMagick, retro-gnu-meditate-levitate icon)..."
    # Icon is configured via build.yml (the --with-*-icon brew options are deprecated).
    mkdir -p "$HOME/.config/emacs-plus"
    cat > "$HOME/.config/emacs-plus/build.yml" <<'EOF'
icon: retro-gnu-meditate-levitate
EOF
    brew tap d12frosted/emacs-plus
    brew install emacs-plus@30 --with-imagemagick
fi

# Daemon + client workflow: drop /Applications/Emacs.app if present and only install Emacs Client.app.
if [ -e /Applications/Emacs.app ] || [ -L /Applications/Emacs.app ]; then
    echo "Removing /Applications/Emacs.app (daemon + client workflow)..."
    rm -rf /Applications/Emacs.app
fi

EMACS_KEG="$(brew --prefix)/opt/emacs-plus@30"
# Drop a stale symlink if one is still there from older install logic.
if [ -L "/Applications/Emacs Client.app" ]; then
    rm "/Applications/Emacs Client.app"
fi
if [ ! -e "/Applications/Emacs Client.app" ]; then
    echo "Copying Emacs Client.app to /Applications..."
    cp -R "$EMACS_KEG/Emacs Client.app" "/Applications/"
fi
# Force LaunchServices to re-register so the app appears in Cmd+Tab / `open -a`.
LS_REGISTER="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"
if [ -x "$LS_REGISTER" ]; then
    "$LS_REGISTER" -f "/Applications/Emacs Client.app" || true
fi
# Re-index for Spotlight so it shows up in search.
if command -v mdimport &>/dev/null; then
    mdimport "/Applications/Emacs Client.app" || true
fi

# The daemon is started by launchd, so it inherits a bare
# /usr/bin:/bin:/usr/sbin:/sbin and no LIBRARY_PATH. `shell/init.zsh` exports
# both, and `doom/config-macos.el` pulls them in via exec-path-from-shell at
# daemon boot (ADR-009). Doom's `.local/env` file is no longer read by Doom, so
# it is not used for this.

# Start the Emacs daemon now and on every login (idempotent).
brew services restart d12frosted/emacs-plus/emacs-plus@30 || \
    brew services start d12frosted/emacs-plus/emacs-plus@30 || true
