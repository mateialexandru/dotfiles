#!/usr/bin/env bash
# Open the "Scrim + Captee for Emacs" bundle on the Mac App Store.
#
# Captee shares the current page (URL + title + selection) from the macOS
# Share Menu as an org-protocol:// request; Scrim is the notarized proxy that
# relays it to a running Emacs server. Both are one-time paid purchases and
# cannot be installed non-interactively, so this just opens the page and
# prints the one-time setup. Standalone — not wired into install.sh.

set -euo pipefail

BUNDLE_ID=1810494622   # "Scrim + Captee for Emacs"

# Pin the org-protocol:// scheme to Scrim. emacs-plus's "Emacs Client.app" also
# claims org-protocol (CFBundleURLSchemes in its Info.plist); with no explicit
# default handler, LaunchServices routes captures to it instead of Scrim, which
# fails under the TCP server and spawns a stray Emacs. Pin it to Scrim — the pin
# lives in LaunchServices and survives emacs-plus upgrades. (duti is installed by
# install.sh.) Two-arg form = URL scheme; a third arg would mean an extension.
pin_org_protocol() {
    if ! command -v duti >/dev/null 2>&1; then
        echo "duti not found — install it (brew install duti) then re-run, or org-protocol:// may route to Emacs Client.app."
        return
    fi
    if [ -d "/Applications/Scrim.app" ]; then
        duti -s com.yummymelon.scrim org-protocol && \
            echo "Pinned org-protocol:// → Scrim."
    fi
}

if [ -d "/Applications/Captee.app" ] && [ -d "/Applications/Scrim.app" ]; then
    echo "Captee + Scrim already installed."
    pin_org_protocol
    exit 0
fi

echo "Opening the Scrim + Captee bundle — purchase + install there, then:"
cat <<'NEXT'
  1. Launch Scrim once. It owns the org-protocol:// scheme and relays captures to
     your running Emacs server. If Scrim can't reach the server (non-default file
     location), use Scrim menu → Setup → select ~/.config/emacs/server/server.
  2. Captee → Settings: Format = Org, Payload = Capture, Use = Protocol,
     capture template key = L (defaults to ~/Documents/org/inbox.org).
  3. Capture: Safari Share button → Captee → Share to Emacs.
     (Optional: System Settings → Keyboard → Shortcuts to bind a hotkey.)

  Then re-run this script (or: duti -s com.yummymelon.scrim org-protocol) to pin
  the scheme to Scrim — otherwise emacs-plus's Emacs Client.app may intercept it.
NEXT

open "macappstore://apps.apple.com/app-bundle/id${BUNDLE_ID}" || true
