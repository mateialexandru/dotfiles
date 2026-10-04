#!/usr/bin/env bash
# Replace every Emacs instance with one launchd daemon and one client frame.

set -euo pipefail

seconds="${1:-10}"
service="d12frosted/emacs-plus/emacs-plus@30"
server_file="$HOME/.config/emacs/server/server"
export ALTERNATE_EDITOR=false

[[ "$seconds" =~ ^[0-9]+$ ]] || { echo "usage: sys restart [seconds]" >&2; exit 2; }
to() { local limit=$1; shift; perl -e 'alarm shift; exec @ARGV' "$limit" "$@"; }
emacs_pids() { pgrep -u "$(id -u)" -ix emacs || true; }
alive() { kill -0 "$1" 2>/dev/null; }

old_pid="$(to 5 emacsclient -f "$server_file" -e '(emacs-pid)' 2>/dev/null || true)"
if [[ -n "$old_pid" ]]; then
    to 5 emacsclient -f "$server_file" -e \
        "(when (fboundp 'my/os-notify) (my/os-notify \"Doom Emacs\" \"Restarting all Emacs instances in ${seconds}s — save now.\"))" \
        >/dev/null 2>&1 || true
fi
echo "→ closing all Emacs instances in ${seconds}s…"
sleep "$seconds"

# Save the server's file buffers, then stop KeepAlive before terminating every
# Emacs process. Otherwise launchd can respawn the daemon during the cleanup.
if [[ -n "$old_pid" ]]; then
    to 8 emacsclient -f "$server_file" -e '(save-some-buffers t)' >/dev/null 2>&1 || true
fi
brew services stop "$service" >/dev/null 2>&1 || true

for pid in $(emacs_pids); do
    echo "  stopping Emacs $pid"
    kill -TERM "$pid" 2>/dev/null || true
done
for _ in $(seq 1 5); do
    [[ -z "$(emacs_pids)" ]] && break
    sleep 1
done
for pid in $(emacs_pids); do
    echo "  force-closing unresponsive Emacs $pid"
    kill -KILL "$pid" 2>/dev/null || true
done
if [[ -n "$(emacs_pids)" ]]; then
    echo "✗ Emacs is still running; leaving the service stopped" >&2
    exit 1
fi

echo "→ starting one fresh Emacs daemon"
brew services start "$service" >/dev/null
new_pid=""
for _ in $(seq 1 45); do
    new_pid="$(to 3 emacsclient -f "$server_file" -e '(emacs-pid)' 2>/dev/null || true)"
    [[ "$new_pid" =~ ^[0-9]+$ ]] && alive "$new_pid" && break
    new_pid=""
    sleep 1
done
if [[ -z "$new_pid" ]]; then
    echo "✗ fresh Emacs daemon did not become reachable" >&2
    exit 1
fi
echo "✓ daemon ready (PID $new_pid)"

# The installed Emacs Client.app is patched to use this TCP server directly.
# It never falls back to launching a standalone full Emacs.
open -a "Emacs Client"
client_ready=""
for _ in $(seq 1 20); do
    client_ready="$(to 3 emacsclient -f "$server_file" -e \
        "(and (cl-some #'display-graphic-p (frame-list)) t)" \
        2>/dev/null || true)"
    [[ "$client_ready" == t ]] && break
    sleep 1
done
if [[ "$client_ready" != t ]]; then
    echo "✗ Emacs Client.app did not connect to the new daemon" >&2
    exit 1
fi
extra_pids=""
for _ in $(seq 1 5); do
    extra_pids="$(emacs_pids | awk -v daemon="$new_pid" '$1 != daemon')"
    [[ -z "$extra_pids" ]] && break
    sleep 1
done
if [[ -n "$extra_pids" ]]; then
    echo "✗ extra Emacs processes remained after client launch: $extra_pids" >&2
    exit 1
fi
echo "✓ client frame connected to the fresh daemon"
