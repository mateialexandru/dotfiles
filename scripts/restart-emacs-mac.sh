#!/usr/bin/env bash
# Gracefully restart the launchd-managed Emacs daemon and restore its GUI frame.

set -uo pipefail

seconds="${1:-10}"
service="d12frosted/emacs-plus/emacs-plus@30"
log="/tmp/homebrew.mxcl.emacs-plus.stderr.log"
export ALTERNATE_EDITOR=false

[[ "$seconds" =~ ^[0-9]+$ ]] || { echo "usage: sys restart [seconds]" >&2; exit 2; }
to() { local timeout=$1; shift; perl -e 'alarm shift; exec @ARGV' "$timeout" "$@"; }

old_pid="$(to 6 emacsclient -e '(emacs-pid)' 2>/dev/null)"
gui="$(to 6 emacsclient -e "(and (cl-some (lambda (f) (frame-parameter f 'window-system)) (frame-list)) t)" 2>/dev/null)"

if [ "$(to 6 emacsclient -e "(if (fboundp 'my/graceful-restart) (progn (my/graceful-restart $seconds) t))" 2>/dev/null)" = t ]; then
    echo "→ daemon $old_pid: notifying, saving, going down in ${seconds}s…"
    [ "$gui" = t ] && echo "  (GUI frame open — will reopen it on the fresh daemon)"
    printf "  waiting for fresh daemon"
    new_pid=""
    for _ in $(seq $((seconds + 45))); do
        pid="$(to 3 emacsclient -e '(emacs-pid)' 2>/dev/null || true)"
        [ -n "$pid" ] && [ "$pid" != "$old_pid" ] && { new_pid="$pid"; break; }
        printf '.'
        sleep 1
    done
    echo
    [ -z "$new_pid" ] && { echo "✗ fresh daemon did not come up in time — check $log"; exit 1; }
    echo "✓ daemon restarted ($old_pid → $new_pid)"
    if [ "$gui" = t ]; then
        open -a "Emacs Client" >/dev/null 2>&1 \
            && echo "✓ GUI frame reopened" \
            || echo '✗ GUI reopen failed — run: open -a "Emacs Client"'
    fi
    exit 0
fi
echo "! daemon has no restart fn or is unresponsive — hard reset"
to 20 emacsclient -e '(save-some-buffers t)' >/dev/null 2>&1 || true
brew services stop "$service" >/dev/null 2>&1 || true
pkill -9 -f 'Emacs.*-daemon' 2>/dev/null || true
brew services start "$service" >/dev/null 2>&1 || true
printf "  waiting for daemon"
for _ in $(seq 40); do
    to 3 emacsclient -e t >/dev/null 2>&1 && break
    printf '.'
    sleep 1
done
echo
if to 3 emacsclient -e t >/dev/null 2>&1; then
    echo "✓ daemon up (pid $(to 3 emacsclient -e '(emacs-pid)' 2>/dev/null))"
    if [ "$gui" = t ]; then
        open -a "Emacs Client" >/dev/null 2>&1 && echo "✓ GUI frame reopened" \
            || echo '✗ GUI reopen failed — run: open -a "Emacs Client"'
    fi
else
    echo "✗ daemon DID NOT come up — check $log"
    exit 1
fi
