#!/usr/bin/env bash
# Download/load Ghostel's pinned native module into Doom's persistent data dir.

set -euo pipefail

if ! emacsclient --eval t >/dev/null 2>&1; then
    echo "Error: an Emacs daemon must be running to provision Ghostel." >&2
    exit 1
fi

result="$(emacsclient --eval \
    '(condition-case err
         (progn
           (require (quote ghostel))
           (let ((ghostel-module-auto-install (quote download)))
             (ghostel--load-module t))
           (if (featurep (quote ghostel-module)) "ready" "not loaded"))
       (error (format "error: %s" (error-message-string err))))')"

if [[ "$result" != '"ready"' ]]; then
    echo "Error: Ghostel native module provisioning failed: $result" >&2
    exit 1
fi

echo "Ghostel native module is ready."
