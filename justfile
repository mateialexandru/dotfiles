doom      := "$HOME/.config/emacs/bin/doom"
emacs_svc := "d12frosted/emacs-plus/emacs-plus@30"
emacs_bin := "$(brew --prefix emacs-plus@30)/bin/emacs"
client_app := "Emacs Client"

default:
    @just --list

# pull latest Doom / package updates (run after init.el or packages.el changes)
update:
    {{doom}} sync

# Restart the Emacs daemon. Emacs saves + closes itself gracefully (OS notification +
# SECS warning); if it can't (no restart fn / unresponsive), fall back to a hard reset.
# If a GUI frame was open, one is reopened on the fresh daemon.
[macos]
restart secs="10":
    #!/usr/bin/env bash
    set -uo pipefail
    svc="{{emacs_svc}}"
    log="/tmp/homebrew.mxcl.emacs-plus.stderr.log"
    # While the daemon is down (post-kill, pre-respawn) emacsclient would fall back to
    # $ALTERNATE_EDITOR and spawn a full GUI Emacs — worse, it hands the `-e` form off as
    # a *filename*, opening a stray "(emacs-pid)" buffer. `false` = fail cleanly instead.
    export ALTERNATE_EDITOR=false
    # perl-alarm timeout — stock macOS has no coreutils `timeout`; keeps emacsclient
    # from hanging on a dead TRAMP buffer or a wedged daemon.
    to() { local t=$1; shift; perl -e 'alarm shift; exec @ARGV' "$t" "$@"; }

    # Note the current daemon PID and whether a GUI frame is open, so we can (a) tell
    # the fresh daemon apart from this one and (b) reopen a GUI frame if there was one.
    oldpid="$(to 6 emacsclient -e '(emacs-pid)' 2>/dev/null)"
    gui="$(to 6 emacsclient -e "(and (cl-some (lambda (f) (frame-parameter f 'window-system)) (frame-list)) t)" 2>/dev/null)"

    # Graceful: Emacs owns the whole close (notify, warn, save, clean kill); launchd
    # (KeepAlive=true) respawns a fresh daemon. We then wait it out and report, so the
    # invocation gives real signal (pid transition + frame reopen), not fire-and-forget.
    if [ "$(to 6 emacsclient -e "(if (fboundp 'my/graceful-restart) (progn (my/graceful-restart {{secs}}) t))" 2>/dev/null)" = t ]; then
      echo "→ daemon $oldpid: notifying, saving, going down in {{secs}}s…"
      [ "$gui" = t ] && echo "  (GUI frame open — will reopen it on the fresh daemon)"
      # Wait for the *fresh* daemon: old and new share the server socket, so key on pid —
      # emacs-pid keeps returning $oldpid until launchd respawns with a new one.
      printf "  waiting for fresh daemon"
      newpid=""
      for _ in $(seq $(({{secs}} + 45))); do
        p="$(to 3 emacsclient -e '(emacs-pid)' 2>/dev/null || true)"
        [ -n "$p" ] && [ "$p" != "$oldpid" ] && { newpid="$p"; break; }
        printf '.'; sleep 1
      done
      echo
      [ -z "$newpid" ] && { echo "✗ fresh daemon did not come up in time — check $log"; exit 1; }
      echo "✓ daemon restarted ($oldpid → $newpid)"
      if [ "$gui" = t ]; then
        open -a "{{client_app}}" >/dev/null 2>&1 \
          && echo "✓ GUI frame reopened" \
          || echo "✗ GUI reopen failed — run: open -a \"{{client_app}}\""
      fi
      exit 0
    fi

    # Nuclear: no restart fn or no response. Best-effort save if it still answers, then
    # force a clean slate.
    echo "! daemon has no restart fn or is unresponsive — hard reset"
    to 20 emacsclient -e '(save-some-buffers t)' >/dev/null 2>&1 || true
    brew services stop "$svc" >/dev/null 2>&1 || true
    pkill -9 -f 'Emacs.*-daemon' 2>/dev/null || true
    brew services start "$svc" >/dev/null 2>&1 || true
    printf "  waiting for daemon"
    for _ in $(seq 40); do to 3 emacsclient -e t >/dev/null 2>&1 && break; printf '.'; sleep 1; done
    echo
    if to 3 emacsclient -e t >/dev/null 2>&1; then
      echo "✓ daemon up (pid $(to 3 emacsclient -e '(emacs-pid)' 2>/dev/null))"
      if [ "$gui" = t ]; then
        open -a "{{client_app}}" >/dev/null 2>&1 && echo "✓ GUI frame reopened" \
          || echo "✗ GUI reopen failed — run: open -a \"{{client_app}}\""
      fi
    else
      echo "✗ daemon DID NOT come up — check $log"; exit 1
    fi

# launch a clean Emacs (no config) for debugging
[macos]
vanilla:
    {{emacs_bin}} -Q &

# doom upgrade — update Doom and all packages
upgrade:
    {{doom}} upgrade

# doom doctor — diagnose environment issues
doctor:
    {{doom}} doctor

# health — full confidence pass (symlink, daemon, env, doctor, tools, lsp, ssh); exit 1 on any fail
health:
    bash scripts/keeper-health.sh

# llm start|stop|status|mirror [model] — the managed Ollama runtime (:11434, on-demand).
#   start [model]  start the daemon (no login registration) + warm a model resident
#   stop           stop the daemon (frees RAM)
#   status         is the endpoint up? show pulled + loaded models
#   mirror         (re)symlink Ollama's GGUF models into LM Studio (labelled `ollama`),
#                  pruning stale links — best-effort convenience, self-healing
[macos]
llm action="status" model="gpt-oss:20b":
    #!/usr/bin/env bash
    set -uo pipefail
    ep="http://localhost:11434"
    up() { curl -fsS "$ep/api/tags" >/dev/null 2>&1; }
    case "{{action}}" in
      start)
        if ! up; then
          brew services run ollama >/dev/null 2>&1 || true
          printf "  waiting for endpoint"
          for _ in $(seq 30); do up && break; printf '.'; sleep 1; done; echo
          up || { echo "✗ Ollama did not come up — check: brew services list | grep ollama"; exit 1; }
        fi
        echo "✓ Ollama up ($ep)"
        # Empty prompt loads the model into memory without generating; keep_alive -1 pins
        # it resident for the whole session (until `keeper llm stop`).
        printf "  warming {{model}}…"
        if curl -fsS "$ep/api/generate" -d '{"model":"{{model}}","prompt":"","keep_alive":-1}' >/dev/null 2>&1; then
          echo " ✓ resident"
        else echo " ✗ (is {{model}} pulled? \`keeper llm status\`)"; fi
        ;;
      stop)
        brew services stop ollama ;;
      status)
        if up; then echo "✓ Ollama up ($ep)"; ollama list; echo; echo "loaded:"; ollama ps
        else echo "✗ Ollama down — run \`keeper llm start\`"; exit 1; fi ;;
      mirror)
        # Ollama can't share files (content-addressed store), so we symlink each GGUF
        # model blob into LM Studio's tree. Walk on-disk manifests (no daemon needed);
        # prune any link that no longer resolves.
        base="$HOME/.lmstudio/models/ollama"; blobs="$HOME/.ollama/models/blobs"
        mroot="$HOME/.ollama/models/manifests"; mkdir -p "$base"
        for d in "$base"/*/; do
          [ -d "$d" ] || continue
          f=$(ls "$d"*.gguf 2>/dev/null | head -1)
          { [ -z "$f" ] || [ ! -e "$f" ]; } && { rm -rf "$d"; echo "  pruned $(basename "$d")"; }
        done
        [ -d "$mroot" ] || { echo "note: no Ollama models yet"; exit 0; }
        find "$mroot" -type f 2>/dev/null | while read -r man; do
          tag=$(basename "$man"); name=$(basename "$(dirname "$man")")
          dig=$(jq -r '.layers[]|select(.mediaType=="application/vnd.ollama.image.model").digest' "$man" 2>/dev/null | sed 's/sha256://')
          [ -z "$dig" ] && continue
          [ -f "$blobs/sha256-$dig" ] || continue
          safe="$name-$tag"; mkdir -p "$base/$safe"
          ln -sf "$blobs/sha256-$dig" "$base/$safe/$safe.gguf"
          echo "  mirrored $name:$tag"
        done
        echo "✓ mirror synced → $base (rescan in LM Studio to see them)" ;;
      *) echo "usage: keeper llm [start|stop|status|mirror] [model]"; exit 2 ;;
    esac

# Full install (packages, Doom Emacs, symlink, fonts)
install:
    bash install.sh
