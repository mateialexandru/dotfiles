# ADR-012: `keeper` — global ops wrapper + `keeper health` confidence pass

**Status:** Accepted
**Date:** 2026-07-26

## Context

Daily Emacs/dotfiles operations (sync after `packages.el`, restart the daemon,
run `doom doctor`, reinstall) were a grab-bag of long commands and script paths
(`~/.config/emacs/bin/doom sync`, `brew services restart …`, `bash install.sh`).
A justfile collected them, reachable only from the repo dir.

Two gaps remained:

- **No global entry.** The recipes were useful but you had to be in `~/Source/dotfiles`
  to run them.
- **No single "is everything in good shape?" answer.** `doom doctor` covers Emacs
  internals but says nothing about the *system* the daemon depends on: the config
  symlink, the daemon itself, LSP servers and formatters on PATH, the Roslyn DLL,
  the Scrim capture pin, the excalidraw toolchain, the tailnet ssh block. These fail
  independently and silently — a missing `pyright` just means Python IDE quietly dies.

## Decision

A single global command, **`keeper`**, that forwards to the repo justfile from
anywhere, plus a **`keeper health`** target that runs one full confidence pass.

### The wrapper

`shell/init.zsh` defines `keeper()` → `just -f ~/Source/dotfiles/justfile -d ~/Source/dotfiles`.
Renamed from `atelier` (commit 68a151c): `keeper` reads as the steward of the rig,
and avoids collision with `workshop`, already owned by the `hack` worktree tooling
(ADR-006). Targets: `health doctor update upgrade sync restart vanilla install`.

`restart` is a shebang (bash) recipe that **drives the close from inside Emacs**, because
only elisp can (a) warn the user, (b) save without prompting, and (c) suppress the
interactive prompts a raw SIGTERM triggers. The recipe (`restart [secs=10]`) calls
`my/graceful-restart` (defined in `config.el`) over `emacsclient`:

1. **Notify + countdown.** `my/os-notify` posts a native OS banner (macOS `osascript
   display notification`; Linux `notify-send`; else `alert`/echo) — "daemon restarting in
   Ns" — then a `run-with-timer` waits `secs` so you can save. The emacsclient call
   returns immediately (it only schedules the timer).
2. **Save everything.** On fire: `save-some-buffers t` (all modified file buffers, no
   prompt) + `recentf`/`savehist`/`desktop` flushes.
3. **Clean, non-interactive kill.** `kill-emacs` under `confirm-kill-processes nil`,
   `confirm-kill-emacs nil`, `kill-emacs-query-functions nil`, **`persp-auto-save-opt 0`**.
   The last is essential: Doom sets `persp-auto-save-opt (if noninteractive 0 1)`, so an
   interactive daemon auto-saves perspectives on kill and *prompts* when no workspace
   exists (dashboard-only) — the exact hang seen before. `kill-emacs` still runs
   `kill-emacs-hook`, so persistence survives; only the persp prompt is bound away.
4. **Respawn.** launchd `KeepAlive=true` restarts the daemon automatically on exit; the
   recipe waits for the fresh one to answer `emacsclient -e t`, nudging with `brew
   services start` only if it doesn't come back.

The recipe tracks the **old** PIDs specifically (KeepAlive respawns a new daemon mid-wait
that must not be reaped) and **force-reaps only an old PID that ignored the clean kill**,
or a wholly unresponsive daemon holding the socket (post-`doom upgrade`: a pre-upgrade
daemon on old loaddefs spamming `Cannot open load file …/+commands` + `clangd killed: 9`
while the service sits in `error`). Every emacsclient call is wrapped in a perl-`alarm`
timeout (`to()`) — stock macOS lacks coreutils `timeout` — so a dead TRAMP buffer or
wedged daemon can't hang the restart.

Rejected: driving the close from the shell via `brew services stop` (its SIGTERM hits the
"Modified buffers exist" / persp prompts with no tty → wedges, orphaning GUI frames — the
original bug); a blanket `-9` (skips `kill-emacs-hook`, can kill mid-save); and a separate
"force" command (makes the *user* diagnose the wedge — the one thing they can't see; the
tool detects a lingering PID instead, keeping rule 1: gate on the symptom).

### `keeper health` (`scripts/keeper-health.sh`)

Full pass, one plain KISS line per check, hard `exit 1` if any check fails, summary
tail. Colour only on a tty. Checks: doom symlink · daemon · native-comp queue ·
doom doctor · core tools · Roslyn DLL · eglot LSP servers · apheleia formatters ·
Emacs Client.app · org-protocol→Scrim pin · excalidraw toolchain · ssh ControlMaster.

Three design rules keep it honest rather than noisy:

1. **Gate on the symptom, not the remedy.** A check fails only on a *live* break, not
   on a missing *mitigation*. The libgccjit `LIBRARY_PATH` bake (ADR-009) is a remedy
   for a boot-abort; if the daemon is reachable, native-comp demonstrably works, so an
   unbaked env is advisory `[..]`, not `[X]`. Same for `doom doctor` warnings (benign
   in bulk) and a draining native-comp queue (transient).

2. **Reachability is the source of truth for the daemon.** `emacsclient --eval t` is
   authoritative — not the `brew services` label, which shows a stale `error`/`stopped`
   after a launchd last-exit while the daemon still serves. Ping first; the service
   label is informational (and a `keeper restart` clears it). See ADR-009 amendment.

3. **Every failure names its fix.** Almost always `keeper install` (the idempotent
   installer *is* the repair path); Scrim/excalidraw point at their standalone scripts.

Three output states: `[ok]` pass · `[X]` fail (red, counts, exits 1) · `[..]` advisory
(latent/transient, never fails). Mac-only checks (daemon service label, libgccjit env,
Client.app, org-protocol pin) are guarded behind `uname == Darwin`.

## Consequences

- `keeper health` from any shell → green/red verdict on the whole rig in one screen;
  suitable as a pre-flight gate (hard exit) or a quick confidence glance.
- Building it surfaced a real latent break: uv tools (`pyright black ruff isort pytest`)
  install to `~/.local/bin`, which was **not on PATH** — so Python LSP and format-on-save
  were silently dead in Doom (exec-path-from-shell inherits the same PATH). Fixed at the
  root by adding `~/.local/bin` to PATH in `shell/init.zsh`, alongside the existing
  `~/.dotnet/tools`. The health check now guards against regressions.
- The check set tracks the install surface: adding an installer-managed tool/LSP/formatter
  means adding one line here, or `keeper health` gives false confidence.
- `keeper doctor` remains the deep dive; `keeper health` is the breadth pass and only
  reports doom doctor's *warning count*, not its full output.

## Alternatives considered

- **Extend `doom doctor`** — it's Emacs-internal and not ours to fork; it can't see the
  daemon service, PATH tools, App Store pins, or the ssh block. `keeper health` wraps it
  as one of thirteen checks rather than replacing it.
- **Per-check exit granularity / machine-readable output** — deferred. Plain lines +
  single aggregate exit code cover the "am I good?" use; JSON can come if something
  consumes it.
- **Gate the daemon on `brew services` state** — rejected; the label lies (stale
  last-exit) while the daemon serves. Ping is truth.
- **Keep `atelier`** — fine name, but undocumented and colliding conceptually with
  `workshop`; `keeper` also signals the new stewardship/health role.

## References

- ADR-009 — macOS emacs-plus@30 daemon/client + the libgccjit `LIBRARY_PATH` fix and
  native-comp queue this health pass probes; amended with the brew-services-vs-ping truth.
- ADR-010 — Safari → org capture via the Scrim `org-protocol` pin that check 11 verifies.
- ADR-008 — excalidraw toolchain (`fswatch` + `excalidraw-cli`) that check 12 verifies.
- ADR-006 — `hack`/`workshop` worktree tooling, whose `workshop` name `keeper` sidesteps.
