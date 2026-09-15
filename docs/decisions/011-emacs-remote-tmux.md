# ADR-011: Effortless Emacs → remote workflow (tuned TRAMP + Ghostel/tmux over Tailscale)

**Status:** Accepted
**Date:** 2026-05-25
**Revised:** 2026-09-15

## Context

Daily work increasingly targets headless systems on a tailnet. Two needs were
conflated and hurt:

- **Editing remote files** felt sluggish. A bare TRAMP round-trip is ~50–100ms
  vs ~1ms locally; with vc probing, locking, and per-file overhead it stacks up
  (Magit status on a remote repo: 10–20s). Reconnect cost dominated the feel —
  the measured ~11ms LAN/Tailscale RTT was never the problem.
- **Running things remotely** had no persistence. A plain `ssh` shell (in or out
  of Emacs) dies on disconnect/sleep/network change, losing long jobs and state.

We wanted both to be one-keystroke from the mac's GUI Emacs, with **no per-host
configuration to maintain**.

## Decision

A **two-lane hybrid**, with the tailnet as the host source of truth.

Doom's official `(ghostel +everywhere)` module is the terminal layer. It supplies
the pinned Ghostel package plus Evil, Eshell, comint, and compilation integration.
The prebuilt native module is downloaded into Doom's persistent data directory,
outside the package checkout, so an upgrade cannot overwrite a library already
mapped by the daemon.

### Lane 1 — edit files: tuned TRAMP (`config/doom/config-tramp.el`)

Settings follow Core Dumped's *Making TRAMP go Brrrr* (2025-06):

- `tramp-default-method "ssh"` and `tramp-use-connection-share nil` — ride the
  ssh ControlMaster from `~/.ssh/config` rather than TRAMP's own handling.
- `remote-file-name-inhibit-locks t`, `tramp-use-scp-direct-remote-copying t`,
  `remote-file-name-inhibit-auto-save-visited t`, `tramp-copy-size-limit 1MB`,
  `tramp-verbose 1`.
- **Direct async processes** via a connection-local profile (the old
  `tramp-direct-async-process` property is deprecated) — the big win for
  compile/Magit/projectile over TRAMP.
- `vc-handled-backends '(Git)` to cut vc probing, the main per-file stall.
- **Connection-local nulls for remote** (added 2026-06-07, see Revision):
  `vc-handled-backends` → `nil` (keeps Git locally, off remotely); `diff-hl`
  disabled in remote buffers; `projectile-project-root` short-circuited to
  `nil` on remote paths via around-advice.

### Lane 2 — run things: Ghostel → persistent tmux (`config/doom/config-remote.el`)

- `my/remote-tmux` picks a tailnet host, then a tmux session, and opens Ghostel
  with `ssh -t HOST 'tmux new-session -A -s SESSION'`. `ghostel-exec` passes the
  local argv without a shell and the session name is quoted for OpenSSH's remote
  shell. `tmux new -A` is the idempotent attach-or-create pattern: the session is
  created once and reattached forever.
- **tmux is the durable layer** (lives on the host, survives disconnect/sleep,
  owns scrollback); the **Ghostel buffer is disposable transport** — kill it, lose
  nothing, reopen to reattach.

### Host source of truth — the tailnet

`my/tailscale-hosts` reads `tailscale status --json`, so any device that joins
the tailnet appears in the picker with **zero repo changes**. The single static
artifact is a one-time `Host *.ts.net` ControlMaster block appended to
`~/.ssh/config` by `install.sh` (idempotent), which speeds both TRAMP and the
Ghostel ssh for every present and future tailnet host.

### tmux config provisioning

`config/doom/remote/tmux.conf` (mouse, 50k history, vi copy-mode) is copied onto a host
via `my/remote-provision-tmux` (TRAMP `copy-file`). The first time a host is used
for a remote terminal we offer to install it and persist the answer
(`remote-seen-hosts.el` in the Doom cache) so the prompt appears once.

## Consequences

- `SPC o x t` → persistent remote terminal; `SPC o x f` → remote dired;
  `SPC o x p` → (re)install tmux.conf. All keyed off a live tailnet picker.
  (`SPC o x`; `o r`/`o R` are Doom's REPL bindings.)
- Editing remote files is snappy after the first connect; long-running remote
  work survives any client disconnect.
- The ssh block is fragile only to the tailnet domain changing; re-running
  `install.sh` is the refresh path.
- `tramp-use-connection-share` is the Emacs-30 name (emacs-plus@30, ADR-009);
  on older Emacs the equivalent is `tramp-use-ssh-controlmaster-options`.
- **dirvish is disabled globally** (`dirvish-override-dired-mode -1` in
  `config-remote.el`). Over TRAMP its previews spawn a remote process on every
  cursor move and its `vc-state` attribute runs git per file, so remote dired
  stalled ~3s/dir and stuttered on navigation. Plain dired is ~milliseconds
  remotely; the trade is losing dirvish's preview/columns locally too.

## Revision 2026-06-07 — remote dired still slow after dirvish off

Disabling dirvish (above) fixed the per-cursor-move stutter, but `dired-find-file`
into a remote directory still took seconds. Profiling (`M-x profiler-start cpu`
on a remote nav) blamed three find-file-hook hangers-on, all stat-bound:

1. **Projectile root detection — 80% of the sample.**
   `projectile-track-known-projects-find-file-hook` → `projectile-project-root`
   walks the tree **bottom-up + top-down + top-down-recurring** for project
   markers (`.git`, `.projectile`, …). Each probe is a `file-exists-p` /
   `file-symlink-p` / `file-attributes` = one ssh `stat` round-trip; dozens per
   nav, no cache hit because `default-directory` changed.
2. **VC backend probing.** `vc-handled-backends '(Git)` is correct locally but
   over TRAMP still shells out to `git` per file/dir.
3. **diff-hl** (`vc-gutter +pretty`) runs `git diff` per remote buffer.

Fix in `config-tramp.el`:

- `(connection-local-set-profile-variables 'remote-without-vc
   '((vc-handled-backends . nil)))` + apply to `(:application tramp)`.
- `find-file-hook` lambda turning `diff-hl-mode` off for `file-remote-p` buffers.
- Around-advice on `projectile-project-root` returning `nil` when
  `default-directory` is remote — covers the find-file-hook *and* mode-line
  redisplay paths, not just the one hot hook.

Trade: projectile's project commands (`SPC p …`, `projectile-compile/test/switch`)
no-op on remote files; plain `M-x compile` + TRAMP dired are unaffected. If
remote project commands ever matter, swap the advice for
`(setq projectile-track-known-projects-automatically nil)` — kills the per-nav
walk only and keeps on-demand commands working.

## Alternatives considered

- **Emacs-on-remote inside tmux** (`emacs -nw` on the box) — fully persistent and
  fastest file access, but a second Emacs/config and terminal-only UX. Rejected
  for a daily driver; the mac's GUI Emacs stays the single front-end.
- **TRAMP-only + `detached.el`** — good for fire-and-forget builds, but weak TRAMP
  support and not a replacement for interactive persistent shells.
- **Eat instead of Ghostel** — pure Elisp and extremely portable, but Doom has no
  official Eat module. Ghostel is Doom's current terminal direction and brings
  libghostty-vt, synchronized output, modern keyboard/graphics protocols, and
  prebuilt native modules.
- **mosh** — only helps with roaming/lid-close on flaky links; tmux + ControlMaster
  already give persistence + fast reconnect on this stable path. It is a one-line
  swap in `my/remote-tmux` (`ssh -t HOST` → `mosh HOST --`) if needed; note mosh
  uses UDP 60000–61000 and supplies no scrollback (tmux does).
- **A global TRAMP "connected" hook to offer provisioning** — too noisy (fires on
  every remote file open); the offer is gated to `my/remote-tmux`, where tmux
  actually matters.

## References

- [Making TRAMP go Brrrr — Core Dumped, 2025-06](https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./)
- [Speeding up Emacs TRAMP via ControlMaster — xenodium](https://xenodium.com/speeding-up-emacs-tramp-via-controlmaster)
- ADR-009 — macOS emacs-plus@30 (Emacs 30, which sets the TRAMP variable names).
- ADR-001 — broader Emacs language/tooling choices this builds on.
