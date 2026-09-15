# ADR-009: macOS Emacs via emacs-plus@30 with daemon + client workflow

**Status:** Accepted
**Date:** 2026-05-15

## Context

The previous macOS install path used `brew install --cask emacs` — the official "Emacs for OSX" build from emacsformacosx.com. That build:

- Has no native compilation (no `--with-native-comp`).
- Is plain GNU Emacs with stock macOS integration.
- Cold-start is noticeably slow under Doom (magit, eglot/Roslyn LSP, org-roam, treemacs, vertico, corfu all load on demand).

For a daily-driver Doom setup, this was the limiting factor on every `Cmd+Tab` to Emacs.

## Decision

Switch macOS Emacs to **`emacs-plus@30`** from the `d12frosted/emacs-plus` tap, configured for a daemon + client workflow.

### Concrete choices

| Aspect | Choice | Rationale |
|---|---|---|
| Tap & formula | `d12frosted/emacs-plus` → `emacs-plus@30` | Native compilation enabled by default; tracks GNU upstream closely; actively maintained; Doom's own recommended Mac build. |
| Version | `@30` (Emacs 30.2, GNU stable) | `@31` exists in the tap but tracks unreleased dev; not safe as a daily driver. |
| Build flags | `--with-imagemagick` only | Other `--with-*` options are either default (native-comp) or unnecessary. `--with-native-comp` is no longer an explicit flag — it's the default. |
| Icon | `retro-gnu-meditate-levitate` (Nevrax Design Team) | Configured via `~/.config/emacs-plus/build.yml`; the deprecated `--with-*-icon` brew options no longer work. Wallpaper to match: <https://www.gnu.org/graphics/meditate-fs.jpg>. |
| Runtime model | Daemon + Emacs Client.app | Cold Emacs startup takes seconds; the daemon serves frames in ~50ms via `emacsclient -c -n`. The user's config already calls `(server-start)` and shell aliases use `emacsclient`. |
| Daemon lifecycle | `brew services start d12frosted/emacs-plus/emacs-plus@30` | Launches `emacs --fg-daemon` via launchd at user login. Idempotent. |
| App in `/Applications` | Only `Emacs Client.app`, **copied** (not symlinked) | macOS LaunchServices indexes copied .apps for Spotlight & Cmd+Tab. Symlinked .apps are often skipped. `Emacs.app` is not installed because the daemon is the only fresh-Emacs entry point we need. |
| App registration | `lsregister -f` + `mdimport` | Forces LaunchServices and Spotlight to pick up the newly-placed Client.app immediately, without waiting for the next reindex. |

### Native compilation linker fix (Apple Silicon specific)

`emacs-plus@30` ships libgccjit but does **not** bake the full target-tuple library path into its linker invocation. On Apple Silicon, Homebrew's gcc places `libemutls_w.a` (emulated TLS, required for native-compiled code) at:

```
/opt/homebrew/Cellar/gcc/<ver>/lib/gcc/current/gcc/aarch64-apple-darwin25/<ver>/libemutls_w.a
```

…which is **not** on libgccjit's default linker search path. The result: the first time Doom triggers a JIT trampoline compile (during `doom-after-modules-init-hook` → `general-auto-unbind-keys`), `ld` fails with `library 'emutls_w' not found`, and Doom **aborts boot mid-module-load**. The symptom is "config doesn't apply" — only 3 built-in modules end up in `doom-modules`, theme isn't set, keybindings dead.

**Fix:** `config/shell/init.zsh` exports `LIBRARY_PATH` to the directory holding `libemutls_w.a`, globbed off the version-stable `/opt/homebrew/lib/gcc/current/gcc/*/*/` symlink (so a `brew upgrade gcc` needs no re-install). `config/doom/config-macos.el` adds `LIBRARY_PATH` to `exec-path-from-shell-variables` and pulls the login shell's env in at boot.

### Revision 2026-07-31 — the env-file route is dead; exec-path-from-shell now covers the daemon

The original fix baked `LIBRARY_PATH` into `~/.config/emacs/.local/env` via `doom env`. **Current Doom no longer reads that file** — nothing in `lisp/` or `early-init.el` calls `doom-load-envvars-file` (it survives only as an obsolete alias in `modules/doom/compat/`). So the bake was inert, and the daemon ran with `LIBRARY_PATH` unset.

Worse, the same gap hid the whole shell `PATH`. launchd starts the daemon with a bare `/usr/bin:/bin:/usr/sbin:/sbin`, and `config-macos.el` gated `exec-path-from-shell-initialize` on `(memq window-system '(mac ns))` — nil while a daemon boots, so it never ran. Emacs saw no Homebrew binaries at all; the visible symptom was vterm's:

```
vterm-module--cmake-is-available: Vterm needs CMake to be compiled.  Please, install CMake
```

…with cmake installed and on the shell's PATH the whole time.

The gate is now `(or (daemonp) (memq window-system '(mac ns)))`, the `doom env` call is gone from the installer, and `sys check` check 2 asserts the daemon actually resolves `cmake` and has `LIBRARY_PATH` — a symptom-level gate rather than a check on the (now unread) env file.

### Daemon health & "config doesn't apply" debugging

Two distinct failure modes look identical from the user side ("Doom doesn't load"):

1. **Broken `~/.config/doom` symlink** — Doom silently falls back to its default doomdir (no user modules). `scripts/install-doom.sh` now detects and repairs wrong symlink targets.
2. **`libemutls_w` linker failure** — as above. Daemon stderr at `/tmp/homebrew.mxcl.emacs-plus.stderr.log` is the diagnostic.

Useful probes:

```sh
emacsclient -e 'doom-user-dir'                       # should be ~/.config/doom or its truename
emacsclient -e '(hash-table-count doom-modules)'     # healthy ≈ 50+, broken ≈ 3
emacsclient -e 'doom-theme'                          # should be 'doom-one
```

`sys check` (ADR-012) automates these environmental probes — daemon reachability,
native-comp queue, the daemon's inherited env, the `~/.config/doom` symlink, and Client.app.

### `brew services` label vs. actual reachability

`brew services list` is **not** authoritative for daemon health. The launchd job records
its *last exit code*, so after any non-zero exit (a crash the KeepAlive already restarted,
a stop/start race) the label reads `error`/`stopped` while a live daemon still answers
`emacsclient`. Observed: `emacs-plus@30 error 1` in the list, yet `emacsclient --eval t`
returns `t`. **Ping is the source of truth**; treat the service label as informational, and
`sys restart` to clear a stale one. (This is why `sys check` gates the daemon on the
ping, not the label — ADR-012.)

## Consequences

- Frame creation feels near-instant once the daemon is warm (~50ms).
- First boot of the daemon after install kicks off a background native-comp queue for all installed packages (5–20 min); UI may feel sluggish until it drains. Check `(length comp-files-queue)` to monitor.
- `Emacs.app` is no longer in `/Applications`. To start a clean Emacs without the daemon, use `/opt/homebrew/opt/emacs-plus@30/bin/emacs` from a shell.
- The daemon's environment is only as good as the login shell's. Anything Emacs needs on `PATH` (or in `LIBRARY_PATH`) must be exported from `config/shell/init.zsh`; adding it to `~/.config/emacs/.local/env` does nothing.
- emacs-plus and emacs-mac (Mitsuharu's port, railwaycat/emacsmacport) are mutually exclusive — installing emacs-mac would conflict and require uninstalling emacs-plus first. We chose emacs-plus for upstream-tracking and native-comp parity over emacs-mac's slightly smoother GUI rendering.

## Alternatives considered

- **`emacsformacosx` cask** — what we replaced. No native-comp. Slow.
- **`emacs-mac` (railwaycat)** — Mitsuharu's port. Smoother macOS feel (pixel scroll, gestures), but lags GNU upstream and native-comp arrived late. Acceptable alternative if rendering quality outweighs upstream proximity.
- **`emacs-plus@31`** — pre-release dev branch; not for daily use until GNU tags 31.0.

## References

- [d12frosted/homebrew-emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
- [Icon gallery](https://github.com/d12frosted/homebrew-emacs-plus/blob/master/community/icons/README.md)
- ADR-001 — broader LSP / language-tooling choices that this works with.
