# AGENTS.md

This file provides guidance to coding agents working in this repository.

## Overview

Cross-platform dotfiles repository managing Doom Emacs configuration for macOS, Windows, and Linux (Bluefin/Fedora). The core pattern is symlink-based: install scripts link `config/doom/` to `~/.config/doom`, so edits here are live immediately in Emacs.

## Installation

### macOS / Linux

```bash
./install.sh                                                                # local checkout
bash -c "$(curl -fsSL https://raw.githubusercontent.com/mateialexandru/dotfiles/main/install.sh)"  # fresh machine bootstrap (clones to ~/Source/dotfiles, then re-execs)
```

Installs Homebrew, core tools (`fzf`, `zoxide`, `gh`, `ripgrep`, `fd`, `node`, `dotnet`, `llvm`, etc.), fonts, Doom Emacs, the Roslyn LSP (via `scripts/install-roslyn-lsp.sh`), and syncs the Doom config. Idempotent — safe to re-run.

On macOS, Emacs is installed by `scripts/install-emacs-mac.sh` (via `d12frosted/emacs-plus` → `emacs-plus@30`, native-comp, `retro-gnu-meditate-levitate` icon). The setup uses a **daemon + client workflow**: `emacs --fg-daemon` runs via `brew services` at login, and only `Emacs Client.app` is copied (not symlinked) into `/Applications` so Spotlight indexes it. New frames open in ~50ms via `emacsclient -c -n`.

The daemon is started by launchd, so it inherits a bare `/usr/bin:/bin:/usr/sbin:/sbin` and none of the shell's env. `config/doom/config-macos.el` fixes that with `exec-path-from-shell` gated on `(or (daemonp) (memq window-system '(mac ns)))` — the `daemonp` half matters, since `window-system` is nil while the daemon boots. It also pulls in `LIBRARY_PATH` (exported by `config/shell/init.zsh`) so libgccjit's linker finds `libemutls_w.a` on Apple Silicon. Without this, Emacs sees no Homebrew binaries (vterm can't find cmake) and native-comp can abort boot mid-module-load. Doom's env file (`~/.config/emacs/.local/env`) is **not** read by current Doom — don't put env there. See ADR-009; daemon stderr is at `/tmp/homebrew.mxcl.emacs-plus.stderr.log`.

### Windows

```powershell
.\install.ps1
```

Orchestrates: profile isolation, prerequisites (via `scripts/install-prerequisites.ps1`), Doom Emacs, and hack worktree tooling.

### Verifying

There is no test suite — the "tests" are: (a) install scripts must remain idempotent, (b) on Windows, `scripts/doctor.ps1` validates the environment (Emacs, ripgrep, fd, fonts, symlinks, Roslyn LSP). When changing installers, re-run on a clean machine or worktree to confirm.

## After changing Doom config

- `config/doom/config.el` changes take effect immediately (no sync needed).
- `config/doom/init.el` or `config/doom/packages.el` changes require `doom sync` and an Emacs restart.

```bash
~/.config/emacs/bin/doom sync
```

## Architecture

### Config split

| File | Purpose |
|------|---------|
| `config/doom/config.el` | Shared configuration loaded on all platforms |
| `config/doom/config-profile.el` | Portable defaults + ordered optional private-layer loader |
| `config/doom/config-eshell.el` | Shared eshell inline-image tooling (`cat`/`rinku`, TRAMP-aware; adapted from xenodium) |
| `config/doom/config-tramp.el` | TRAMP performance tuning for remote editing (ControlMaster reuse, direct-async, skip vc) — see ADR-011 |
| `config/doom/config-remote.el` | Tailnet host picker + persistent vterm→tmux terminals + tmux.conf provisioning (`SPC o x`) — see ADR-011 |
| `config/doom/config-gptel.el` | LLM client on top of `:tools llm` — ChatGPT-OAuth + Ollama backends, gptel-agent, project chats (`SPC o l`) — see ADR-014 |
| `config/doom/config-python.el` | uv-owned venvs, ruff format/lint, pyright against the project `.venv`, projectile `python-uv` type, `SPC m u` bootstrap — see ADR-015 |
| `config/doom/config-macos.el` | macOS-specific (Command=Meta, exec-path-from-shell, dired) |
| `config/doom/config-linux.el` | Linux-specific (dired ls flags, libnotify alerts) |
| `config/doom/config-windows.el` | Windows-specific (fonts, Git Bash shell, Magit performance) |
| `config/doom/init.el` | Module declarations — controls which Doom modules are loaded |
| `config/doom/packages.el` | Extra package declarations beyond Doom modules |

Platform detection uses `(pcase system-type ...)` at the bottom of `config.el`, which `load!`s the appropriate file.

### Key packages (beyond Doom modules)

- **`devcontainer`** — build/start Docker containers; compile inside them. C# test error pattern registered with `compilation-error-regexp-alist`.
- **`winpulse`** — flashes window background on focus.
- **`apheleia`** — format-on-save. C# uses `csharpier` via `dotnet csharpier --write-stdout`.

### org-roam contexts

`my/roam-register-context` / `my/roam-switch-context` support multiple roam databases. Toggle with `SPC t r`. The public default uses `~/Documents/org/roam/`; optional private layers can register additional contexts.

### Project TLDR (`SPC p ?`)

`my/project-tldr` opens `TLDR.org` (or `TLDR.md`) from the project root in a right-hand
popup, offering to scaffold one if the project has none. It's for what the code doesn't say
and a stale upstream README gets wrong: the command that actually builds, the port something
is served on, the gotcha that cost an afternoon. Lives in the repo it describes, so it
travels with worktrees. Mnemonic: *project → what do I do here again?*

### Local AI inference (Ollama managed + LM Studio user-managed)

Local LLMs on Apple Silicon — see ADR-013. **Ollama** (Homebrew formula) is the *managed*
runtime: GGUF, reproducible from `scripts/ollama-models.txt`, OpenAI endpoint
`localhost:11434`. **LM Studio** is kept as the GUI + MLX playground but is *user-managed*
— you download MLX models in the app yourself; the installer only ensures it's present.
The two can't share files (Ollama always copies into its own store; GGUF≠MLX), so
`keeper llm mirror` symlinks Ollama's GGUF models into LM Studio (labelled `ollama`,
self-healing/prune) as a convenience. Both installed by `scripts/install-llm-mac.sh`.

The Ollama daemon is **on-demand, not a login service** — control it via `keeper`:

```
keeper llm start    # brew services run ollama (no login registration) + warm a model resident
keeper llm stop     # brew services stop ollama (frees RAM)
keeper llm status   # endpoint up? → ollama list + ollama ps
keeper llm mirror   # (re)sync Ollama's GGUF models into LM Studio
```

Models live in `scripts/ollama-models.txt` (a `#`-commented manifest; `keeper install`
pulls it, idempotently). Default 64 GB set: `gpt-oss:20b` (all-rounder/reasoning),
`qwen3-coder:30b` (coding), `nomic-embed-text` (embeddings).

### LLM client in Emacs (gptel)

Doom's `:tools llm` module is **on** — it brings gptel plus `gptel-quick` (explain at point),
`gptel-magit` (generated commit messages, `M-g` in a commit buffer), `ob-gptel` (org-babel
`gptel` blocks), a popup rule, and the `SPC o l` map. `config/doom/config-gptel.el` layers on top
rather than re-declaring any of it. See ADR-014.

Auth is the **ChatGPT subscription over OAuth** (`gptel-make-openai-oauth`) — no OpenAI
platform API key, no second bill. First request opens a browser login; force it with
`M-x gptel-openai-oauth-login`. The token lands in `~/.config/emacs/.cache/gptel-openai/`.

A second backend points at ADR-013's Ollama endpoint. `gptel-agent` supplies the agentic
layer: ~32 tools (read/grep/glob free, bash/edit/write confirmed), sub-agents, skills from
the upstream-compatible `~/.claude/skills/` location (independent of the Claude Code CLI),
and TRAMP support. Sub-agent calls are routed to local Ollama so
delegated work costs no quota.

`gptel-quick` (`SPC o l e`, or `?` as an embark action on a candidate/identifier/region)
is routed to Ollama's `qwen3-coder:30b` for the same reason, plus a correctness one:
upstream doesn't work with reasoning models, and every Codex model is one. The endpoint
probe is cached for a minute so a lookup stays instant; when the daemon is down the
lookup falls back to whatever backend the session is on.

The popup is a ladder, not a dead end — `+` asks for a longer answer, `w` copies it (then
`p`), and `r` carries the query and the answer into a gptel chat buffer, where the
conversation continues on the session backend (ChatGPT) and `C-x C-w` keeps it as a file.
`r`/`w` are aliases for upstream's `M-RET`/`M-w`: a GUI frame sends `M-<return>`, which
org-mode already claims for `org-ctrl-c-ret`, so upstream's key is unreachable in exactly
the buffers a lookup most wants a follow-up in. `M-<return>` is aliased too, so both
spellings now work everywhere.

```
SPC o l l   # chat buffer          SPC o l p   # project chat → <repo>/.gptel/chat.org
SPC o l e   # explain at point     SPC o l A   # ephemeral agent session
SPC o l s   # send                 SPC o l F   # add project files to context
SPC o l r   # rewrite region       SPC o l c   # clear context
SPC o l m   # menu (model/preset)  SPC o l k   # compact conversation
SPC o l b   # toggle ChatGPT ↔ Ollama          C-c l   # same map, no leader
```

Two drop-in growth surfaces, live without a `doom sync` (they ride the `config/doom/` symlink):

- `config/doom/gptel/agents/` — one md/org file per sub-agent (`description` is the only required
  frontmatter key); `reviewer.md` is the worked example.
- `config/doom/gptel/tools.el` — machine-specific tools only (`keeper_health`, `ollama_models`).
  Add the name to `my/gptel-extra-tools` so the agent preset picks it up.

Project transcripts live in `<repo>/.gptel/chat.org` and are kept out of commits by
`config/git/ignore`, symlinked to `~/.config/git/ignore` by `install.sh`.

### Python (uv-first)

`uv` owns interpreters, dependencies, and each project's `.venv` — see ADR-015. Homebrew's
`python@3.14` is externally managed (PEP 668), so there is no global `pip` path and none is
wanted. `:lang python` runs `(python +lsp +pyright +uv +tree-sitter)`; the `+uv` flag
auto-activates the nearest `.venv` on buffer switch (modeline shows `UV:<version>`), so
nothing needs activating by hand.

Only two Python tools are global (`uv tool`, → `~/.local/bin`): **`ruff`** (format, import
sort, lint — replaces black + isort + pyflakes) and **`pyright`**. Everything else is a
project dependency run via `uv run`; `install.sh` actively uninstalls the retired tools so
stale shims can't win on PATH.

`config/doom/config-python.el` wires apheleia to `(ruff-isort ruff)`, disables the flake8/pylint
flycheck checkers, sets `lsp-pyright-venv-directory` to `.venv` (without this pyright
type-checks against its own isolated tool env and flags every third-party import), points
`python-pytest-executable` at `uv run pytest`, registers a `python-uv` projectile type on
`uv.lock` (otherwise a uv project matches `python-toml` and `SPC p t` runs
`python -m unittest discover`), and defaults `compile-command` to `uv run <file>`.

Deliberately **not** written, because they already work: venv activation (`uv-mode-set`
under `+uv` does what the widely-copied `uv-activate` snippet does, automatically),
ruff quickfixes (`lsp-ruff` is an `:add-on?` client in lsp-mode's defaults — it attaches
next to pyright with no config, giving `SPC c a` "Remove unused import" / "Organize
imports"), and auto-import completions (pyright, on by default). `uv add`/`uv sync` stay
shell commands.

Only bootstrapping is scripted, since uv can't infer which distribution an `import` means:

```
SPC m u n   new project (uv init + uv sync)
SPC m u p   move this file into an isolated project, with its dependencies
SPC m u s   give this file a PEP 723 header instead — no project, `uv run` anywhere
SPC m u v   re-activate the venv by hand
```

Detection is pipreqs via `uvx`, scanning a copy of the single file (so neighbouring scripts
don't leak in). Modules sitting beside the file are subtracted from the result — otherwise
a local `helpers.py` gets looked up on PyPI and some stranger's `helpers` package becomes a
dependency. Imports it genuinely can't map are named in the prompt rather than dropped
(`cv2` wants `opencv-python`), and the seeded list is editable before anything installs.

```bash
uv init myproj && cd myproj && uv add requests && uv sync   # then just open a file
```

Astral's `ty` is the eventual replacement for pyright (it is what Doom's module README now
recommends) but is still preview-grade.

### Remote workflow (Emacs → tailnet)

Two-lane hybrid for working on remote devices from the mac's GUI Emacs — see ADR-011.

- **Edit lane** — `config-tramp.el` tunes TRAMP (ssh ControlMaster reuse, direct-async, no vc probing) for snappy remote file editing.
- **Run lane** — `config-remote.el` opens a vterm bound to `ssh -t HOST 'tmux new -A -s SESSION'`; tmux on the host is the persistent layer (survives disconnect/sleep), the vterm buffer is disposable.

Hosts come from `tailscale status --json` (no host list to maintain). `install.sh` appends a one-time `Host *.ts.net` ControlMaster block to `~/.ssh/config`. `config/doom/remote/tmux.conf` is provisioned onto a host via TRAMP and offered on first connect.

```
SPC o x t   # pick host + tmux session → persistent vterm terminal
SPC o x f   # pick host → remote dired (TRAMP)
SPC o x p   # (re)install tmux.conf on a host
```

### Worktree tooling (`hack`)

`tools/hack` is a small, dependency-free Rust tool that lazily indexes Git repositories beneath `~/Source` (plus roots in `HACK_SOURCE_ROOTS`) and creates their task worktrees beneath `~/worktree`. Normal lookup reads `~/.cache/hack/repos`; an unknown alias triggers one filesystem-only refresh, avoiding repeated walks and Git subprocesses on Windows. Spawning always fetches first and creates a new task branch directly from `origin/develop` when it exists, otherwise from `origin/HEAD`, so a stale local base branch cannot leak into a new task. A repository may override inference with local `git config hack.baseBranch BRANCH`. If the task branch already exists on the remote, it is resumed instead. The tool does not launch agents or editors.

```text
hack <repo>/<branch-slug>           # Fetch + create/resume worktree
hack repos [--refresh]              # Show or explicitly refresh the index
hack list                           # Show active worktrees
hack remove <repo>/<branch-slug>    # Remove only when clean and merged
```

There is no manually maintained repository catalog. Clone a repository beneath a discovery root and its first lookup indexes it automatically. Local Git config supplies the uncommon per-repository overrides (`hack.baseBranch` and `hack.branchPrefix`). Install with `scripts/install-hack.sh` or `scripts/install-hack.ps1`.

### Shell (`config/shell/init.zsh`)

Source this from `.zshrc` for shared aliases (`e`, `et`, `g`, `gs`, `gd`, `gl`), `$EDITOR=emacsclient -c`, zoxide and fzf init.

### Scripts

| Script | Purpose |
|--------|---------|
| `scripts/install-doom.sh` / `.ps1` | Symlink doom dir + install Doom Emacs; detects & repairs wrong symlink targets |
| `scripts/install-emacs-mac.sh` | macOS-only: emacs-plus@30 + Emacs Client.app; `install.sh` starts the daemon after Doom sync (daemon env/libgccjit fix lives in `config/shell/init.zsh` + `config/doom/config-macos.el`) |
| `scripts/install-llm-mac.sh` | macOS-only: local LLM layer — Ollama formula (managed GGUF endpoint, pulls `scripts/ollama-models.txt`) + LM Studio cask (user-managed MLX GUI) + `keeper llm mirror` (symlink Ollama models into LM Studio). Daemon on-demand via `keeper llm start`, not a login service (see ADR-013) |
| `scripts/install-scrim-captee-mac.sh` | macOS-only, standalone (not in install.sh): opens the App Store "Scrim + Captee for Emacs" bundle + prints org-capture setup (see ADR-010) |
| `scripts/install-excalidraw-mac.sh` | macOS-only: excalidraw prereqs (fswatch + `@swiftlysingh/excalidraw-cli` faithful exporter + drawings dir) via `install_excalidraw_prereqs`; retires the old `excalidraw_export`/node-canvas/fonts; prints manual Chrome-PWA/handler steps (see ADR-008) |
| `scripts/keeper-health.sh` | macOS/Linux: full confidence pass — doom symlink, daemon env (PATH/LIBRARY_PATH inherited), daemon (ping-authoritative), native-comp queue, doom doctor, core tools, Roslyn DLL, LSP servers, apheleia formatters, Emacs Client.app, org-protocol→Scrim pin, excalidraw toolchain, Ollama (advisory when down), gptel/gptel-agent built + global gitignore in effect, ssh ControlMaster. Hard-exits 1 on any fail; latent/transient items are advisory. Run via `keeper health` |
| `scripts/install-hack.sh` / `.ps1` | Build and install the Rust `hack` binary with Cargo |
| `scripts/install-roslyn-lsp.sh` / `.ps1` | Download Microsoft Roslyn LSP NuGet package to `~/.local/share/roslyn-lsp` (or `%LOCALAPPDATA%\roslyn-lsp\` on Windows) |
| `scripts/install-plantuml.sh` | Download PlantUML's jar into Doom's profile data directory |
| `scripts/install-prerequisites.ps1` | Windows: ctags, node, dotnet, cmake, etc. via winget |
| `scripts/setup-profile.ps1` | Windows: move PowerShell profile out of OneDrive |
| `scripts/doctor.ps1` | Verify environment (Windows) |
| `scripts/Test-DotNetLsp.ps1` | Smoke-test the Roslyn LSP launches and serves requests |

## Decision records

Significant design choices are documented in `docs/decisions/` as ADRs. Check there before changing package choices or tooling.

| ADR | Topic |
|-----|-------|
| `000-repository-purpose.md` | Goals, design principles, scope |
| `001-emacs-packages.md` | LSP (lsp-mode), C# (Roslyn), C++ (clangd), code nav (xref+ctags), eshell — **read this before swapping language tooling** |
| `002-neovim-lazyvim.md` | Aspirational Neovim/LazyVim setup — **NOTE:** no `nvim/` directory exists in the repo today; treat as planning, not current state |
| `003-dotnet-devcontainer.md` | Devcontainer workflow for .NET projects |
| `004-csharp-lsp-omnisharp.md` | C# LSP history (now superseded by Roslyn in ADR-001) |
| `005-hack-worktree-tooling.md` / `006-hack-kiss-redesign.md` | Why `hack`/`workshop` exist and the v3 KISS rewrite |
| `007-archived-packages.md` | Retired package history; removed implementations remain recoverable from Git |
| `008-excalidraw-integration.md` | Excalidraw diagramming |
| `009-macos-emacs-plus.md` | macOS Emacs via emacs-plus@30 + daemon/client workflow + libgccjit `LIBRARY_PATH` workaround |
| `010-safari-org-capture.md` | Safari → org capture via org-protocol; now via the Scrim + Captee App Store bundle (DIY extension/handler/Xcode retired — see Revision 2026-05-22) |
| `011-emacs-remote-tmux.md` | Effortless Emacs → remote workflow: tuned TRAMP (edit lane) + vterm→persistent-tmux over Tailscale (run lane); tailnet as host source of truth |
| `012-keeper-ops-wrapper.md` | Global `keeper` ops wrapper (renamed from `atelier`) + `keeper health` full confidence pass; gate-on-symptom-not-remedy, ping-authoritative daemon check |
| `013-llm-ollama-lmstudio.md` | Local LLM: Ollama managed GGUF endpoint (`:11434`) + LM Studio user-managed MLX GUI; why stores can't be shared (Ollama copies in; GGUF≠MLX); `keeper llm` on-demand control + `mirror` symlink bridge; curated 64 GB set |
| `014-gptel-emacs-llm-client.md` | gptel on Doom's `:tools llm`: ChatGPT-subscription OAuth (not an API key), Ollama as second backend, gptel-agent for project sessions/tools/sub-agents, in-repo transcripts + global gitignore |
| `015-python-uv.md` | Python: `+uv` auto-activates the project `.venv`, ruff replaces black+isort+pyflakes, pyright pinned to `.venv`, `uv run pytest`, projectile `python-uv` type; only `pyright`+`ruff` stay global, only bootstrapping (`SPC m u`) is scripted |
| `017-compiled-hack-worktrees.md` | Compiled, lazily indexed worktrees; every task starts from freshly fetched `origin/<base>` |
| `018-drop-claude-code.md` | Claude Code removed after its subscription was discontinued; gptel remains the Emacs LLM surface |
| `019-repository-layout.md` | KISS layout: configuration, documentation, assets, scripts, and tools have distinct homes |

## Related files

- `README.md` — user-facing install instructions (mirror changes here when install flow changes).
