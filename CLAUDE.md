# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Cross-platform dotfiles repository managing Doom Emacs and Neovim (LazyVim) configuration for Windows and Linux (Bluefin/Fedora). Uses symlinks to link `doom/` to `~/.config/doom` and `nvim/` to `~/.config/nvim` (Linux) or `%LOCALAPPDATA%\nvim` (Windows).

## Setup

### Linux (Bluefin/Fedora)

```bash
./install.sh               # Creates symlinks + installs Doom if needed + syncs
```

### Windows

```powershell
.\install.ps1              # Creates symlinks + installs everything
```

This runs `scripts/setup-doom.ps1` which installs Emacs, Doom Emacs, and dependencies (ripgrep, fd, LLVM, Pandoc, ShellCheck, JetBrains Mono NF) via winget, then configures the system (Git Bash, fonts, Emacs server, Explorer context menus).

## Doom Emacs Commands

After installation, the `doom` CLI is available:

```sh
doom sync              # Sync config after changing init.el or packages.el
doom upgrade           # Update Doom and packages
doom doctor            # Diagnose issues
doom build             # Recompile packages
```

## Neovim (LazyVim) Configuration Structure

- `nvim/init.lua` — Bootstrap entry point (loads lazy.nvim)
- `nvim/lua/config/lazy.lua` — lazy.nvim setup with LazyVim spec and `rocks = { enabled = false }`
- `nvim/lua/config/options.lua` — Shared Neovim options
- `nvim/lua/config/keymaps.lua` — Custom keymaps (LazyVim defaults used)
- `nvim/lua/config/autocmds.lua` — Custom autocommands
- `nvim/lua/plugins/treesitter.lua` — Treesitter with `ensure_installed` languages
- `nvim/lua/plugins/csharp.lua` — C# via `seblyng/roslyn.nvim` (handles its own Roslyn download)

Adding plugins: create a new file in `nvim/lua/plugins/` returning a lazy.nvim plugin spec.

## Doom Configuration Structure

- `doom/init.el` — Module declarations (what Doom features to enable). Run `doom sync` after changes.
- `doom/packages.el` — Additional package declarations. Run `doom sync` after changes.
- `doom/config.el` — Shared user configuration (theme, keybindings, EWW, devcontainer, org, ctags, Roslyn LSP). Changes take effect on Emacs restart without `doom sync`.
- `doom/config-linux.el` — Linux-specific settings (dired switches).
- `doom/config-windows.el` — Windows-specific settings (Git Bash shell, dired ls-lisp, drive letter URI fix).

## Shell Tooling

- `shell/hack.ps1` — Worktree-based developer workflow (dot-sourced into PowerShell profile)
- `shell/hack-config.sample.json` — Example config for `~/.config/hack/config.json`

Two commands: `workshop` manages repos, `hack` handles worktree lifecycle.

### `workshop` — Repo Management

| Command | Purpose |
|---------|---------|
| `workshop` | List registered repos |
| `workshop add <alias> <url> [baseBranch]` | Register a repo (auto-creates config on first use) |
| `workshop remove <alias>` | Unregister a repo |
| `workshop default <alias>` | Set default repo |

### `hack` — Worktree Lifecycle

| Command | Alias | Purpose |
|---------|-------|---------|
| `hack [repo]` | — | Create scratch worktree + launch agent in plan mode |
| `hack name "description"` | — | Crystallize scratch → named branch + dir + SPEC.md |
| `hack done` | — | Push + create PR (picker if not in worktree) |
| `hack resume [filter]` | `r` | Picker + cd + launch agent (`claude --resume`) |
| `hack go [filter]` | `g` | Picker + cd only |
| `hack list` | `lh` | Status dashboard grouped by repo |
| `hack clean` | — | Interactive cleanup (merged + unnamed highlighted) |
| `hack branch` | — | Worktree from existing remote branch |
| `hack status` | — | Show current hack info + toggle agent mode |

### Explore-First Flow

```
workshop add infra https://...          # register repo
hack infra                              # scratch worktree → explore/ideate
hack name "add retry logic to auth"     # crystallize → named branch + SPEC.md
r                                       # resume (alias for hack resume)
hack done                               # push + PR
hack list                               # dashboard
hack clean                              # cleanup
```

### Prompt Integration

`Get-HackPrompt` returns `"repo/task"` or `""` — opt-in for shell prompt customization.

Config lives at `~/.config/hack/config.json` (not committed). Auto-created on first `workshop add`.

## Scripts

- `scripts/install-doom.ps1` — Install Doom Emacs on Windows (symlinks + deps + setup)
- `scripts/install-doom.sh` — Install Doom Emacs on Linux (symlinks + deps + setup)
- `scripts/install-nvim.ps1` — Install Neovim/LazyVim on Windows (symlink + deps)
- `scripts/install-nvim.sh` — Install Neovim/LazyVim on Linux (symlink + deps)
- `scripts/setup-doom.ps1` — Windows Doom dependency installer (winget, called by install-doom.ps1)
- `scripts/setup-lazyvim.ps1` — Windows Neovim dependency installer (winget, called by install-nvim.ps1)
- `scripts/setup-lazyvim.sh` — Linux Neovim dependency installer (Homebrew, called by install-nvim.sh)
- `scripts/doctor.ps1` — Diagnose dotfiles/Doom issues on Windows
- `scripts/Test-DotNetLsp.ps1` — Test Roslyn LSP connectivity
- `scripts/install-roslyn-lsp.ps1` — Install Roslyn language server DLL (Windows)
- `scripts/install-roslyn-lsp.sh` — Install Roslyn language server DLL (Linux)
- `scripts/install-hack.ps1` — Adds hack.ps1 dot-source to PowerShell profile (Windows)
- `scripts/install-hack.sh` — Adds hack.ps1 dot-source to pwsh profile (Linux)

## Key Design Decisions

- **Symlink-based**: Install scripts symlink `doom/` and `nvim/` to their platform config directories so editors read config from the repo
- **Cross-platform**: Shared config with platform-specific overrides loaded via `(load! "config-<platform>")`
- **Evil mode**: Vim keybindings via evil-mode
- **Eglot over LSP-mode**: Uses eglot for language server support
- **Roslyn LSP (--stdio)**: Cross-platform C# language server via `dotnet` + Roslyn DLL with `--stdio` flag; sends `solution/open` notification for .sln discovery. Windows DLL in `%LOCALAPPDATA%/roslyn-lsp/`, Linux in `~/.local/share/roslyn-lsp/`
- **Corfu over Company**: Uses corfu for completion
- **Universal Ctags + xref**: Fallback navigation via etags when LSP isn't available
- **EWW browser**: Built-in web browser with persp-mode session persistence
- **Org-roam**: Knowledge management with org-roam2
- **Devcontainer support**: Docker-based dev containers with path rewriting for compilation output
- **LazyVim**: Neovim distribution with lazy.nvim plugin management, used alongside Doom Emacs
- **roslyn.nvim**: C# language server for Neovim — handles Roslyn download automatically (unlike Eglot which needs install scripts)

## Decision Records

Major decisions (package choices, architectural patterns, tool selections) are documented in `decisions/` as Architecture Decision Records (ADRs).

**Guidelines:**
- Document the "why" behind significant choices, not just the "what"
- Include alternatives considered and rationale for rejection
- Revisit decisions periodically as the ecosystem evolves
- Format: `decisions/NNN-short-description.md`

**Current decisions:**
- `000-repository-purpose.md` - Cross-platform scope, design principles, editor choices
- `001-emacs-packages.md` - LSP client, language servers, ctags, eshell packages
- `002-neovim-lazyvim.md` - LazyVim choice, roslyn.nvim, treesitter compilation, config-in-repo
- `003-dotnet-devcontainer.md` - .NET devcontainer integration, TRAMP-based approach
- `004-csharp-lsp-omnisharp.md` - Switch from csharp-ls to OmniSharp for devcontainer workflows
- `005-hack-worktree-tooling.md` - Worktree tooling refactor: config-driven, same-terminal, cross-platform

## Documentation Practices

This documentation follows an **append-only** approach where it makes sense:
- New features and configurations are added to existing sections
- Historical context is preserved rather than overwritten
- Changes are additive to maintain a living record of the configuration's evolution
- Only remove content when it's genuinely obsolete or misleading
