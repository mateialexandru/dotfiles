# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Cross-platform dotfiles repository managing Doom Emacs configuration for Windows and Linux (Bluefin/Fedora). Uses symlinks to link `doom/` to `~/.config/doom`.

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

## Doom Configuration Structure

- `doom/init.el` — Module declarations (what Doom features to enable). Run `doom sync` after changes.
- `doom/packages.el` — Additional package declarations. Run `doom sync` after changes.
- `doom/config.el` — Shared user configuration (theme, keybindings, EWW, devcontainer, org, ctags). Changes take effect on Emacs restart without `doom sync`.
- `doom/config-linux.el` — Linux-specific settings (dired switches).
- `doom/config-windows.el` — Windows-specific settings (Git Bash shell, dired ls-lisp, Roslyn LSP).

## Scripts

- `scripts/setup-doom.ps1` — Windows dependency installer (winget)
- `scripts/doctor.ps1` — Diagnose dotfiles/Doom issues on Windows
- `scripts/Test-DotNetLsp.ps1` — Test Roslyn LSP connectivity
- `scripts/install-roslyn-lsp.ps1` — Install Roslyn language server DLL

## Key Design Decisions

- **Symlink-based**: Install scripts symlink `doom/` to `~/.config/doom` so Emacs reads config from the repo
- **Cross-platform**: Shared config with platform-specific overrides loaded via `(load! "config-<platform>")`
- **Evil mode**: Vim keybindings via evil-mode
- **Eglot over LSP-mode**: Uses eglot for language server support
- **Roslyn LSP (--stdio)**: C# language server on Windows via `dotnet` + Roslyn DLL with `--stdio` flag; sends `solution/open` notification for .sln discovery
- **Corfu over Company**: Uses corfu for completion
- **Universal Ctags + xref**: Fallback navigation via etags when LSP isn't available
- **EWW browser**: Built-in web browser with persp-mode session persistence
- **Org-roam**: Knowledge management with org-roam2
- **Devcontainer support**: Docker-based dev containers with path rewriting for compilation output

## Decision Records

Major decisions (package choices, architectural patterns, tool selections) are documented in `decisions/` as Architecture Decision Records (ADRs).

**Guidelines:**
- Document the "why" behind significant choices, not just the "what"
- Include alternatives considered and rationale for rejection
- Revisit decisions periodically as the ecosystem evolves
- Format: `decisions/NNN-short-description.md`

**Current decisions:**
- `001-emacs-packages.md` - LSP client, language servers, ctags, eshell packages

## Documentation Practices

This documentation follows an **append-only** approach where it makes sense:
- New features and configurations are added to existing sections
- Historical context is preserved rather than overwritten
- Changes are additive to maintain a living record of the configuration's evolution
- Only remove content when it's genuinely obsolete or misleading
