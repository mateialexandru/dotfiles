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
- `doom/config.el` — Shared user configuration (theme, keybindings, EWW, devcontainer, org). Changes take effect on Emacs restart without `doom sync`.
- `doom/config-linux.el` — Linux-specific settings (dired switches).
- `doom/config-windows.el` — Windows-specific settings (Git Bash shell, dired ls-lisp).

## Key Design Decisions

- **Symlink-based**: Install scripts symlink `doom/` to `~/.config/doom` so Emacs reads config from the repo
- **Cross-platform**: Shared config with platform-specific overrides loaded via `(load! "config-<platform>")`
- **Evil mode**: Vim keybindings via evil-mode
- **Eglot over LSP-mode**: Uses eglot for language server support
- **Corfu over Company**: Uses corfu for completion
- **EWW browser**: Built-in web browser with persp-mode session persistence
- **Org-roam**: Knowledge management with org-roam2
- **Devcontainer support**: Docker-based dev containers with path rewriting for compilation output
