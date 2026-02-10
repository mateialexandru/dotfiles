# ADR-000: Repository Purpose

**Status:** Accepted (updated 2026-02-10)
**Date:** 2026-01-31

## Context

This document establishes the fundamental purpose and design principles for this dotfiles repository.

## Decision

### Purpose

Automatically set up a complete development environment with Doom Emacs and Neovim (LazyVim) on Windows and Linux (Bluefin/Fedora) via symlink-based configuration management.

### Goals

1. **Reliable installation** - Fresh machine to working editors in minimal steps
2. **Idempotent scripts** - Safe to re-run installation scripts without side effects
3. **Cross-platform** - Windows 10/11 (winget, PowerShell) and Linux (Homebrew, shell scripts)
4. **Evil mode (Vim keybindings)** - Full Vim emulation in both editors
5. **Symlink-based config** - Editor configs live in the repo and are symlinked to platform config directories

### Editors

- **Doom Emacs** - Primary editor with full configuration (LSP, org-roam, devcontainers, EWW, ctags)
- **Neovim (LazyVim)** - Secondary editor for lighter-weight use (quick edits, remote SSH, systems without Emacs)

### Design Principles

- **Automation over documentation** - If it can be scripted, script it
- **Prefer official/stable packages** - GNU-maintained, core Emacs features, LLVM tools
- **Minimal manual steps** - User runs install script, everything else is automated
- **Decision documentation** - Record significant choices in ADRs for future reference
- **Shared config with platform overrides** - Common configuration with OS-specific files loaded conditionally

## Consequences

- Installation scripts must handle missing prerequisites gracefully
- All tool dependencies documented and installed via scripts
- Configuration tested on Windows 10/11 and Linux (Bluefin/Fedora)
- Both `doom/` and `nvim/` directories are symlinked by install scripts
