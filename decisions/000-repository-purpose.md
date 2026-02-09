# ADR-000: Repository Purpose

**Status:** Accepted
**Date:** 2026-01-31

## Context

This document establishes the fundamental purpose and design principles for this dotfiles repository.

## Decision

### Purpose

Automatically set up a complete Doom Emacs development environment with all prerequisites on Windows.

### Goals

1. **Reliable installation** - Fresh Windows machine to working Doom Emacs in minimal steps
2. **Idempotent scripts** - Safe to re-run installation scripts without side effects
3. **Windows 10/11 focused** - Native Windows tooling (winget, PowerShell, cmdproxy)
4. **Evil mode (Vim keybindings)** - Full Vim emulation via evil-mode

### Design Principles

- **Automation over documentation** - If it can be scripted, script it
- **Prefer official/stable packages** - GNU-maintained, core Emacs features, LLVM tools
- **Minimal manual steps** - User runs install script, everything else is automated
- **Decision documentation** - Record significant choices in ADRs for future reference

## Consequences

- Installation scripts must handle missing prerequisites gracefully
- All tool dependencies documented and installed via scripts
- Configuration tested on Windows 10/11 (other platforms not supported)
