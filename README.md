# Dotfiles

Cross-platform Doom Emacs configuration for Windows and Linux (Bluefin/Fedora).

## Contents

- `doom/` — Doom Emacs configuration (shared + platform-specific)
  - `config.el` — Shared config (theme, org, EWW, devcontainer)
  - `config-linux.el` — Linux-specific settings
  - `config-windows.el` — Windows-specific settings
  - `init.el` — Module declarations
  - `packages.el` — Extra package declarations
- `nvim/` — Neovim (LazyVim) configuration
- `decisions/` — Architecture Decision Records (design rationale)
- `install.sh` — Linux installer (symlinks + Doom install)
- `install.ps1` — Windows installer (symlinks + full setup)
- `scripts/setup-doom.ps1` — Windows dependency setup

## Installation

### Linux (Bluefin/Fedora)

Prerequisites: Emacs, Git, ripgrep, fd (install via brew or your package manager).

```bash
git clone <your-repo-url> ~/Source/dotfiles
cd ~/Source/dotfiles
./install.sh
```

This will:
1. Symlink `doom/` to `~/.config/doom` (backs up existing config if present)
2. Install Doom Emacs if not already installed
3. Run `doom sync`

### Windows

Prerequisites: Git, Developer Mode enabled (for symlinks) or run as Administrator.

```powershell
git clone <your-repo-url> C:\avd\dotfiles
cd C:\avd\dotfiles
.\install.ps1
```

This will:
1. Create symlink: `~\.config\doom` → `dotfiles\doom`
2. Run `setup-doom.ps1` which:
   - Sets HOME environment variable
   - Installs Emacs, ripgrep, fd, LLVM, Pandoc, ShellCheck, JetBrains Mono NF
   - Clones and installs Doom Emacs
   - Configures Git bash, fonts, and Emacs server
   - Adds Windows Explorer context menu entries

## Requirements

### Linux
- Emacs 29+
- Git
- ripgrep, fd (for Doom search/file-finding)

### Windows
- Windows 10/11
- PowerShell 5.1+
- Git
- Developer Mode enabled (for symlinks) or run as Administrator
