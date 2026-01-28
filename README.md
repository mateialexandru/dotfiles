# Dotfiles

Personal configuration files for Windows development environment.

## Contents

- `doom/` - Doom Emacs configuration
- `scripts/setup-doom.ps1` - Installs Emacs, Doom, and dependencies
- `install.ps1` - Creates symlinks and runs setup

## Installation

```powershell
# Clone the repo
git clone <your-repo-url> C:\avd\dotfiles

# Run install (creates symlinks + installs dependencies)
cd C:\avd\dotfiles
.\install.ps1
```

## What install.ps1 does

1. Creates symlink: `~\.config\doom` → `C:\avd\dotfiles\doom`
2. Runs `setup-doom.ps1` which:
   - Sets HOME environment variable
   - Installs Emacs, ripgrep, fd, LLVM, Pandoc, ShellCheck, JetBrains Mono NF
   - Clones and installs Doom Emacs
   - Configures Git bash, fonts, and Emacs server
   - Adds Windows Explorer context menu entries

## Requirements

- Windows 10/11
- PowerShell 5.1+
- Git
- Developer Mode enabled (for symlinks) or run as Administrator
