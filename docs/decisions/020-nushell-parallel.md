# ADR-020: Nushell as a parallel interactive shell

**Status:** Accepted
**Date:** 2026-09-15

## Context

Nushell offers one structured, typed interactive language across macOS, Linux, and
Windows. Most daily commands in this setup (`git`, `hack`, `sys`, `gh`, `uv`,
`dotnet`, and `emacsclient`) are external executables and do not depend on POSIX shell
syntax.

Making Nu the login shell immediately would carry unrelated compatibility risk. Existing
bootstrap scripts are Bash or Windows PowerShell, Emacs packages may generate POSIX shell
commands, and remote workflows such as TRAMP commonly assume a POSIX login shell.

## Decision

Install Nushell and provide a small shared interactive configuration, but do not change
the login shell, terminal defaults, `.zshrc`, or any Bash/PowerShell scripts.

The public configuration is linked as `10-dotfiles.nu` in Nu's user autoload directory.
This preserves a user's existing `config.nu` and follows Nu's native modular configuration
mechanism. The installer also generates the officially supported zoxide and fzf Nu
integrations when those tools are present.

Private layers may add `shell/init.nu`. Because Nu resolves `source` paths at parse time,
the installer links these files directly into the same autoload directory as
`50-layer-NAME.nu`, preserving lexical layer order without runtime evaluation.

## Consequences

- Run `nu` from an existing zsh, Bash, or PowerShell session to experiment safely.
- The same aliases, editor variables, and user tool paths are available in Nu.
- `sys`, installers, Emacs, SSH, and TRAMP continue using their existing interpreters.
- Activating a new private layer requires rerunning `scripts/install-nushell.sh` (or
  `.ps1`) so its optional `shell/init.nu` link is registered.
- Changing the login or default shell remains a separate future decision based on actual
  experience rather than being part of installation.
