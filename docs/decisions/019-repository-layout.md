# 019 — KISS repository layout

**Date:** 2026-09-14
**Status:** Accepted

## Context

The repository root mixed public entrypoints, application configuration, documentation,
examples, third-party artwork, and dead implementations. Several single-purpose directories
and documents made the root harder to scan without communicating a useful taxonomy.

## Decision

Use six semantic directories:

```text
assets/   passive media and its attribution
config/   files installed or linked into the home directory
docs/     decisions and examples for humans
scripts/  installation and maintenance orchestration
tools/    user-facing programs built from source
LICENSES/ third-party license texts
```

The root retains only conventional repository metadata and public entrypoints: `README.md`,
`AGENTS.md`, `.gitignore`, `.gitattributes`, `install.sh`, and `install.ps1`.

Within `config/`, application names remain explicit (`doom`, `shell`, `git`, `ctags`). Private
layer contracts still use their own `doom/pre.el`, `doom/post.el`, and `shell/init.zsh` paths;
those paths describe a layer, not this repository's layout.

Delete the in-tree `archived/` shelf. Retired code remains recoverable from Git history and
ADR-007 records why it was retired.

## Consequences

- Installers and health checks must address `config/` paths when creating symlinks.
- Doom's repository-relative Windows script lookup gains one parent directory.
- Documentation lives under `docs/decisions/`, `docs/howto/`, and
  `docs/examples/`. Provisioned Org-roam nodes belong in `docs/howto/`;
  standalone fixtures and non-roam examples remain in `docs/examples/`.
- `install.sh` and `install.ps1` stay at root because they are stable public interfaces;
  moving their implementation would require wrappers without reducing root noise.
