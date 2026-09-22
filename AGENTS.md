# AGENTS.md

Guidance for coding agents working in this repository.

## Repository

Cross-platform dotfiles for macOS, Windows, and Linux, centered on Doom Emacs.
Installers are idempotent and mostly symlink configuration into place; in particular,
`config/doom/` is linked to `~/.config/doom`, so edits are live.

The repository is the desired-state source. Do not treat local machine state as the
configuration to preserve or copy machine-specific/private values into public files.

## Working rules

- Keep changes scoped and cross-platform unless the file is explicitly platform-specific.
- Preserve installer idempotency and existing installations where possible.
- Put private or machine-specific settings in optional layers under
  `~/.config/dotfiles/layers.d/`, not in this repository.
- Read the relevant ADR in `docs/decisions/` before changing architecture, package choices,
  language tooling, installers, or security policy.
- Update `README.md` when changing installation or user-facing workflows.
- Do not commit or push unless explicitly asked.

### Windows safety

- Never bypass a Defender block.
- Do not infer organizational approval from installer switches or public package catalogs.
- Keep downloads, elevation, machine-wide changes, and upgrades explicit.
- Preserve the current policy of reusing working tools rather than reinstalling them.

## Key paths

- `config/doom/config.el` — shared Doom configuration and platform dispatch
- `config/doom/config-*.el` — feature and platform-specific Doom configuration
- `config/doom/init.el` — Doom modules
- `config/doom/packages.el` — additional Emacs packages
- `config/pi/` — portable Pi defaults; installers compose these with private layers
- `config/shell/`, `config/nushell/` — shell configuration
- `scripts/` — platform installers, updates, health checks, and provisioning
- `tools/hack/` — Rust worktree CLI
- `tools/sys/` — Rust system-operations CLI
- `docs/decisions/` — architectural decisions and constraints
- `docs/howto/` — repository-owned operational notes

## Common commands

```bash
./install.sh                         # install/reconcile macOS or Linux
~/.config/emacs/bin/doom sync       # after init.el or packages.el changes
sys check                            # macOS/Linux confidence check
```

```powershell
.\install.ps1                       # install/reconcile Windows
.\scripts\doctor.ps1                # validate Windows setup
.\scripts\test-install-tool-policy.ps1  # offline installer-policy tests
```

Changes to `config/doom/config.el` and other loaded config files do not require
`doom sync`. Changes to `config/doom/init.el` or `config/doom/packages.el` require
`doom sync` and an Emacs restart.

Run the narrowest relevant tests first. For installer or cross-platform changes, run the
platform health/doctor command when available.

## Important design references

Use the ADR index in `docs/decisions/`; especially:

- `001-emacs-packages.md` — language tooling and navigation
- `009-macos-emacs-plus.md` — macOS daemon/client setup and environment
- `012-sys-ops-cli.md` — `sys` behavior and health checks
- `013-llm-ollama-lmstudio.md` / `014-gptel-emacs-llm-client.md` — local LLM and gptel
- `015-python-uv.md` — uv, Ruff, and Pyright policy
- `017-compiled-hack-worktrees.md` — worktree behavior
- `020-nushell-parallel.md` — Nushell integration
- `021-pi-private-model-layer.md` / `023-agent-shell-pi.md` — Pi layering and Emacs integration
