# ADR-021: Public Pi, private local-model layer

## Context

Pi is useful across personal, work, and fresh-machine environments, so its installation
and portable behavior belong in this public dotfiles repository. Exact local model
choices, hardware descriptions, benchmark observations, and LM Studio tuning describe a
particular private machine and should not leak into the public baseline.

Pi has three global declarative files—`settings.json`, `models.json`, and `AGENTS.md`—but
does not itself provide a dotfiles-style overlay system.

## Decision

The public repository owns the Pi package, portable settings, and shared agent context in
`config/pi/`. During installation, `scripts/install-pi.sh` or `.ps1` composes that base
with optional `pi/` fragments from the existing ordered private-layer directory.

- JSON objects deep-merge in lexical layer order; an array in a later layer replaces the
  earlier array.
- `pi/AGENTS.md` files are appended after the public context.
- `pi/install.sh` or `pi/install.ps1` may converge a layer's platform-specific runtime.
- Credentials, sessions, and trust state remain unmanaged in `~/.pi/agent/`.

The public installer has no knowledge of private repository names or model identifiers.

## Consequences

- Pi is restored by the normal public dotfiles installation on every platform.
- A private layer can reproduce a particular Mac's local inference setup without making
  the public repository personal or hardware-specific.
- Generated Pi configuration is a regular managed file rather than a symlink to either
  repository.
- Activating or changing a layer requires rerunning the Pi installer; layer activation
  scripts should do this automatically.
