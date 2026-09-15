# 018 — Drop Claude Code

**Date:** 2026-09-14
**Status:** Accepted

## Context

Claude Code had been installed as a separate CLI and exposed inside Emacs through
`stevemolitor/claude-code.el`. The Claude Code subscription was discontinued. Keeping its
installer, package, keybindings, and Windows notification hook would leave a dead interface
and imply that a second paid AI service is part of a clean-machine setup.

ADR-014 has since established gptel as the integrated Emacs LLM surface, using the existing
ChatGPT subscription and the local Ollama runtime. Worktree creation is independently handled
by Hack and does not launch an AI client.

## Decision

Remove Claude Code from the Unix and Windows installers, Doom packages and configuration,
notification hooks, health verification, and active documentation. Do not archive the small
integration: its package declaration and bindings are straightforward to reconstruct from Git
history if ever needed.

The `~/.claude/skills/` name may still appear in gptel-agent documentation because that is an
upstream-compatible skill discovery path; it does not install or invoke Claude Code.

## Consequences

- A clean installation does not install the Claude CLI or configure an Anthropic account.
- `SPC o c`, `SPC o C`, and the Claude-specific `SPC c` actions are freed.
- `SPC o l` / `C-c l` remain the single LLM interaction surface in Emacs.
- Windows keeps the generic BurntToast dependency used by Doom Sync notifications.
