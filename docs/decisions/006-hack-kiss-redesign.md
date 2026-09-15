# 006 — Hack KISS Redesign

**Date:** 2026-02-17
**Status:** Accepted (v3)
**Supersedes:** 005-hack-worktree-tooling.md (partially — config format and directory layout preserved)

## Context

v2 of hack.ps1 grew to 1,431 lines handling worktree management, agent launching, PR creation, SPEC.md generation, and scratch→name flow. In practice, the core need is: create worktrees, switch between 3-5 of them with fzf, and clean up when done. Agent launching, PR creation, and SPEC.md are separate concerns that don't belong in a worktree manager.

## Decision

Rewrite hack.ps1 targeting ~400 lines. Keep only worktree lifecycle commands. Require fzf for interactive selection. Drop all agent integration, PR creation, SPEC.md, and scratch→name flow.

## What Was Dropped

| Feature | Replacement |
|---------|-------------|
| `hack [repo]` (scratch worktree) | `hack repo/branch-name` — user knows task name upfront |
| `hack name "desc"` (scratch→name) | Not needed — branch named at creation |
| `hack done` (push + PR) | `gh pr create` / `az repos pr` directly |
| `hack resume` (cd + launch claude) | `hack go` + run claude yourself |
| `hack status` (agent toggle) | Not worktree management |
| `hack branch` (from remote) | Folded into `hack repo/branch` — auto-detects existing remote branches |
| SPEC.md | Branch name is the task identifier |
| Agent mode file (`~/.agent-mode`) | Removed |
| `defaultModel` config key | Removed |

## New Command Surface

| Command | Alias | Purpose |
|---------|-------|---------|
| `hack <repo>/<branch-slug>` | — | Create worktree (or checkout existing remote branch) |
| `hack go [filter]` | `g` | fzf picker + cd |
| `hack list [repo]` | `lh` | Grouped dashboard |
| `hack clean [repo]` | — | Auto-remove merged + fzf multi-select |

`workshop` subcommands unchanged: `add`, `remove`, `default`, list.

## Key Design Changes

### fzf replaces numbered picker
The old `Show-WorktreePicker` used numbered lists with `Read-Host`. Now uses fzf with `--ansi` coloring, `--preview` for git log, `--multi` for clean, and `--query` for pre-filtering. Direct jump when filter matches exactly one worktree.

### Branch-name-upfront replaces explore-first
v2's scratch→name flow assumed users don't know what they're building. In practice, users have a task name from a ticket or conversation. `hack infra/fix-auth-retry` is one command instead of three (`hack infra` → explore → `hack name "fix auth retry"`).

### `git -C` everywhere
v2 used `Push-Location`/`Pop-Location` for git operations. v3 uses `git -C $path` consistently, avoiding directory state bugs and simplifying error handling.

### Merged check simplified
v2 used `git branch --merged` (20+ lines). v3 uses `git merge-base --is-ancestor` (one command).

### No agent opinions
The worktree manager creates directories and switches between them. What you run inside them (claude, copilot, vim, etc.) is your business.

## Config Compatibility

The config format is backward-compatible. Only change: `defaultModel` is ignored (not removed from existing configs). Directory layout is identical — existing worktrees continue to work.

## Alternatives Considered

### Keep scratch flow as optional
Could have kept `hack repo` for scratch creation alongside `hack repo/branch`. Decided against it to maintain a single creation path and avoid the naming complexity.

### Built-in PR creation
Could have kept `hack done` since it's convenient. But `gh pr create` is well-known, and wrapping it adds provider-detection complexity (GitHub vs ADO) that doesn't belong in a worktree tool.
