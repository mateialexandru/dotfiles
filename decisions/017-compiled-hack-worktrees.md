# 017 — Compiled catalog-driven worktrees

**Date:** 2026-09-14
**Status:** Accepted
**Supersedes:** ADR-006

## Context

The useful part of `hack` is being able to remember a collection of repositories and
spawn an isolated checkout for any of them. The PowerShell implementation also grew
navigation, interactive selection, status presentation, shell aliases, and cleanup UI.
Those features made the tool and its installation larger without improving the core
workflow.

A recurring correctness problem is the starting point for a task. A worktree created
from a local `develop` can silently start behind the remote branch.

## Decision

`hack` is a compiled Rust program with two responsibilities:

1. Maintain a small repository catalog in `~/.config/hack/config.json`.
2. Create and safely remove Git worktrees for catalogued repositories.

Each repository has one bare metadata/object store at `<baseDir>/.trees/<alias>`.
`hack <repo>/<task>` always performs `git fetch --prune origin` and creates a new branch
from `origin/<baseBranch>`, where the default base is `develop`. It never creates from
a local base branch. If `origin/<branchPrefix>/<task>` already exists, Hack creates a
tracking worktree for that branch instead.

The config format and disk layout remain compatible with v3 so existing catalogs and
worktrees do not need to move.

## Command surface

```text
hack repo add NAME URL [BASE]
hack repo list
hack repo remove NAME
hack REPO/TASK
hack list
hack remove REPO/TASK
```

Removal is conservative: a worktree must be clean and its branch must be merged into
the freshly fetched remote base. Repository removal refuses while worktrees remain.

## Deliberate omissions

- No editor, terminal, LLM, or agent launching. Emacs and other tools own that step.
- No `cd` or shell profile functions; a child process cannot change its parent shell.
- No fuzzy picker. Zoxide and editor project navigation already solve discovery.
- No PR/provider integration.
- No forced removal of dirty or unmerged work.
