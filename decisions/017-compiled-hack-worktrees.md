# 017 — Compiled discovered worktrees

**Date:** 2026-09-14
**Status:** Accepted
**Supersedes:** ADR-006

## Context

The useful part of `hack` is being able to work cleanly across a collection of repositories
and spawn an isolated checkout for any of them. The PowerShell implementation also grew
navigation, interactive selection, status presentation, shell aliases, and cleanup UI.
Those features made the tool and its installation larger without improving the core
workflow.

A recurring correctness problem is the starting point for a task. A worktree created
from a local `develop` can silently start behind the remote branch.

## Decision

`hack` is a compiled, dependency-free Rust program with two responsibilities:

1. Discover existing Git repositories beneath conventional source roots.
2. Create and safely remove Git worktrees for those repositories.

The existing checkout is the source of truth for its `origin` and Git object store; Hack
does not maintain a second catalog or clone. Repositories are indexed beneath `~/Source`,
plus any paths supplied by `HACK_SOURCE_ROOTS`. The generated index lives at
`~/.cache/hack/repos`. Normal lookup reads only that file. A missing name or stale path
triggers one filesystem-only rescan; scanning never launches Git once per directory.
`hack repos --refresh` forces the same refresh explicitly. Worktrees live beneath
`~/worktree`, overridable with `HACK_WORKTREE_ROOT`.

`hack <repo>/<task>` always performs `git fetch --prune origin`. It creates from
`origin/develop` when that branch exists, otherwise from `origin/HEAD`. It never creates
from a local base branch. Local Git config `hack.baseBranch` handles exceptions. If the
task branch already exists on the remote, Hack creates a tracking worktree for it instead.

## Command surface

```text
hack REPO/TASK
hack repos [--refresh]
hack list
hack remove REPO/TASK
```

Removal is conservative: a worktree must be clean and its branch must be merged into
the freshly fetched remote base.

## Deliberate omissions

- No editor, terminal, LLM, or agent launching. Emacs and other tools own that step.
- No `cd` or shell profile functions; a child process cannot change its parent shell.
- No repository-add step or separate JSON configuration.
- No fuzzy picker. Zoxide and editor project navigation already solve navigation.
- No PR/provider integration.
- No forced removal of dirty or unmerged work.
