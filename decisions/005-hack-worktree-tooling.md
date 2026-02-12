# 005 — Hack Worktree Tooling

**Date:** 2026-02-12
**Status:** Accepted (v2)

## Context

The PowerShell profile contained ~1000 lines of worktree management code, hardcoded to 4 Azure DevOps repos with Windows paths, a fixed branch prefix, and duplicated `$repoMap` across 4+ functions. v1 consolidated into `shell/hack.ps1` but still had two problems:

1. **Too many commands** — 7 top-level functions (`hack`, `resume`, `list-hacks`, `clean-hack`, `toggle-agent`, `hack-init`, `hack -Branch`) with no grouping
2. **Naming forced upfront** — branch name required before exploration, but the best names come from ideation

## Decision

Two commands total. `workshop` manages repos. `hack` handles the entire worktree lifecycle via subcommands. Explore-first flow: start with a scratch worktree, name it after you know what you're building.

## Command Surface

### `workshop` — repo management

| Command | Purpose |
|---------|---------|
| `workshop` | List registered repos |
| `workshop add <alias> <url> [baseBranch]` | Register a repo (auto-creates config on first use) |
| `workshop remove <alias>` | Unregister a repo |
| `workshop default <alias>` | Set default repo |

### `hack` — worktree lifecycle

First positional arg determines action. If it matches a known subcommand, dispatch. Otherwise treat as repo alias.

| Command | Alias | Purpose |
|---------|-------|---------|
| `hack [repo]` | — | Create scratch worktree + launch agent in plan mode |
| `hack name "description"` | — | Crystallize scratch → named branch + dir + SPEC.md |
| `hack done` | — | Push + create PR (picker if not in worktree) |
| `hack resume [filter]` | `r` | Picker + cd + launch agent (`claude --resume`) |
| `hack go [filter]` | `g` | Picker + cd only |
| `hack list` | `lh` | Status dashboard grouped by repo |
| `hack clean` | — | Interactive cleanup (merged + unnamed highlighted) |
| `hack branch` | — | Worktree from existing remote branch |
| `hack status` | — | Show current hack info + toggle agent mode |

## Key Design Choices

### Explore-first with scratch worktrees
`hack [repo]` creates `_scratch-<4hex>` dir on a `_scratch/<hex>` temp branch and launches the agent in `--permission-mode plan` with no prompt. The user explores, ideates, and plans freely. When the direction crystallizes, `hack name "description"` renames the branch and directory, writes SPEC.md. This avoids premature naming.

### Two-command surface
Everything worktree-related is a `hack` subcommand. Repo registration is `workshop`. No scattered top-level functions. Reserved subcommands: `name`, `done`, `resume`, `go`, `list`, `clean`, `branch`, `status`. Anything else is treated as a repo alias.

### PWD-based repo inference
`Resolve-RepoFromPwd` checks if `$PWD` starts with `baseDir/<alias>` and the alias is registered. Fallback chain: PWD inference → explicit arg → `defaultRepo` → error with help. Means `hack` with no args works from inside any worktree.

### Same-terminal execution
`hack` runs the agent in the same terminal via `Set-Location` + `Invoke-Expression`. When the agent exits, the user is already in the worktree directory.

### Config-driven
`~/.config/hack/config.json` stores base directory, branch prefix, default model, default repo, and repo aliases. Not committed. Auto-created on first `workshop add` — prompts for baseDir + branchPrefix inline.

### Auto-detect git provider for PRs
`hack done` inspects the remote URL to determine GitHub vs Azure DevOps and uses the appropriate CLI (`gh` or `az repos pr`). Falls back to opening a browser URL. Blocks on scratch branches with a helpful message.

### SPEC.md as lightweight metadata
YAML frontmatter stores task description, branch, model, creation date, repo, and PR URL. Only written on `hack name` (not on scratch creation). Used by `hack resume` for context and `hack list` for display.

### Grouped list output
`hack list` groups worktrees by repo via `Group-Object`. Scratch worktrees tagged `[UNNAMED]`. Shows PR URL, merge status, ahead/behind counts.

### Cross-platform via pwsh
Uses `[IO.Path]::Combine()` for path construction. Platform-conditional paths for config and agent-mode file. Install scripts for both Windows and Linux.

## Alternatives Considered

### PowerShell module instead of dot-source
Modules provide better isolation but more ceremony. Dot-sourcing keeps functions in the global scope (needed for interactive use) and is simpler to install/update.

### `Start-HackTerminal` (new windows)
Discarded in favor of same-terminal execution. New windows split context, require clipboard for paths, and don't work well with `claude --resume`.

### Description-first (v1 pattern)
`hack "add retry logic" infra` required naming before exploration. Replaced with scratch-first flow where naming is deferred until the task is understood.

## File Structure

```
shell/
  hack.ps1                  # All hack functions (~1500 lines)
  hack-config.sample.json   # Example config (committed)
scripts/
  install-hack.ps1          # Adds dot-source to PS profile
  install-hack.sh           # Adds dot-source to pwsh profile on Linux
```
