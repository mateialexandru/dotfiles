---
name: launch
description: Open a new coding session in an existing directory, without creating or changing worktrees.
argument-hint: "<absolute directory or repo/task>"
---

# Launch an existing directory

Resolve `DOOM_DOTFILES_DIR` from the process environment, falling back to its
current-user environment value. If missing, report that the public dotfiles
skill installer must be run. Never assume a private checkout or organization.

First check for an optional `DOTFILES_LAUNCHER` process/current-user environment
variable naming a private launcher script. When configured, require that file
to exist and call it with `-Path <requested argument>` unchanged. The override
owns private alias and worktree-path resolution. Fail explicitly on a broken
override; do not silently substitute another tool.

Otherwise, absolute arguments identify any existing directory. Relative `repo/task` paths
resolve beneath `HACK_WORKTREE_ROOT`, defaulting to `~/worktree`. The `dotfiles`
alias resolves to `DOOM_DOTFILES_DIR`. Require an existing directory; do not
clone, fetch, create worktrees, reset branches or consult a repository registry.
For missing targets, report the path; `hack list` can list managed worktrees.

Use `<dotfiles>\scripts\launch-session.ps1 -Path <resolved directory>`.
It starts `copilot` by default, or `claude` when already inside Claude Code.
An explicit `-Command` and `-CommandArguments` can select another installed
executable. Never add permission-bypass flags or organization/MCP arguments
unless the user or their private configuration explicitly selects them.

Private aliases or deliverable lookup belong to that private integration, not
this generic skill. Pass paths as arguments; do not build executable command
strings from user input. Report launch failures and do not claim readiness just
because a process was started.
