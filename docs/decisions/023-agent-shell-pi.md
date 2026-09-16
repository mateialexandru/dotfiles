# ADR-023: Pi owns full-project agents in Emacs

## Context

gptel is excellent for inline rewrites, explanations, ordinary chat, and explicit
buffer context. Its persistent project chat duplicates work that Pi already handles
more completely: repository discovery, edits, commands, permissions, skills, private
model selection, and resumable sessions.

## Decision

Use agent-shell as the full-project Emacs UI and connect it to the existing Pi
installation through the pinned `pi-acp` adapter.

- `SPC o l p` and `C-c l p` start `agent-shell-pi-start-agent` in the current project.
- Pi remains the source of truth for models, authentication, context, skills, and sessions.
- gptel remains the lightweight editor-native surface. `my/gptel-project` stays available
  through `M-x` only so old `.gptel/chat.org` transcripts are not stranded.
- `scripts/install-pi.sh` and `.ps1` install both pinned executables. Doom owns the
  `shell-maker`, `acp`, and `agent-shell` Emacs packages.

## Consequences

- Terminal Pi and Emacs agent-shell behave as two clients of one configuration instead of
  two separately configured agents.
- Private local model definitions remain in the private Pi layer and are automatically
  visible from agent-shell.
- Pi sessions remain under `~/.pi/agent/sessions`; `pi-acp` keeps a small mapping under
  `~/.pi/pi-acp` so ACP sessions can be resumed.
- The adapter is an additional moving part and is still evolving, so it is pinned and
  checked by `sys check`.
- agent-shell may create `.agent-shell` UI artifacts such as screenshots. Upstream adds
  that directory to the checkout's Git `info/exclude`, keeping the repository clean without
  modifying its tracked `.gitignore`.

## Alternatives considered

- **Keep the gptel project binding** — rejected because it offers a second, weaker project
  workflow and makes the keymap ambiguous.
- **Configure models again in Emacs** — rejected because it would duplicate private Pi state
  and drift from the terminal client.
- **Launch terminal Pi inside Ghostel** — retained as a useful terminal option, but it misses
  agent-shell's native Emacs presentation and project-buffer integration.
