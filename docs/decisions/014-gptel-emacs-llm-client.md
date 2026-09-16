# ADR-014: gptel in Emacs — layered providers and editor-native workflows

**Status:** Accepted
**Date:** 2026-08-01

## Context

Emacs previously had exactly one LLM surface: `claude-code` in a vterm side buffer (later
removed by ADR-018). That was an external CLI agent — good at "go do this", useless for "explain
this region", "rewrite this paragraph", "write this commit message". There was no in-buffer
client, no way to aim a model at a local endpoint, and no place to grow tools.

At the same time ADR-013 stood up a managed OpenAI-compatible endpoint at `localhost:11434`
that nothing in Emacs consumed, and Doom's `:tools llm` module sat commented out in `init.el`.

The personal environment has a ChatGPT subscription, while a work environment may use a
different provider. Provider choice therefore belongs to a private layer, not the portable
public configuration.

## Decision

**Enable Doom's `:tools llm` module, expose a provider-neutral primary-backend slot, keep local
Ollama as the portable fallback, and layer `gptel-agent` on top for tool-enabled sessions.** The
personal layer fills that slot with ChatGPT OAuth; another layer can register another provider.

### Build on `:tools llm`, don't bypass it

The module is not a thin gptel wrapper. It brings `gptel-quick` (explain at point),
`gptel-magit` (generated commit messages in magit), `ob-gptel` (org-babel `gptel` blocks with
completion-at-point), a Doom popup rule for gptel buffers, and a `SPC o l` leader map with nine
bindings. `:tools magit` and `:lang org` are both on here, so all of it activates.

Consequently `config/doom/config-gptel.el` never re-declares what the module owns —
`gptel-default-mode`, `gptel-display-buffer-action` and the popup rule are left alone, and our
keys are *added* to `SPC o l` rather than given a second prefix of their own. `C-c l` mirrors
the map without the leader; it is free because Doom only binds `doom-localleader-alt-key` to
`C-c l` in non-evil setups.

### Personal layer: OAuth, not an API key

`gptel-make-openai-oauth` targets the Codex endpoint on `chatgpt.com` with a ChatGPT Plus/Pro
login. **ChatGPT Plus does not include OpenAI platform API access** — they are separate
products with separate billing — so an API-key setup would have meant paying twice for a
subscription the user already has. The module's README still says an API key is required; that
predates OAuth support in gptel. The personal layer never touches `gptel-api-key`.

The provider declaration lives in the personal private layer's `doom/post.el`:

```elisp
(after! gptel
  (my/gptel-register-primary-backend
   (gptel-make-openai-oauth "ChatGPT")
   'gpt-5.6-sol
   #'gptel-openai-oauth-p))
```

The final predicate tells the public compatibility plumbing that this backend rejects optional
generation parameters. A work layer can register its own backend and model without inheriting
that ChatGPT-specific behavior.

Two consequences worth knowing:

- The refresh token is written to `~/.config/emacs/.cache/gptel-openai/openai-oauth-token`, not
  to auth-source. Nothing in this repo, and nothing to encrypt via `~/.authinfo.gpg`.
- Codex models reject both `temperature` and `max_output_tokens`, warning on **every**
  request if either is present. `gptel-temperature` is set to nil globally. `max_output_tokens`
  is the awkward one: gptel-agent raises `gptel-max-tokens` to 8192 in every agent buffer
  (sensible for backends whose default is low), which made the ChatGPT backend warn
  constantly. `my/gptel--omit-generation-parameters-p` gates it using the predicate supplied
  by the private layer — our own project command skips the raise, and an `:around` advice on
  `gptel-agent` binds the variable non-nil for the call so upstream's `unless` no-ops. Ollama
  buffers still get 8192. `my/gptel-toggle-backend` clears/restores the buffer-local value when
  an agent buffer moves between the two endpoints.

Login is `M-x gptel-openai-oauth-login`, or automatic on first request. The default
`authorization-code` flow needs a localhost callback on port 1455, which is fine for the local
daemon; over SSH, `gptel-openai-oauth-login-method` must be set to `device`.

### Primary provider and local backend, one key apart

The latest private layer to call `my/gptel-register-primary-backend` selects the primary provider
and makes it active. With no private provider, public gptel remains useful through `Ollama`, which
points at ADR-013's managed endpoint with the models from `scripts/ollama-models.txt`.
`my/gptel-toggle-backend` (`SPC o l b`, also in the `SPC t n` toggle menu) flips between the
registered primary and Ollama and remembers the model last used with each — switching backend
without switching model would send a model name the other end has never heard of.

That daemon is on-demand, so the toggle probes `/api/tags` first and points at
`sys llm start` instead of failing mid-request.

### Quick lookups run local

`gptel-quick` (`SPC o l e`) comes from the module, but the module leaves its model unset, so it
inherits the session backend — and upstream states plainly that it does not work with reasoning
models, which emit their thinking ahead of the answer and fill the popup with it. Every Codex
model is a reasoning model, so the default is the failing case; the endpoint also rejects the
`max_output_tokens` gptel-quick uses to size the reply, warning once per request.

So `gptel-quick-backend`/`gptel-quick-model` are bound around the command to Ollama's
`qwen3-coder:30b` — an instruct model, and a twelve-word lookup off quota. When the on-demand
daemon is down the command falls through to the session's backend rather than erroring, since a
degraded lookup beats none.

The `/api/tags` probe is cached for a minute. Unlike the backend toggle, which is a deliberate
act, a lookup is meant to cost nothing to reach for, and a curl round-trip per invocation is
exactly the friction the command exists to avoid.

`?` in `embark-general-map` runs the same command on a completion candidate, an identifier or a
region — upstream's own suggestion, and the key is free in both embark's map and Doom's.

The popup's transient map already carries the whole escalation ladder: `+` for a longer answer,
`M-w` to copy, `M-RET` to seed a gptel chat buffer with the query and the answer (org, `gptel-mode`
on, so `SPC o l s` continues it and `C-x C-w` keeps it with backend and `GPTEL_BOUNDS` intact).
None of that needed building — but `M-RET` could not be pressed. A GUI frame sends `M-<return>`,
and Emacs only falls back to `M-RET` when nothing else claims it; `org-mode-map` claims it for
`org-ctrl-c-ret`. So upstream's follow-up key was dead in exactly the buffers where a lookup most
wants a follow-up, and fine everywhere else — which reads as flaky rather than broken.

The fix is aliases onto upstream's own closures, not reimplementations: `r` → `M-RET`,
`M-<return>` → `M-RET`, `w` → the `kill-ring-save` remap. Single characters need no meta and no
function-key translation, and a one-char binding in an `overriding-terminal-local-map` outranks
evil's own `r`/`w` for as long as the popup lives. Since the commands stay upstream's, there is
nothing to keep in sync if they change.

Upstream builds that map inside `gptel-quick--callback-posframe` out of `cl-flet` closures, so
there is no hook and no map to reach from outside — but it hands the finished map to
`set-transient-map` synchronously, and intercepting that call is enough. That is the assumption
to revisit if the aliases ever stop appearing. `gptel-quick-timeout` goes from 10s to 20s: the
map dies with the popup, and deciding to escalate takes longer than reading.

The chat buffer runs on the session backend, since `my/gptel-quick--prefer-local` binds Ollama
only around the `gptel-quick` call. That is the ladder working as intended — the local model
answers the twelve-word question off quota, the layer-selected primary model picks up the
conversation.

### Inline rewrites default to optimistic apply

Visual `SPC r` is the hot path: select a region, describe the change, and let the completed
response replace it automatically. `SPC o l r` and `C-c l r` remain discoverable aliases. The
delete-and-insert pair is bounded and amalgamated into one undo step, so Evil's ordinary `u`
rejects the whole applied rewrite and `C-r` restores it. `SPC o l R` keeps upstream's review-first
overlay flow for changes that deserve a diff or another iteration before acceptance.

The requested default action is buffer-local because the rewrite completes asynchronously; a
dynamic binding would disappear before the response arrived. Each entry command sets the policy
explicitly, which also prevents an automatic rewrite in one buffer from changing another.

### gptel-agent for the agentic layer

`karthink/gptel-agent` — gptel's own author — supplies what the module doesn't: 16 core tools
(`Bash Eval WebSearch WebFetch YouTube Diagnostics Mkdir Edit Insert Write Glob Read Grep
TodoWrite Skill Agent`) plus ~16 Emacs introspection tools, sub-agents defined as md/org files,
skills read from the upstream-compatible `~/.claude/skills/` path (this does not require the
Claude Code CLI), an [Agent]/[Plan] header toggle, `gptel-agent-compact`,
and TRAMP support (so it works against ADR-011's remote hosts with no Emacs on them).

Its per-tool `:confirm` flags already encode the wanted policy — `Read`/`Grep`/`Glob`/
`WebSearch`/`WebFetch` run free, `Bash`/`Eval`/`Edit`/`Write`/`Insert`/`Mkdir`/`Agent` prompt —
so the global `gptel-confirm-tool-calls` is left at its default rather than forced on, which
would make even `Read` prompt.

Sub-agent calls are routed to the local box (`gptel-agent-preset` → Ollama / `qwen3-coder:30b`)
so delegated grunt work costs no subscription quota.

Two packaging constraints, both load-bearing:

- The module pins gptel to a release commit; gptel-agent requires gptel from **master**. Hence
  `(unpin! gptel)` in `packages.el`. `gptel-quick`/`gptel-magit`/`ob-gptel` stay pinned, and the
  unpin is one line to revert if an upstream gptel change ever breaks them.
- The recipe must carry `:files (:defaults "agents")` — copied from upstream's MELPA recipe.
  Without it the bundled `executor`/`researcher`/`introspector`/`gptel-plan` definitions are
  not installed.

### Legacy project sessions persist in-repo

`M-x gptel-agent` builds a throwaway buffer. `my/gptel-project` can put the same session in
`<project-root>/.gptel/chat.org`: a real file, so it survives daemon restarts and is greppable
next to the code it discusses. ADR-023 moved `SPC o l p` to Pi through agent-shell; this helper
is retained unbound so existing transcripts remain accessible.

Applying the agent preset to a file-visiting buffer uses `gptel--apply-preset`, a private
function — it is what `gptel-agent` itself calls, and the only preset entry point that isn't
per-request. **If a future gptel drops it, this is the thing that breaks**; plain `gptel-agent`
stays bound at `SPC o l A` as the escape hatch.

Because transcripts live inside other repositories, they must never be committable, and that
has to be solved once rather than per repo. `install.sh` links `config/git/ignore` to
`~/.config/git/ignore` — git's XDG default, so no `core.excludesfile` setting is needed — and
`sys check` gates on the symptom: `git check-ignore .gptel/chat.org` must resolve.

### Growth surfaces

Two drop-in directories, both under `config/doom/` so they ride the existing `~/.config/doom` symlink
and are live without a sync (the `config/doom/remote/tmux.conf` trick):

- `config/doom/gptel/agents/` — one md/org file per sub-agent; only `description` is mandatory.
  Seeded with `reviewer.md` as a worked example.
- `config/doom/gptel/tools.el` — deliberately thin, for tools only the dotfiles can answer
  (`sys_check`, `ollama_models`). General filesystem/shell/web tools come from gptel-agent.
  Names listed in `my/gptel-extra-tools` are appended to the agent preset by an `:after` advice
  on `gptel-agent-update`, since the preset's tool list is rebuilt from upstream's
  `agents/gptel-agent.md` on every update.

## Consequences

- The public repository contains no remote-provider or account choice. Personal ChatGPT OAuth
  lives in a private layer; work layers may register a different subscription backend.
- gptel is the lightweight Emacs LLM surface for chat, inline explanation, and rewrites.
  ADR-023 adds agent-shell as the separate full-project surface backed by Pi.
- Doom's LLM ecosystem is now on: org-babel `gptel` blocks work in Org notes, magit offers
  generated commit messages, `SPC o l e` explains at point.
- Repositories only grow a `.gptel/` directory when the unbound legacy project command is used.
  It remains invisible to git via the global ignore.
- gptel now tracks master. Faster access to things like OAuth support, at the cost of the
  occasional breaking change — `sys doom sync` is where that would surface.
- One private-API dependency (`gptel--apply-preset`), documented above.

## Alternatives considered

- **Declaring gptel ourselves and leaving `:tools llm` off** — the original plan. Rejected: it
  would have thrown away gptel-quick, gptel-magit, ob-gptel and the popup integration, and
  fought Doom's existing `SPC o l` map for no gain.
- **An OpenAI platform API key** — rejected: separate billing from the subscription the user
  already pays for.
- **Hand-rolled tools** (`read_file`, `ripgrep_search`, `write_file`, …) — rejected once
  gptel-agent was found: a strictly worse, unmaintained subset of the same thing.
- **`skissue/llm-tool-collection`** — rejected as a second collection: its filesystem/buffer/
  search tools overlap gptel-agent's nearly one-for-one and would register competing
  near-duplicate names. Still available for cherry-picking a single tool:
  `(apply #'gptel-make-tool llm-tc/list-directory)`.
- **Central transcript store** (`~/.local/share/gptel/<project>.org`) — rejected in favour of
  in-repo `.gptel/`, which keeps the conversation next to the code; the cost is the global
  gitignore dependency.
- **Ephemeral chat buffers only** — rejected: every conversation would die with the daemon.

## References

- ADR-011 — the remote workflow gptel-agent's TRAMP support plugs into.
- ADR-012 — `sys` + `sys check`; check 14 follows "gate on the symptom, not the remedy."
- ADR-013 — the managed Ollama endpoint this uses as its second backend and sub-agent runtime.
