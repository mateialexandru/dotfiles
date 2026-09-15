# ADR-013: Local LLM inference — Ollama managed + LM Studio user-managed

**Status:** Accepted
**Date:** 2026-07-26

## Context

The rig had no local-LLM layer managed as desired state — only the `lm-studio` cask,
installed inline in `install.sh`. On an M5 Pro / 64 GB Mac local inference is genuinely
useful for daily/agentic/editor work, and it should be provisioned and verified like
everything else here: idempotent installer, `sys` control, `sys check`, ADR.

The design explored whether one runtime could serve everything and whether the two tools
could share a model store. Two hard facts settled it:

- **GGUF ≠ MLX.** Ollama loads **GGUF**; LM Studio loads both GGUF and **MLX**
  (Apple-Silicon-native, faster + lower memory on the M5). No single file is both.
- **Ollama's store is content-addressed and self-contained — it always *copies* a model
  in.** You cannot point Ollama at an external file another app also uses. So true
  zero-duplication sharing between the two is architecturally impossible, in any format.

Given that, "share one store" is a dead end. The useful split is to let each tool do what
it's best at and bridge one direction only.

## Decision

**Ollama is the managed runtime; LM Studio is kept but user-managed; a best-effort symlink
mirror makes Ollama's models also visible in LM Studio.**

- **Ollama (Homebrew formula)** — the desired-state runtime. GGUF, scriptable, reproducible
  from `scripts/ollama-models.txt`, OpenAI-compatible endpoint `localhost:11434`. This is
  the SOTA-for-64 GB daily/reasoning/coding set the installer owns.
- **LM Studio (cask)** — kept as the GUI + MLX playground, but **not scripted**: the user
  downloads whatever MLX models they want in the app themselves. The installer only ensures
  it's present (and `lms bootstrap`s the CLI onto PATH for convenience).
- **`sys llm mirror`** — symlinks each Ollama GGUF model blob into
  `~/.lmstudio/models/ollama/<name>-<tag>/<name>-<tag>.gguf` so the managed Ollama models
  also appear in LM Studio, labelled `ollama`. It walks Ollama's on-disk manifests (no
  daemon needed), and **prunes any link that no longer resolves** — self-healing against
  Ollama's content-addressed GC/re-pull. Run automatically at the end of the installer.

This is an explicitly-accepted convenience crutch (symlinks into LM Studio), scoped to the
one direction that works: LM Studio *can* read GGUF, so it can use Ollama's files; the
reverse (Ollama reading LM Studio's MLX) cannot work.

Rejected: native mlx-lm (a Python env to maintain — fragile on the box's Python 3.14 — for
fine-tune/day-one-weights wins not yet needed); making LM Studio the managed runtime (the
user prefers to manage its models by hand); forcing a single shared GGUF store (impossible
with Ollama's copy-in design, and it would cost LM Studio its MLX edge).

### One script owns the layer

`scripts/install-llm-mac.sh` (the old inline LM Studio block left `install.sh`):
`brew install --cask lm-studio` + `brew install ollama`, bring the daemon up transiently
to `ollama pull` the manifest, then run `sys llm mirror`.

### On-demand daemon, not a login service

The user wants Ollama up only when working. `sys llm <action> [model]`:

- `sys llm start [model]` — `brew services run ollama` (**run**, not `start`: starts now
  *without* registering for login/boot), waits for `:11434`, then warms a model resident
  (empty-prompt `/api/generate` with `keep_alive: -1` → stays until `stop`). Default model
  the all-rounder `gpt-oss:20b`; override e.g. `sys llm start qwen3-coder:30b`.
- `sys llm stop` — `brew services stop ollama`.
- `sys llm status` — endpoint reachable? then `ollama list` + `ollama ps`.
- `sys llm mirror` — (re)sync the LM Studio symlinks (above).

### `sys check` (ADR-012 rule 1: gate on the symptom, not the remedy)

Check 13 in `scripts/sys-health.sh`. Only a missing `ollama` binary is a `[X]` fail
(`sys install`); the daemon is on-demand so down is advisory `[..]`. Manifest models are
compared against `ollama list` when up. LM Studio is user-managed, so it is deliberately
**not** gated here.

### Model set — curated GGUF for 64 GB

MoE picks (few active params → big quality, fast decode) co-reside with headroom:

- `gpt-oss:20b` — all-rounder / reasoning / daily driver (~13 GB MoE, fast).
- `qwen3-coder:30b` — coding (30B MoE, 3.3B active, ~19 GB Q4, 256K ctx).
- `nomic-embed-text` — embeddings (~274 MB; RAG / future org-roam).

Documented heavier swap in the manifest: `qwen3-coder-next:80b` (~38 GB, run solo).

## Consequences

- One managed endpoint (`:11434`) any agent/editor targets; a committed text manifest.
- LM Studio stays a first-class MLX GUI the user drives; Ollama's models show up there too
  via the mirror, no manual linking.
- The daemon costs nothing when idle (on-demand; no login RAM/battery).
- `sys check` tracks the managed surface (Ollama); LM Studio is intentionally untracked.
- Some models may exist twice (GGUF in Ollama, MLX in LM Studio) — unavoidable and fine;
  each copy is the format its runtime is fastest on.
- Fine-tuning / day-one HF weights aren't available until/unless mlx-lm is added later.

## Alternatives considered

- **Single shared store** — impossible: Ollama always copies in, and GGUF≠MLX would nerf
  LM Studio's MLX.
- **LM Studio as the managed runtime (MLX, `lms get`)** — rejected: the user prefers to
  manage LM Studio's models by hand; Ollama is the reproducible, scriptable endpoint.
- **mlx-lm native** — deferred: a Python env (fragile on 3.14) for wins not yet needed.
- **Login-persistent daemon (like Emacs, ADR-009)** — rejected: local inference is bursty;
  `sys llm start` on demand beats always-on RAM.

## References

- ADR-009 — the emacs-plus daemon/login-service pattern this deliberately diverges from.
- ADR-012 — compiled `sys` CLI + `sys check`; the `llm` command and check 13 extend it,
  following "gate on the symptom, not the remedy."
