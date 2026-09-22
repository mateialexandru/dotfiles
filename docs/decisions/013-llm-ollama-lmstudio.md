# ADR-013: Local LLM inference — LM Studio/MLX primary, Ollama fallback

**Status:** Accepted
**Date:** 2026-07-26

## Context

The machine is an Apple-Silicon Mac with enough memory for useful local inference. Two
local runtimes are available, but they serve different formats:

- **MLX** is Apple-Silicon-native and is the preferred runtime for MLX model artifacts.
- **Ollama** loads GGUF and provides a mature, scriptable, OpenAI-compatible service.
- **GGUF ≠ MLX.** Ollama cannot consume LM Studio's MLX files, and Ollama's
  content-addressed store copies models into its own directory.

The original version of this decision made Ollama managed and LM Studio user-managed.
That made the default local path use the less suitable runtime for the models actually
used on this Mac, and caused `sys llm start` and Emacs to warm Ollama unnecessarily.

## Decision

**LM Studio is the primary local runtime. Ollama remains an optional second-class GGUF
fallback.**

### LM Studio / MLX

- LM Studio is installed as the macOS local-LLM GUI.
- The `lms` CLI is bootstrapped onto PATH.
- MLX models are selected and downloaded by the user in LM Studio.
- The local server uses the OpenAI-compatible endpoint `http://127.0.0.1:1234`.
- `sys llm start [model]` starts the server and loads
  `qwen3.6-35b-a3b` by default. `qwen3.8-27b` is the larger alternative.
- `sys llm stop` unloads LM Studio models and stops its server.
- `sys llm status` reports the primary server and loaded models.

The default MLX model is intentionally a model key, not a desired-state download:
LM Studio remains the place where model files are chosen and managed.

### Ollama fallback

Ollama remains installed by the macOS local-LLM installer when the optional local layer
is enabled. Its manifest and GGUF store are retained for models or integrations that
need Ollama:

```text
sys llm ollama start [model]
sys llm ollama stop
sys llm ollama status
```

Ollama is not started by `sys llm start`, is not the default gptel backend, and is not
required for Pi's MLX models. `scripts/ollama-models.txt` remains a reproducible fallback
manifest.

### Optional mirror

`sys llm mirror` symlinks Ollama's GGUF blobs into LM Studio's model tree. LM Studio can
read those GGUF files, so this is a convenience for viewing them in one UI; it does not
make MLX models available to Ollama and does not eliminate format-specific stores.

## Consequences

- Pi and gptel use LM Studio/MLX by default on this Mac.
- Native MLX models are not needlessly duplicated as Ollama GGUF files.
- `sys llm` has one obvious daily path, while GGUF compatibility remains available.
- The LM Studio server and model are still on-demand, so large models do not consume RAM
  when not working.
- The installer still provisions the optional Ollama fallback for compatibility; users
  who do not need GGUF can omit the local LLM layer or uninstall Ollama afterward.
- Only one large local model should normally be resident at a time on a 64 GB machine.

## Alternatives considered

- **Ollama as the primary runtime** — rejected for this Mac's MLX-first workflow; retained
  as a useful GGUF fallback.
- **LM Studio as a fully desired-state model manager** — rejected because model choice and
  downloads are intentionally user-managed.
- **A single shared store** — impossible across GGUF and MLX, and Ollama copies files into
  its own content-addressed store.
- **Native `mlx-lm` as another runtime** — deferred; LM Studio already provides the needed
  MLX server and GUI without another Python environment to maintain.

## References

- ADR-012 — compiled `sys` CLI and health-check semantics.
- ADR-014 — gptel's LM Studio local backend and Ollama fallback.
