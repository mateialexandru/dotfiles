# ADR-013: Local LLM inference — LM Studio/MLX

**Status:** Accepted (revised October 2026)
**Date:** 2026-07-26

## Context

The Apple Silicon Mac has 64 GB of unified memory. LM Studio runs MLX model
artifacts natively and serves an OpenAI-compatible API on `127.0.0.1:1234`.
The previous optional Ollama setup kept a second GGUF model store, but no
current Pi or gptel workflow needed it.

## Decision

**LM Studio is the sole configured local runtime.** The current private model
profile uses Qwen3.6 35B-A3B in 8-bit MLX for Pi, gptel quick help, and new
Waypoint lessons. The public `sys llm start [model]` command loads
`qwen3.6-35b-a3b` by default. It uses one inference slot and unloads after
one hour idle. `sys llm stop` unloads models and stops the server;
`sys llm status` reports the server and loaded models.

LM Studio model downloads remain user-managed, or are restored by an activated
private layer. The public installer provisions LM Studio and the `lms` CLI;
it no longer installs Ollama or pulls GGUF models. The obsolete Ollama model
manifest, LM Studio mirror, gptel backend, and system check were removed.

Pi advertises a 131,072-token context for this local model. LM Studio's MLX
runtime currently auto-sizes the loaded context rather than honoring a
requested length; check `lms ps` for the effective value on this machine.
Disk cleanup does not increase the Mac's unified memory available at runtime.

## Consequences

- Pi and gptel use the same local model identifier and endpoint.
- Only one large local model should normally be resident on the 64 GB Mac.
- An older Waypoint Day 1 transcript retains its original model identifier as
  historical provenance; new lesson chats use Qwen3.6.

## Alternatives considered

- **Ollama GGUF fallback** — retired after the MLX workflow became the only
  active local path and its model copies remained unused.
- **Native `mlx-lm` server** — deferred because LM Studio already provides the
  needed local API and GUI without another service to maintain.

## References

- ADR-012 — compiled `sys` CLI and health-check semantics.
- ADR-014 — gptel's LM Studio backend.
