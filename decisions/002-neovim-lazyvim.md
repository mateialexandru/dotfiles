# 002 - Neovim with LazyVim

## Status

Accepted

## Context

Adding Neovim as a secondary editor alongside Doom Emacs, primarily for scenarios where a lighter-weight editor is useful (quick edits, remote SSH, systems without Emacs). Need a Neovim distribution that provides a good out-of-box experience with minimal configuration.

## Decision

### LazyVim as the Neovim distribution

**Chosen:** LazyVim (folke/LazyVim)

**Alternatives considered:**
- **kickstart.nvim** — Educational starter, but requires building everything yourself. More work to maintain.
- **NvChad** — Good defaults but less flexible plugin management.
- **AstroNvim** — Feature-rich but heavier abstraction layer.
- **Vanilla Neovim** — Maximum control but significant setup effort for a secondary editor.

**Rationale:** LazyVim provides sensible defaults with easy overrides via the `plugins/` directory. Uses lazy.nvim for plugin management which is the current standard. Well-maintained by folke (author of many core Neovim plugins).

### C# Language Server: roslyn.nvim

**Chosen:** seblyng/roslyn.nvim

**Alternatives considered:**
- **omnisharp-vim** — Older, uses OmniSharp which is being sunset in favor of Roslyn.
- **csharp.nvim** — Less maintained.
- **Built-in LSP with manual Roslyn config** — Roslyn's stdio protocol has quirks (solution/open notification) that roslyn.nvim handles.

**Rationale:** roslyn.nvim handles its own Roslyn server download and manages the `solution/open` notification automatically. No need for manual NuGet package installation or DLL management — unlike our Eglot setup which requires `scripts/install-roslyn-lsp.sh`.

### Treesitter compilation: system clang/gcc

**Chosen:** Use clang (Windows, via LLVM.LLVM already installed by setup-doom.ps1) or gcc (Linux, available natively).

**Alternatives considered:**
- **zig cc** — Would require installing zig and setting global CC=zcc, which pollutes the environment for all C compilation on the system.

**Rationale:** Treesitter auto-detects available C compilers (cc, gcc, clang). LLVM is already a dependency for Doom Emacs on Windows. No additional toolchain needed.

### Config-in-repo (not cloned starter)

The `nvim/` directory is committed to this repo and symlinked, matching the pattern used for `doom/`. This avoids:
- Cloning the LazyVim starter and patching files in-place
- Fragile regex-based file modifications
- Config that isn't version-controlled

### luarocks disabled

`rocks = { enabled = false }` in lazy.lua because luarocks has known issues on Windows. No current plugins require it.

## Consequences

- Neovim config is version-controlled and portable like the Doom config
- `install.ps1` / `install.sh` handle symlinks and dependency installation
- Adding new Neovim plugins: create a file in `nvim/lua/plugins/` and restart Neovim
- Roslyn server is downloaded automatically by roslyn.nvim on first use (no install script needed)
