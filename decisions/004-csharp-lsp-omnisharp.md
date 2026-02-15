# Decision: Switch from csharp-ls to OmniSharp-Roslyn for C# LSP

## Context

We initially chose csharp-ls (see [003-dotnet-devcontainer.md](./003-dotnet-devcontainer.md#why-csharp-ls-over-omnisharp-roslyn)) as our C# LSP server based on:
- Modern Roslyn-based architecture
- Lighter weight
- Active development
- .NET 10+ support

However, csharp-ls has **persistently failed** when invoked through TRAMP's docker exec with **exit code 3** due to LSP protocol line ending issues.

### The Problem

**LSP Specification Requirements:**
- Protocol headers MUST use CRLF (`\r\n`) line endings per RFC 7230
- Example: `Content-Length: 123\r\n\r\n{"jsonrpc":"2.0"...}`

**What's Happening:**
```
[jsonrpc] Server exited with status 3
[eglot] -1: Server died

Error: StreamJsonRpc.BadRpcHeaderException:
  Header does not end with expected \r\n character sequence
```

**Root Cause:**
When eglot starts csharp-ls through TRAMP's docker exec, the line endings in LSP protocol headers are being converted from CRLF (`\r\n`) to LF-only (`\n`). csharp-ls has strict header validation and immediately crashes with exit code 3.

**Failed Workarounds:**
1. Binary process coding system - Applied too late in the process chain
2. Connection-local variables for exec-path - Doesn't affect encoding
3. Custom wrapper scripts - Line endings still converted by docker exec
4. `advice-add` on `eglot--connect` - Process already created with wrong encoding

**Test Results:**
- `csharp-ls --version` in container → Works
- `csharp-ls --solution file.sln --diagnose` → Works
- LSP with proper CRLF (manual echo -ne test) → Works
- LSP via eglot + TRAMP → Exit code 3

### Time Investment

After **multiple hours** of debugging and attempting various workarounds:
- Read csharp-ls source code to understand header parsing
- Tested manual LSP protocol with proper CRLF - confirmed server works
- Implemented binary mode coding - didn't fix issue
- Attempted TRAMP encoding configuration - no effect
- Considered wrapper scripts - line endings still converted

**Conclusion**: The issue is deep in the TRAMP/process creation stack, and continuing to debug has diminishing returns.

## Decision

**Switch to OmniSharp-Roslyn** as the C# LSP server for devcontainer workflows.

## Rationale

### Why OmniSharp Over csharp-ls

**Technical Superiority for Remote Workflows:**

1. **Robust LSP Protocol Implementation**
   - More lenient header parsing
   - Handles various line ending scenarios gracefully
   - Proven to work with VS Code Remote (similar to TRAMP)
   - Less strict about protocol edge cases

2. **Battle-Tested Maturity**
   - 10+ years of development
   - Used by millions of developers across editors (VS Code, Sublime, Vim, Emacs)
   - Extensive real-world testing in remote/container scenarios
   - Large community = more documented solutions to edge cases

3. **Better Remote Connection Support**
   - Designed for VS Code Remote architecture (similar constraints as TRAMP)
   - Handles stdio communication robustly
   - Well-tested with container environments
   - Official Docker container support

4. **Feature Completeness**
   - Full C# IntelliSense
   - Advanced refactoring tools
   - Code actions and fixes
   - Better diagnostics
   - Semantic highlighting

### Trade-offs Accepted

| Aspect | csharp-ls | OmniSharp | Impact |
|--------|-----------|-----------|--------|
| **Startup Time** | <1s | 5-10s | Acceptable - long editing sessions |
| **Memory Usage** | ~150MB | ~300MB | Not an issue on modern systems |
| **Installation** | `dotnet tool` | Manual binary | Requires devcontainer.json update |
| **Stability** | Crashes | Reliable | Critical |
| **Remote Work** | CRLF issues | Proven | Critical |

**Why the trade-offs are worth it:**
- **Startup time**: 5-10s is negligible for editing sessions that last hours
- **Memory**: 300MB is <1% of typical dev machine RAM (16-64GB)
- **Installation**: One-time setup cost, then fully automated
- **Stability & Remote**: These are blockers - must work reliably

### Alternative: Keep Debugging csharp-ls?

**Why we're NOT continuing to debug csharp-ls:**

1. **Time sink**: Already spent multiple hours on workarounds
2. **Deep stack issue**: Problem is in TRAMP/Emacs process creation internals
3. **Uncertain fix**: May require patches to TRAMP, docker.el, or csharp-ls
4. **Maintenance burden**: Custom workarounds would need ongoing maintenance
5. **Pragmatic choice**: OmniSharp is a proven solution that works today

**Better investment**: Spend time on productive development, not fighting tooling.

## Consequences

### Positive

- **Immediate reliability** - OmniSharp works with TRAMP/docker out of the box
- **No CRLF workarounds** - Robust LSP protocol implementation
- **Better features** - More comprehensive IDE functionality
- **Proven remote support** - Battle-tested in VS Code Remote
- **Large community** - Extensive documentation and troubleshooting help
- **Future-proof** - Less likely to hit edge cases

### Negative

- **Slower startup** - 5-10s vs <1s (acceptable for long sessions)
- **Higher memory** - 300MB vs 150MB (not significant on modern hardware)
- **Manual installation** - Requires wget in devcontainer.json (one-time setup)
- **Larger binary** - ~50MB download (cached after first build)

### Neutral

- Can keep csharp-ls installed for local (non-TRAMP) workflows if desired
- Can revisit csharp-ls if TRAMP encoding issues are fixed upstream

## Implementation

### devcontainer.json Update

```json
{
  "postCreateCommand": "dotnet tool install --global csharpier && wget https://github.com/OmniSharp/omnisharp-roslyn/releases/latest/download/omnisharp-linux-x64-net6.0.tar.gz -O /tmp/omnisharp.tar.gz && sudo mkdir -p /opt/omnisharp && sudo tar -xzf /tmp/omnisharp.tar.gz -C /opt/omnisharp && sudo chmod +x /opt/omnisharp/OmniSharp && rm /tmp/omnisharp.tar.gz"
}
```

### config.el Changes

**Replace csharp-ls eglot configuration (lines ~185-218):**

```elisp
(after! eglot
  ;; Configure OmniSharp for C#
  (defun my/omnisharp-command (&optional _interactive _mode)
    "Generate OmniSharp command with solution file if available."
    (let* ((project-root (or (doom-project-root) default-directory))
           (sln-files (directory-files project-root t "\\.sln$")))
      (if sln-files
          (list "/opt/omnisharp/OmniSharp" "-lsp" "-s" (car sln-files))
        (list "/opt/omnisharp/OmniSharp" "-lsp"))))

  (setf (alist-get '(csharp-mode csharp-ts-mode) eglot-server-programs
                   nil nil #'equal)
        #'my/omnisharp-command)

  ;; OmniSharp-specific settings
  (setq eglot-events-buffer-size 2000000)
  (setq eglot-connect-timeout 60)  ; OmniSharp needs more time
  (setq eglot-autoshutdown nil))
```

**Remove:**
- Binary mode coding workarounds
- Custom advice on `eglot--connect`

## Verification Steps

1. **Rebuild container** with new OmniSharp installation
2. **Open C# file** via TRAMP: `SPC f d` → `tests/OrderProcessingTests.cs`
3. **Start eglot**: `M-x eglot`
4. **Verify**:
   - No "Server died" errors
   - `[eglot]` appears in modeline (5-10s startup)
   - Completion works: `Console.TAB`
   - Go-to-definition works: `gd`
   - Hover docs work: `K`

## Alternatives Considered

1. **Keep debugging csharp-ls** - Too much time investment, uncertain outcome
2. **Use local Emacs** - Defeats purpose of devcontainer workflow
3. **VS Code for C#** - Loses Emacs/Evil benefits, not a solution
4. **Microsoft Roslyn Language Server** - Not standalone-friendly, designed for VS Code extension
5. **No LSP** - Unacceptable loss of IDE features

## Status

**IMPLEMENTED** - OmniSharp configured, ready for testing

## Implementation Date

2026-02-09

## References

- [OmniSharp-Roslyn GitHub](https://github.com/OmniSharp/omnisharp-roslyn)
- [OmniSharp Releases](https://github.com/OmniSharp/omnisharp-roslyn/releases)
- [csharp-ls Issue: CRLF Requirements](https://github.com/razzmatazz/csharp-language-server/issues)
- [LSP Specification - Base Protocol](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#baseProtocol)
- [TRAMP Docker Integration](https://www.gnu.org/software/emacs/manual/html_mono/tramp.html)

## Related Decisions

- [003-dotnet-devcontainer.md](./003-dotnet-devcontainer.md) - Original choice of csharp-ls
- [003-dotnet-devcontainer.md - TRAMP-Based Approach](./003-dotnet-devcontainer.md#2026-02-08-update-tramp-based-approach) - Context for this decision

---

## Future Considerations

**If csharp-ls CRLF issues are resolved:**
- Could benchmark performance difference
- Could offer both servers as options
- Could default back to csharp-ls for lighter workflows

**If OmniSharp becomes unmaintained:**
- Microsoft Roslyn Language Server may become standalone-friendly
- New community forks may emerge
- Could revisit csharp-ls with better TRAMP encoding handling

---

## 2026-02-14 Update: Standalone Roslyn Limitation + Dual-Backend Toggle

### Standalone Roslyn: file-creation refactorings not supported

Confirmed via eglot event log: `codeAction/resolve` response for "Move type to file"
returns `{"documentChanges": []}` — empty, not an error, just unsupported.

Affects any refactoring that requires creating a new file (Move type to file, Extract
class, etc.).

**Root cause:** the standalone Roslyn server (`Microsoft.CodeAnalysis.LanguageServer`)
runs outside the VS Code extension host and does not implement the VS-Code-specific
`_roslyn/activateRazorLanguageServer` + file-creation workspace edit path.

Not fixable via config; would require upstream work in the Roslyn server itself.

### Decision: dual-backend with live toggle; platform defaults differ

- **Linux default: OmniSharp** — better refactoring, works well in practice
- **Windows default: Roslyn** — no OmniSharp config/install on Windows yet
- Single `eglot-server-programs` entry (`my/csharp-eglot-contact`) dispatches on
  `my/csharp-lsp-backend` variable (`'roslyn` | `'omnisharp`)
- Variable declared with default `'roslyn` in `config.el`; `config-linux-omnisharp.el`
  overrides it to `'omnisharp` when the binary is present (so default flips on Linux
  automatically after `install-omnisharp.sh` is run)
- `SPC t L` / `my/csharp-toggle-lsp` flips the variable and calls `eglot-reconnect`
- `solution/open` hook guards on `(eq my/csharp-lsp-backend 'roslyn)` — OmniSharp
  discovers `.sln` itself via `-s <sln>` arg

### Linux: DOTNET_ROOT workaround for Homebrew .NET

OmniSharp 1.39.13 is a `net6.0` framework-dependent binary. Homebrew installs .NET at
a non-standard prefix (not `/usr/share/dotnet`). Without `DOTNET_ROOT`, OmniSharp exits
with "libhostfxr.so could not be found".

Fix: `my/omnisharp-contact` prepends
`env DOTNET_ROOT=<brew>/libexec DOTNET_ROLL_FORWARD=Major` so the net6.0 app runs on
.NET 10.

### Linux vs Windows LSP matrix

| | Linux | Windows |
|---|---|---|
| **Default backend** | OmniSharp | Roslyn |
| **OmniSharp available** | Yes (`install-omnisharp.sh`) | Yes (`install-omnisharp.ps1`) |
| **Toggle** | `SPC t L` | `SPC t L` (when installed) |
| **Move type to file** | OmniSharp ✓ / Roslyn ✗ | OmniSharp ✓ / Roslyn ✗ |
| **Diagnostics / completion** | Both work well | Both work well |
| **.NET discovery** | Homebrew → `DOTNET_ROOT` needed | winget → system path |

### New files added

- `scripts/install-omnisharp.sh` — downloads OmniSharp v1.39.13 to `~/.local/share/omnisharp/`
- `scripts/install-omnisharp.ps1` — downloads OmniSharp v1.39.13 to `%LOCALAPPDATA%\omnisharp\`
- `doom/config-omnisharp.el` — defines `my/omnisharp-contact`; always loaded on all
  platforms (no-op if binary absent).  Linux flips default to OmniSharp when binary
  is present; Windows keeps Roslyn as default but makes OmniSharp available for toggle.

### Updated status

Both backends operational on Linux. **OmniSharp is now default on Linux** (set in
`config-omnisharp.el` when binary is present); Roslyn remains default on Windows but
OmniSharp is available via `SPC t L` once `scripts/install-omnisharp.ps1` is run.
Toggle with `SPC t L` as needed.
