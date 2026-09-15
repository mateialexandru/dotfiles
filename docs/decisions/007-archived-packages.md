# ADR-007: Archived Packages (Spring Cleaning 2026-04)

**Status:** Accepted
**Date:** 2026-04-22

## Decision

Removed three feature clusters from active config. Their implementations were initially kept
in an `archived/` directory, then removed from the working tree during the KISS repository
layout cleanup (ADR-019). Git history remains the archive.

| Feature | Former file | Reason |
|---------|---------------|--------|
| `symon` (system monitor sparklines) | `archived/symon.el` | Disabled on macOS anyway; Windows backend required a custom PowerShell process — too much maintenance for a minibuffer widget |
| `alert` + `alert-toast` + ntfy.sh | `archived/ntfy-alert.el` | Compilation notifications and push alerts were rarely used in practice |
| Microsoft Roslyn LSP | `archived/roslyn-lsp.el` | Works well but adds install overhead (NuGet DLL, separate script). Not actively doing .NET work. Doom's `csharp+lsp` module remains, will fall back to OmniSharp |

## To inspect or restore any of these

1. Read the former file from the last commit that contained the shelf, for example
   `git show 8210dd2:archived/symon.el`.
2. Port the relevant blocks into `config/doom/config.el` / `config/doom/config-windows.el` / etc.
3. Re-add the `package!` declarations to `config/doom/packages.el`
4. Run `doom sync`
