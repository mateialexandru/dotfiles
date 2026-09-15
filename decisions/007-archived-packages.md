# ADR-007: Archived Packages (Spring Cleaning 2026-04)

**Status:** Accepted
**Date:** 2026-04-22

## Decision

Removed three feature clusters from active config. Code preserved in `archived/` for potential revival.

| Feature | Archived file | Reason |
|---------|---------------|--------|
| `symon` (system monitor sparklines) | `archived/symon.el` | Disabled on macOS anyway; Windows backend required a custom PowerShell process — too much maintenance for a minibuffer widget |
| `alert` + `alert-toast` + ntfy.sh | `archived/ntfy-alert.el` | Compilation notifications and push alerts were rarely used in practice |
| Microsoft Roslyn LSP | `archived/roslyn-lsp.el` | Works well but adds install overhead (NuGet DLL, separate script). Not actively doing .NET work. Doom's `csharp+lsp` module remains, will fall back to OmniSharp |

## To restore any of these

1. Open the corresponding file in `archived/`
2. Uncomment and paste the relevant blocks back into `config.el` / `config-windows.el` / etc.
3. Re-add the `package!` declarations to `packages.el`
4. Run `doom sync`
