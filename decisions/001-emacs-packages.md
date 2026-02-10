# ADR-001: Emacs Package Choices for Long-term Support

**Status:** Accepted
**Date:** 2026-01-31

## Context

Choosing Emacs packages for language support, code navigation, and shell enhancement. Priority: long-term maintenance, stability, and integration with Emacs core.

## Decisions

### 1. LSP Client: eglot (not lsp-mode)

**Choice:** `eglot` with Doom's `+eglot` flag

**Rationale:**
- Built into Emacs core since v29 (GNU-maintained, guaranteed long-term support)
- Minimal design philosophy - does one thing well
- No external dependencies
- lsp-mode is feature-rich but heavier, more dependencies, community-maintained

### 2. C# Language Server: Microsoft Roslyn LSP

**Choice:** `Microsoft.CodeAnalysis.LanguageServer` (same server VS Code uses)

**Rationale:**
- **The reference implementation:** This is the exact C# language server that powers VS Code's C# extension
- Active development by Microsoft, best feature coverage
- OmniSharp-Roslyn is deprecated in VS Code (2025)
- SofusA/csharp-language-server is deprecated
- csharp-ls (razzmatazz) is community-maintained, active, but third-party

**Installation:** `scripts\install-roslyn-lsp.ps1` (downloads NuGet package, extracts to `%LOCALAPPDATA%\roslyn-lsp\`)

**Launch:** `dotnet Microsoft.CodeAnalysis.LanguageServer.dll --stdio` (eglot launches directly, stdio transport)

**Transport:** Roslyn v5.0.0+ supports `--stdio` (added in [PR #76437](https://github.com/dotnet/roslyn/pull/76437), January 2025). This makes Roslyn use standard stdin/stdout for LSP communication — the same transport eglot uses with clangd, pyright, and every other LSP server. Neovim's `roslyn.nvim` also uses `--stdio`.

**Note:** Requires a `.sln` or `.csproj` in the project - won't work on loose `.cs` files.

#### Capabilities & Keybindings

Roslyn reports a rich set of LSP capabilities. The table below documents what's available and how to access each feature:

| Key | Feature | Function | Notes |
|-----|---------|----------|-------|
| `gd` | Go to definition | `+lookup/definition` | Requires URI colon fix (see below) |
| `gD` | Find references | `+lookup/references` | |
| `gI` | Find implementations | `eglot-find-implementation` | |
| `K` | Hover / docs | `+lookup/documentation` | Type info, XML docs |
| `SPC c a` | Code actions | `eglot-code-actions` | Refactors, quick fixes |
| `SPC c r` | Rename symbol | `eglot-rename` | Project-wide rename |
| `SPC c j` | Workspace symbols | `consult-eglot-symbols` | Fuzzy search all symbols |
| `SPC c f` | Format buffer | `+format/region-or-buffer` | |
| `SPC c T` | Find type definition | `eglot-find-typeDefinition` | Jump to type of a variable |
| `SPC c h` | Toggle inlay hints | `eglot-inlay-hints-mode` | Type hints on `var`, parameter names |
| `SPC c o` | Organize imports | `eglot-code-action-organize-imports` | Sort/remove usings |
| `SPC c q` | Quick fix | `eglot-code-action-quickfix` | Apply first available fix |
| Auto | Completion | corfu | Powered by Roslyn |
| Auto | Signature help | eldoc | Parameter info on function calls |
| Auto | Document highlight | automatic | Highlights symbol under cursor |
| Auto | Semantic tokens | automatic | Enhanced syntax highlighting |

**Windows URI fix:** eglot encodes `:` as `%3A` in file URIs (`file:///c%3A/...`), which causes Roslyn's `CreateAbsoluteUri` to throw `UriFormatException`. Fixed by `(aset eglot--uri-path-allowed-chars ?: t)` in config.el — allows colons in URI paths so drive letters are encoded correctly as `file:///c:/...`.

**Project discovery fix (2026-02):** Two issues prevented Roslyn from loading the real `.sln`/`.csproj`:

1. **File-based programs:** Roslyn v5 on .NET 10 SDK enables "file-based programs" by default. When eglot returns `null` for `projects.dotnet_enable_file_based_programs` in `workspace/configuration`, Roslyn defaults to `true` and creates synthetic `.csproj` files under `dotnet/runfile/` temp paths instead of loading the real project. Fixed with `eglot-workspace-configuration` (`setq-default` required — the variable is `defvar-local`, so plain `setq` only sets it in the buffer current at load time, not in eglot's temp buffer that handles `workspace/configuration` requests).

2. **Solution discovery:** The standalone Roslyn language server does NOT auto-discover `.sln` files — unlike VS Code's C# extension, which searches the workspace and sends solution paths to Roslyn. Without explicit notification, Roslyn sits idle with no project loaded. Fixed by sending `solution/open` notification (same protocol as [roslyn.nvim](https://github.com/seblyng/roslyn.nvim)) via `eglot-managed-mode-hook`. Also set `dotnet_enable_automatic_restore: true` so NuGet packages resolve automatically.

#### History

Previously used `csharp-ls` (razzmatazz/csharp-language-server) which was chosen for being lighter weight and actively maintained. Upgraded to Microsoft's Roslyn LSP for better feature parity with VS Code and first-party support.

**Roslyn v5 transport journey (2026-02):** Roslyn v5.0.0 switched default transport from stdio to named pipes. We first built a C# TCP-to-named-pipe bridge (`roslyn-bridge/`) with eglot `:autoport`, but TCP on Windows had IPv6 port-probe issues and unreliable data flow. Discovered that Roslyn v5.0.0 already supports `--stdio` (requested by Neovim's roslyn.nvim maintainer). Switched to direct stdio — eliminated ~200 lines of bridge code and all TCP/IPv6 workarounds.

**Sources:**
- [NuGet: Microsoft.CodeAnalysis.LanguageServer.win-x64](https://www.nuget.org/packages/Microsoft.CodeAnalysis.LanguageServer.win-x64)
- [csharp-ls on GitHub](https://github.com/razzmatazz/csharp-language-server) (previous choice)

### 3. C++ Language Server: clangd

**Choice:** `clangd` (part of LLVM)

**Rationale:**
- Official LLVM project, guaranteed maintenance
- Already installed via setup-doom.ps1 (LLVM package)
- Fast, well-integrated with eglot
- Alternative (ccls) is community-maintained

### 4. Code Navigation: Native xref + Universal CTags

**Choice:** Built-in `xref` with Universal CTags (no packages)

**Rationale:**
- **Zero dependencies** - xref is built into Emacs since v25
- **40+ years battle-tested** - TAGS format is the Emacs standard since the 1980s
- `M-.` (jump to definition), `M-,` (go back), `M-?` (find references) work natively
- Single TAGS file at project root covers all languages
- ctags included with LLVM (already installed)

#### Journey & Alternatives Considered

**Attempt 1: Citre + Universal Ctags (rejected)**
- Citre is a modern ctags frontend from the Universal Ctags organization
- Provides `citre-jump`, fuzzy symbol search, xref integration
- **Problem:** Added package complexity for features we didn't need
- xref handles TAGS natively - Citre was redundant
- Decided to try something even simpler

**Attempt 2: ggtags + GNU Global (rejected)**
- GNU Global is a mature source code tagging system
- ggtags provides Emacs frontend with good xref integration
- **Problems on Windows:**
  - Requires Chocolatey for GNU Global installation
  - Requires Pygments Python package for multi-language support
  - Needs GTAGSLABEL environment variable set to "native-pygments"
  - More moving parts = more maintenance burden
- GNU Global less actively maintained than Universal Ctags
- Decided external dependencies weren't worth it

**Attempt 3: Native xref + Universal Ctags (accepted)**
- Zero packages - just Emacs built-ins + ctags
- ctags already installed (comes with LLVM)
- Required some Doom-specific configuration (see below)

#### Doom Emacs Integration Challenges

Simply using `visit-tags-table` + vanilla xref doesn't work in Doom because:

1. **Doom's `gd` binding uses `+lookup/definition`** - This has its own backend list that doesn't include etags by default. Even with TAGS loaded, `gd` reports "No Xref backend available".

2. **etags backend not in xref-backend-functions** - Need to explicitly add `etags--xref-backend` to `xref-backend-functions`.

3. **TAGS file not auto-loaded** - Emacs doesn't automatically find/load TAGS files. Need hooks to visit TAGS on file open.

#### Windows Path Gotcha

ctags fails silently with absolute paths on Windows:
```bash
# FAILS - produces 0-byte TAGS file
ctags -e -R -f "c:/project/TAGS" "c:/project/"

# WORKS - use relative paths with default-directory
cd c:/project && ctags -e -R -f TAGS .
```

In Elisp, set `default-directory` and use relative paths:
```elisp
(let ((default-directory root))
  (compile "ctags -e -R -f TAGS ."))
```

#### Final Implementation (config.el)

```elisp
;; Generate TAGS at project root (Windows-safe)
(defun my/create-tags ()
  (interactive)
  (let* ((root (or (projectile-project-root) default-directory))
         (default-directory root)
         (tags-file (expand-file-name "TAGS" root)))
    (compile "ctags -e -R -f TAGS .")  ; relative paths work on Windows
    (add-hook 'compilation-finish-functions
              (lambda (_buf _status)
                (when (file-exists-p tags-file)
                  (visit-tags-table tags-file)))
              nil t)))

;; Auto-visit TAGS file when found in project
(defun my/visit-project-tags ()
  (when-let* ((root (projectile-project-root))
              (tags-file (expand-file-name "TAGS" root))
              ((file-exists-p tags-file)))
    (visit-tags-table tags-file t)))

(add-hook 'find-file-hook #'my/visit-project-tags)

;; Add etags to xref backends
(after! xref
  (add-to-list 'xref-backend-functions #'etags--xref-backend t))

;; Bind gd to Doom's +lookup (eglot -> etags -> other backends)
(after! evil
  (map! :map prog-mode-map
        :n "gd" #'+lookup/definition    ; eglot -> etags -> other backends
        :n "gD" #'xref-find-definitions)) ; direct xref (always uses etags)
```

#### Keybindings

| Key | Action | Function |
|-----|--------|----------|
| `gd` | Jump to definition (eglot -> etags) | `+lookup/definition` |
| `gD` | Direct xref (etags only) | `xref-find-definitions` |
| `M-.` | Jump to definition | `xref-find-definitions` |
| `M-,` | Go back | `xref-go-back` |
| `M-?` | Find references | `xref-find-references` |
| `SPC c t` | Create/update TAGS | `my/create-tags` |

### 5. Eshell Packages

| Package | Maintainer | Last Activity | Rationale |
|---------|-----------|---------------|-----------|
| eshell-syntax-highlighting | akreisher | Active | Fish-style highlighting, widely used |
| eshell-git-prompt | xuchunyang | Stable | Git branch in prompt, simple |

**Note:** Avoided eshell-prompt-extras (less active) in favor of eshell-git-prompt.

### 6. PowerShell: powershell.el

**Choice:** `powershell.el` (jschaf/powershell.el)

**Rationale:**
- Dedicated major mode for PowerShell
- Syntax highlighting, execution support
- Can integrate with pwsh-ls for LSP if desired

## Consequences

- Lighter setup (eglot vs lsp-mode)
- C# uses Microsoft's Roslyn LSP (installed via `scripts\install-roslyn-lsp.ps1`)
- All choices favor GNU/official project maintenance over feature-rich community alternatives
- Code navigation: eglot provides LSP-based go-to-definition; ctags (`SPC c t`) remains as fallback

## Tool Installation Commands

```powershell
# Universal CTags - comes with LLVM (already installed via setup script)
# Or install standalone: winget install UniversalCtags.Ctags

# C++ clangd - comes with LLVM (already installed via setup script)

# C# Roslyn LSP (same server as VS Code)
.\scripts\install-roslyn-lsp.ps1

# YAML language server
npm install -g yaml-language-server
```
