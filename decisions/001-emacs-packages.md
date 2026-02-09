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

### 2. C# Language Server: csharp-ls (not omnisharp)

**Choice:** `csharp-ls` (razzmatazz/csharp-language-server)

**Rationale:**
- **omnisharp-roslyn is declining:** VSCode dropped it for Microsoft's built-in Roslyn server. Limited community resources, months-long contribution gaps reported.
- **csharp-ls is actively maintained:** v0.21.0 released January 2026, uses Roslyn under the hood
- Lighter weight than omnisharp
- Works with older .NET SDKs (Core 3, Framework 4.8)
- Microsoft's own Roslyn LSP (csharp-roslyn) is another option but less tested with eglot

**Installation:** `dotnet tool install --global csharp-ls`

**Sources:**
- [OmniSharp future concerns](https://github.com/OmniSharp/omnisharp-roslyn/issues/2663)
- [csharp-ls on GitHub](https://github.com/razzmatazz/csharp-language-server)
- [csharp-ls on NuGet](https://www.nuget.org/packages/csharp-ls/)

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

;; Rebind gd to xref (bypassing Doom's +lookup)
(after! evil
  (map! :map prog-mode-map
        :n "gd" #'xref-find-definitions
        :n "gD" #'+lookup/definition))
```

#### Keybindings

| Key | Action | Function |
|-----|--------|----------|
| `gd` | Jump to definition | `xref-find-definitions` |
| `gD` | Doom lookup (fallback) | `+lookup/definition` |
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
- C# requires manual csharp-ls installation
- All choices favor GNU/official project maintenance over feature-rich community alternatives
- Code navigation requires one-time `SPC c t` at monorepo root to generate tags

## Tool Installation Commands

```powershell
# Universal CTags - comes with LLVM (already installed via setup script)
# Or install standalone: winget install UniversalCtags.Ctags

# C++ clangd - comes with LLVM (already installed via setup script)

# C# csharp-ls (actively maintained Roslyn-based server)
dotnet tool install --global csharp-ls

# YAML language server
npm install -g yaml-language-server
```
