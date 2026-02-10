# Decision: .NET Development with devcontainer.el Integration

## Context

We want to develop .NET/C# projects in Doom Emacs using devcontainers, with all tooling (LSP, formatting, compilation) running inside containers transparently.

## Decision

Use devcontainer.el's API (`devcontainer-advise-command`) to wrap LSP and formatting commands, ensuring all .NET tooling runs inside the container without host dependencies.

## Rationale

### Why devcontainer.el API over Raw Docker Commands

**Rejected Approach**: Custom wrapper scripts with hardcoded `docker exec` commands
- **Problem**: Brittle - breaks if container names change
- **Problem**: Doesn't integrate with devcontainer.el's lifecycle management
- **Problem**: Requires manual container detection logic
- **Problem**: Doesn't respect devcontainer.el's exclusion rules

**Chosen Approach**: Use `devcontainer-advise-command` from devcontainer.el
- ✅ **Automatic container detection** - devcontainer.el tracks running containers
- ✅ **Lifecycle integration** - respects devcontainer-mode activation/deactivation
- ✅ **Smart wrapping** - only wraps when `devcontainer-advisable-p` returns true
- ✅ **Consistent behavior** - same wrapping mechanism as compilation commands
- ✅ **Maintainable** - single source of truth for container execution

### Why csharp-ls over omnisharp-roslyn

**Chosen**: csharp-ls (modern Roslyn-based LSP server)
- Latest release: v0.22.0 (Feb 5, 2026)
- Pure LSP implementation (better architecture)
- .NET 10+ support with legacy compatibility
- Active development (41 releases)
- Lighter weight than omnisharp

**Rejected**: omnisharp-roslyn
- Still maintained but older architecture
- More complex setup
- Heavier resource usage
- VS Code moved away from it

### Integration Points

**Apheleia (Formatting)**:
```elisp
(after! devcontainer
  (after! apheleia
    (add-hook 'apheleia-mode-hook #'+format-devcontainer-wrap-h)))
```
- Wraps formatter commands with `devcontainer-advise-command`
- Applied per-buffer when apheleia-mode starts
- Only activates when `devcontainer-advisable-p` is true

**Eglot (LSP)**:
```elisp
(after! devcontainer
  (after! eglot
    (add-hook 'eglot-managed-mode-hook #'+lsp-devcontainer-wrap-h)))
```
- Modifies `eglot-server-programs` to use wrapped commands
- Applied per-buffer before LSP server starts
- Only activates when `devcontainer-advisable-p` is true

**Installation (devcontainer.json)**:
```json
{
  "postCreateCommand": "dotnet tool install --global csharp-ls",
  "remoteEnv": {
    "PATH": "${containerEnv:PATH}:${containerEnv:HOME}/.dotnet/tools"
  }
}
```
- Automatic installation when container is created
- No manual steps required
- PATH configuration ensures tools are accessible

## Consequences

**Positive**:
- Zero host dependencies (.NET SDK not needed on host)
- Consistent behavior across dev environments
- Automatic container integration
- Clean separation of concerns
- Easy to debug (standard devcontainer.el logging)

**Negative**:
- Slightly more complex initial setup (hooks + config)
- Requires devcontainer.el understanding
- First-time container build takes longer (installs csharp-ls)

## Alternatives Considered

1. **Apheleia remote algorithm** - Doesn't integrate with devcontainer.el lifecycle
2. **Global make-process advice** - Too broad, would affect all Emacs processes
3. **Wrapper scripts** - Brittle, requires manual maintenance
4. **Host LSP with container file mounting** - Slow, LSP struggles with path mapping

## Implementation Date

2026-02-08

## Status

**SUPERSEDED** - See "2026-02-08 Update: TRAMP-Based Approach" below for the current implementation

## Template-Based Setup

The devcontainer setup command uses a template-based approach:

**Template Location**: `~/.config/doom/devcontainer/dotnet/devcontainer.json`

**Benefits**:
- Easy to maintain and update
- Can be versioned in dotfiles repository
- Simple to add templates for other project types (e.g., `~/config/doom/devcontainer/python/`, etc.)
- No manual string concatenation in elisp code

**Usage**:
```elisp
(my/setup-dotnet-devcontainer)  ; Copies template to project's .devcontainer/
```

**Adding More Templates**:
Create new templates in `~/.config/doom/devcontainer/<type>/devcontainer.json` and add corresponding setup functions.

---

## 2026-02-08 Update: TRAMP-Based Approach

### Why We Switched from Custom Wrapping to TRAMP

**Problem with Previous Approach**:
- Custom docker exec wrapping (~70 lines of complex code)
- Path translation issues between host and container
- Fragile - required manual wrapper maintenance
- LSP and formatting didn't work reliably

**TRAMP Solution**:
- Uses Emacs' built-in remote file handling
- **Zero LSP configuration needed** - eglot "just works" with TRAMP
- **Automatic path translation** - TRAMP handles it natively
- **Native container completion** - `/docker:TAB` shows running containers
- Much simpler (~85 lines vs ~130 lines, net +15 lines but much cleaner)

### New Workflow

**Opening Files in Containers**:

1. **Quick access to current project's devcontainer**:
   ```
   SPC f d → opens file picker in /docker:container:/workspaces/cloudcity/
   ```

2. **Generic container access**:
   ```
   SPC f D → shows list of all running containers → pick one → navigate to file
   ```

3. **Native TRAMP completion**:
   ```
   C-x C-f /docker:TAB → shows running containers with names/IDs
   ```

4. **Bookmarks for frequent containers**:
   ```
   SPC b D → creates bookmark "devcontainer:cloudcity"
   SPC RET → type "devcontainer:cloudcity" → jump to container workspace
   ```

**LSP and Formatting (No Configuration Needed)**:

When you open a file via TRAMP (e.g., `/docker:abc123:/workspaces/cloudcity/Program.cs`):
- **Eglot** automatically finds `csharp-ls` in the container and starts it
- **Apheleia** automatically runs `dotnet csharpier` in the container
- **No custom wrapping** - TRAMP transparently executes commands in the container

### Implementation Details

**What Changed**:

1. **Removed** (~70 lines):
   - Custom apheleia devcontainer wrapper hook
   - Custom eglot devcontainer wrapper hook

2. **Added** (~85 lines):
   - TRAMP configuration for Docker containers
   - Helper functions: `my/docker-containers`, `my/find-file-in-docker`, `my/devcontainer-find-file`, `my/bookmark-devcontainer`
   - Simplified eglot config (just basic server program registration)
   - Simplified apheleia config (auto-enable for C# files)

3. **Kept** (unchanged):
   - Compilation path rewriting (still useful for test output)
   - Auto-enable devcontainer-mode detection
   - devcontainer.el integration for lifecycle management

**New Keybindings**:
- `SPC f d` - Find file in current project's devcontainer
- `SPC f D` - Find file in any Docker container (shows picker)
- `SPC b D` - Bookmark current devcontainer workspace

### How It Works

**Traditional Approach (broken)**:
```
1. Open local file: /var/home/.../cloudcity/Program.cs
2. Hook wraps eglot with: docker exec -w /workspaces/cloudcity container csharp-ls
3. LSP sends URIs: /workspaces/cloudcity/Program.cs
4. ❌ Path mismatch - Emacs can't map container paths to host paths
```

**TRAMP Approach (works)**:
```
1. Open TRAMP file: /docker:container:/workspaces/cloudcity/Program.cs
2. Eglot starts: csharp-ls (TRAMP runs it in container automatically)
3. LSP sends URIs: /workspaces/cloudcity/Program.cs
4. ✅ TRAMP translates paths transparently
5. goto-definition, formatting, etc. all work seamlessly
```

### Verification Steps

After reloading config (`doom sync` not needed for config.el changes):

1. **Check TRAMP completion**:
   ```
   C-x C-f /docker:TAB
   # Should show: List of running containers with IDs and names
   ```

2. **Test helper function**:
   ```
   SPC f d
   # Should prompt: "Find file in devcontainer: /docker:abc123:/workspaces/cloudcity/"
   ```

3. **Test LSP**:
   - Open file via `SPC f d` → navigate to `Program.cs`
   - Buffer name: `/docker:cloudcity:/workspaces/cloudcity/Program.cs`
   - Start eglot: `M-x eglot`
   - ✅ Should auto-detect csharp-ls and connect
   - Test: `gd` (goto-definition), `K` (hover docs)

4. **Test formatting**:
   - In the same TRAMP file, mess up formatting
   - Press `SPC m f`
   - ✅ Should auto-format with CSharpier

### Troubleshooting

**TRAMP completion doesn't show containers**:
- Check Emacs version: `M-x emacs-version` (need 29+)
- Test docker: `M-! docker ps`
- Check TRAMP verbosity: `M-: tramp-verbose` (set to 6 for debugging)

**Eglot can't find LSP server**:
- Verify in container: `docker exec <container> which csharp-ls`
- Check PATH: `docker exec <container> echo $PATH`
- Should include `/root/.dotnet/tools` or `~/.dotnet/tools`

**Formatting doesn't work**:
- Verify CSharpier: `docker exec <container> csharpier --version`
- Check apheleia-mode active: `M-: apheleia-mode` (should return `t`)
- Check `*Messages*` buffer for errors

### Benefits Over Previous Approach

**✅ Simpler**: Remove complex wrapping code, add simple helpers
**✅ Official**: Uses Emacs built-in TRAMP (no custom hacks)
**✅ Maintainable**: Leverages standard Emacs patterns
**✅ Zero LSP config**: Eglot "just works" with TRAMP
**✅ Better UX**: Helper functions + Vertico make container selection easy
**✅ Persistent**: Bookmarks provide quick access to frequent containers
**✅ Scalable**: Works for any container, not just devcontainer

### References

- [TRAMP User Manual - Container Support](https://www.gnu.org/software/emacs/manual/html_mono/tramp.html)
- [Emacs 29+ Native Docker Support](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/net/tramp-container.el)
- [Eglot + TRAMP Integration](https://joaotavora.github.io/eglot/)
- [Dev Containers Part 2: Emacs](https://www.happihacking.com/blog/posts/2023/dev-containers-emacs/)
- [Using bookmarks with docker-tramp](https://fuco1.github.io/2017-10-08-Using-bookmarks-with-eshell-and-docker-tramp.html)
