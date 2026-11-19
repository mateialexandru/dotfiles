---
name: emacs
description: Open files, source locations, Magit views, and patches in the existing local Emacs daemon.
argument-hint: "<file[:line], repository, diff, or patch>"
---

# Open in the existing Emacs session

Resolve the public checkout from `DOOM_DOTFILES_DIR` in the process environment,
falling back to its current-user environment value. If unset, report that the
public dotfiles installer or `scripts\install-skills.ps1` must be run.
Use `<dotfiles>\scripts\open-in-emacs.ps1`; do not assume a clone location.

```powershell
$dotfiles = $env:DOOM_DOTFILES_DIR
if (-not $dotfiles) {
    $dotfiles = [Environment]::GetEnvironmentVariable('DOOM_DOTFILES_DIR', 'User')
}
if (-not $dotfiles) { throw 'Run the dotfiles skill installer first.' }
$open = Join-Path $dotfiles 'scripts\open-in-emacs.ps1'
```

Resolve files in the requested checkout, not another clone. Pass absolute
Windows paths as arguments, never interpolate paths or user text into Lisp.
For `file:line`, distinguish the drive colon from the final line number.

```powershell
& $open -Path 'C:\Source\project\src\main.cpp' -Line 42 -Column 1
& $open -Path 'C:\Source\project' -View Status
& $open -Path 'C:\Source\project' -View Unstaged
& $open -Path 'C:\Source\project' -View Staged
& $open -Path 'C:\Source\project' -View Range -Base HEAD~1 -Target HEAD
& $open -Path 'C:\Source\changes.patch' -View Patch
```

Use Status for an unspecified request to show repository changes: it includes
staged, unstaged and untracked files. Range is a two-endpoint diff, not an
automatic merge-base calculation. Do not fetch or change Git state just to
open a view. In Magit, TAB expands sections and RET visits files/hunks.

The helper reuses the existing daemon and focuses a graphical frame, creating
only a client frame if needed. It never starts or restarts a daemon, closes
buffers, saves changes, or stops terminal/agent processes. Display-only requests
do not authorize applying patches, staging, committing, or discarding edits.
Do not reload all configuration. Validate against the existing daemon.

Check the exit status and returned buffer/view/daemon PID before claiming it
opened. If the daemon or local desktop is unavailable, report that limitation;
do not launch a replacement or claim that a remote session opened a local view.
