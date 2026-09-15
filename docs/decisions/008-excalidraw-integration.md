# ADR-008: Excalidraw Integration in Doom Emacs

**Status:** Accepted (Revision 2026-09-15) — was Proposed/deferred 2026-04-22
**Date:** 2026-04-22

## Context

Mermaid + PlantUML wired. Text-DSL diagrams covered. Excalidraw adds freeform/hand-drawn canvas. Worth integration cost?

## Options

1. **`wdavew/org-excalidraw`** (161★) — canonical. Custom `excalidraw:` Org link. PWA edits JSON on disk. Emacs file notifications run the exporter → SVG. Org displays the SVG inline.
2. **`4honor/org-excalidraw`** (17★) — fork, less traction. Skip.
3. **On-demand, no watcher** — `after-save-hook` scoped to `.excalidraw`, or manual command.
4. **Self-host Excalidraw** (Docker) — offline, but PWA File Handling needs HTTPS + manifest.
5. **No integration** — edit on excalidraw.com, export manually, link as image.

## Flow (option 1)

```
M-x org-excalidraw-create-drawing
  → UUID.excalidraw JSON + [[excalidraw:path]] link
click link → Chrome PWA opens file (File Handling API)
save in PWA → JSON rewritten
  → Emacs file notification → excalidraw-cli → UUID.excalidraw.svg
  → org inline image
```

## Deps

| Dep | Purpose | Risk |
|-----|---------|------|
| `@swiftlysingh/excalidraw-cli` | JSON → SVG | Requires Node ≥ 20.19 |
| Chrome/Chromium PWA | Editor | Hard req — Firefox/Safari lack File Handling API |
| OS file association | Route `.excalidraw` to PWA | One-time GUI setup per platform |
| Emacs file notifications | Cross-platform save watcher | Backend/event shapes vary by OS |

## Decision Drivers

- Setup: one CLI dependency plus a one-time Chrome PWA/file-association step.
- Chromium lock-in.
- Value ceiling: only freeform beats Mermaid/PlantUML. Boxes-and-arrows → text DSL wins (diff, VC, setup).

## Decision

**Adopting option 1 (`wdavew/org-excalidraw`).** (Reverses the original defer.)

Freeform/hand-drawn need confirmed; Chrome installed as a dedicated editor app
(not the daily browser — Safari stays primary, ADR-010). The setup is scripted +
idempotent: `scripts/install-excalidraw-mac.sh` installs the exporter and drawings
directory on macOS; `scripts/install-prerequisites.ps1` does the same on Windows.
Only the GUI steps stay manual (Chrome PWA install + `.excalidraw` file association).

Wiring lives in `config/doom/packages.el` (`package!`) + `config/doom/config.el` (`use-package!`).
The shared config owns the `file-notify-add-watch` registration. It is guarded on
the exporter and directory, handles both atomic `renamed` saves and Windows-style
`changed` saves, and reports watcher failure without aborting Doom boot.

### Exporter: dropped `excalidraw_export`, adopted `@swiftlysingh/excalidraw-cli`

`wdavew/org-excalidraw` shells out to Timmmm's `excalidraw_export`. Two problems,
both now retired:

1. **Fidelity (the deciding one).** `excalidraw_export` reimplements Excalidraw's
   renderer and **garbles container-bound, multi-line text**. Concrete: the
   `design-yelp-mock-exponent` diagram has 16 bound texts like
   `"Places backend\nService"`; they rendered stacked/overlapping
   (`Placesservicekend`, `Business hpland ata`). Standalone labels were fine.
   Confirmed *not* an Emacs/librsvg issue — librsvg and resvg rasterise the
   produced SVG identically; the SVG is wrong at source.
2. **Build.** It pins `canvas@^2`, which won't compile on modern Node — canvas 2.x
   calls `v8::Context::GetIsolate`, removed from V8 (Node 26 → `error: no member
   named 'GetIsolate'`). We had worked around this with a private npm dir forcing
   `canvas@^3` (prebuilt) + a font download — all removed now.

`@swiftlysingh/excalidraw-cli` uses the **real** `@excalidraw/utils`
`exportToSvg()` + bundled fonts → faithful bound/multi-line layout (verified on the
same yelp diagram) and rounded corners. No browser, no node-canvas, brew/npm
install, offline. So the whole cairo/pango/canvas/fonts stack is gone. The Doom
side redefines `org-excalidraw--shell-cmd-to-svg` (same signature) to call it and
supplies its own portable, debounced file-notification handler.

Considered `4honor/org-excalidraw` (kroki-cli) — drops node-gyp but adds a kroki
server (public kroki.io leaks diagram JSON unless self-hosted). Not needed now.

Revisit if:
- Safari gains the File Handling API → drop Chrome, edit in Safari PWA.
- excalidraw-cli's headless `exportToSvg` regresses → the faithful fallback is a
  real-browser renderer (Playwright + self-hosted Docker excalidraw).

## Applied Recipe

### 1. Prerequisites

Both platforms install `@swiftlysingh/excalidraw-cli` with npm (Node ≥ 20.19; no
native build) and create `~/Documents/org/excalidraw/`. macOS does this through
`scripts/install-excalidraw-mac.sh`; Windows uses
`scripts/install-prerequisites.ps1`. The macOS script also cleans up any prior
`excalidraw_export` install
(the `~/.local/share/excalidraw-export` node-canvas host, its PATH symlink, the
Virgil/Cascadia fonts) so a stale binary can't win on PATH.

### 2. Chrome (GUI, one-time)

- Install Chrome (dedicated editor app, not the daily browser — Safari stays primary).
- excalidraw.com → install as PWA. File Handling API is default-on since Chrome 102
  (no `chrome://flags`); grant the `.excalidraw` prompt (or chrome://apps → App info).
- Associate `.excalidraw` with the installed Excalidraw PWA: Finder → Get Info →
  Open with on macOS, or Open with → Choose another app on Windows.
- Verify that opening a `.excalidraw` file launches the PWA with the file loaded.

### 3. Doom — `config/doom/packages.el`

```elisp
(package! org-excalidraw
  :recipe (:host github :repo "wdavew/org-excalidraw"))
```

### 4. Doom — `config/doom/config.el`

The shared configuration registers a guarded Emacs-native watcher. It debounces
save events and accepts `changed`, `created`, and `renamed` actions, then refreshes
Org previews after a successful export. Opening uses `open` on macOS,
`w32-shell-execute` on Windows, and `xdg-open` elsewhere. No external watcher is
required:

```elisp
(use-package! org-excalidraw
  :after org
  :commands (org-excalidraw-create-drawing)
  :config
  (setq org-excalidraw-directory "~/Documents/org/excalidraw")
  ;; Faithful exporter: real Excalidraw renderer, not excalidraw_export (garbles
  ;; bound/multi-line text — see Decision). Same signature; our watcher uses it.
  (defun org-excalidraw--shell-cmd-to-svg (path)
    (format "excalidraw-cli convert %s --format svg --output %s"
            (shell-quote-argument path)
            (shell-quote-argument (concat path ".svg"))))
  (my/org-excalidraw--start-watcher)
  (org-link-set-parameters
   "excalidraw"
   :follow #'my/org-excalidraw-follow
   :preview (and (fboundp 'org-link-preview-file)
                 #'my/org-excalidraw-preview)))
```

**Inline-preview gotcha (org 9.7+):** the package registers the link's inline
image via the old `:image-data-fun' link param, which org 9.7 dropped for the new
`:preview' API — on org 9.8 the thumbnail silently doesn't render (0 overlays).
Re-register a `:preview' fn; the excalidraw: link path is the exported `.svg`, so
`my/org-excalidraw-preview' delegates rendering to Org's built-in
`org-link-preview-file' and adds a `mouse-1` open action. Also needs inline images
on: `(setq org-startup-with-inline-images t)` (or `#+STARTUP: inlineimages`).

`~/.config/emacs/bin/doom sync` → restart.

### 5. Skip watcher variant

```elisp
(add-hook 'after-save-hook
  (lambda ()
    (when (and buffer-file-name
               (string-match-p "\\.excalidraw\\'" buffer-file-name))
      (call-process "excalidraw-cli" nil 0 nil "convert" buffer-file-name
                    "--format" "svg" "--output" (concat buffer-file-name ".svg")))))
```

`nil 0` = async.

### 6. Usage

At the beginning of an Org line, `<excali TAB` prompts for a drawing name, creates
the backing file, inserts its `excalidraw:` preview link, and opens the PWA.
This is the fast creation path and parallels the `<merm TAB` YASnippet, which
inserts a named, scaffolded Mermaid block.

Named-file wrapper + rename live on the Org localleader under `SPC m D`
(`D` is diagram; lowercase `d` is Doom's date/deadline menu), backed by
`my/org-excalidraw-*` in `config.el` (upstream only offers UUID filenames):

| Key | Action |
|-----|--------|
| `<excali TAB` | Prompt, create, insert, and open a named drawing |
| `SPC m D n` | Diagram → new — prompts, slugifies → `my-diagram.excalidraw`; blank uses UUID |
| `SPC m D o` | Diagram → open at point in the Chrome PWA |
| `SPC m D r` | Diagram → rename at point — renames `.excalidraw` + `.svg`, rewrites link |

Blank name → UUID fallback; slug collisions auto-suffix `-1`, `-2`. Left-click a
preview (or use `SPC m D o`) → PWA; save there → Emacs regenerates the SVG and
redisplays matching Org previews. Toggle/refresh inline images manually with
`C-c C-x C-v` if needed (they're on at startup via
`org-startup-with-inline-images`).

## Related

- ADR-001 — Emacs package rationale.
- `config/doom/packages.el` — Mermaid + `ob-mermaid` wiring.
- `docs/howto/howto-excalidraw.org` — hands-on create/edit/rename worksheet.
