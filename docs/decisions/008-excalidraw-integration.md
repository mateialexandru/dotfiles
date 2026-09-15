# ADR-008: Excalidraw Integration in Doom Emacs

**Status:** Accepted (Revision 2026-07-25) — was Proposed/deferred 2026-04-22
**Date:** 2026-04-22

## Context

Mermaid + PlantUML wired. Text-DSL diagrams covered. Excalidraw adds freeform/hand-drawn canvas. Worth integration cost?

## Options

1. **`wdavew/org-excalidraw`** (161★) — canonical. Custom `excalidraw:` org link. PWA edits JSON on disk. Filewatcher runs `excalidraw_export` → SVG. Org inline-image displays SVG.
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
  → fswatch → excalidraw_export → UUID.excalidraw.svg
  → org inline image
```

## Deps

| Dep | Purpose | Risk |
|-----|---------|------|
| `excalidraw_export` (Timmmm) | JSON → SVG | node-canvas native build fails without cairo/pango |
| `canvas` npm | Exporter backend | Breaks on Node major upgrade |
| Virgil + Cascadia TTFs | SVG text render | Missing = garbled glyphs |
| Chrome/Chromium PWA | Editor | Hard req — Firefox/Safari lack File Handling API |
| `chrome://flags` File Handling API | Route `.excalidraw` to PWA | Can reset on Chrome update |
| `fswatch` / `inotify-tools` | Filewatcher | Negligible idle cost |

## Decision Drivers

- Setup: 6 deps, 3 failure-prone (node-gyp, Chrome flag, fonts).
- Chromium lock-in.
- Value ceiling: only freeform beats Mermaid/PlantUML. Boxes-and-arrows → text DSL wins (diff, VC, setup).

## Decision

**Adopting option 1 (`wdavew/org-excalidraw`).** (Reverses the original defer.)

Freeform/hand-drawn need confirmed; Chrome installed as a dedicated editor app
(not the daily browser — Safari stays primary, ADR-010). The setup is scripted +
idempotent (`scripts/install-excalidraw-mac.sh`, run from `install.sh` on macOS):
fswatch, the exporter (`@swiftlysingh/excalidraw-cli`), and the drawings dir. Only
the GUI steps stay manual (Chrome PWA install + `.excalidraw` handler), printed by
the script.

Wiring lives in `config/doom/packages.el` (`package!`) + `config/doom/config.el` (`use-package!`).
`org-excalidraw-initialize` is guarded on the deps + dir existing so a partial
install never aborts Doom boot (cf. the libgccjit boot-abort class, ADR-009).

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
side redefines `org-excalidraw--shell-cmd-to-svg` (same signature) to call it;
the fswatch handler is unchanged.

Considered `4honor/org-excalidraw` (kroki-cli) — drops node-gyp but adds a kroki
server (public kroki.io leaks diagram JSON unless self-hosted). Not needed now.

Revisit if:
- Safari gains the File Handling API → drop Chrome, edit in Safari PWA.
- excalidraw-cli's headless `exportToSvg` regresses → the faithful fallback is a
  real-browser renderer (Playwright + self-hosted Docker excalidraw).

## Applied Recipe

### 1. Prereqs — `scripts/install-excalidraw-mac.sh`

Scripted + idempotent, run from `install.sh` on macOS: `brew install fswatch`,
`npm install -g @swiftlysingh/excalidraw-cli` (Node ≥ 20.19; no native build), and
`mkdir` the drawings dir. It also cleans up any prior `excalidraw_export` install
(the `~/.local/share/excalidraw-export` node-canvas host, its PATH symlink, the
Virgil/Cascadia fonts) so a stale binary can't win on PATH.

### 2. Chrome (GUI, one-time — printed by the script)

- Install Chrome (dedicated editor app, not the daily browser — Safari stays primary).
- excalidraw.com → install as PWA. File Handling API is default-on since Chrome 102
  (no `chrome://flags`); grant the `.excalidraw` prompt (or chrome://apps → App info).
- Finder: `.excalidraw` → Get Info → Open with Excalidraw.app → Change All.
- Verify: `open file.excalidraw` launches the PWA with the file loaded.

### 3. Doom — `config/doom/packages.el`

```elisp
(package! org-excalidraw
  :recipe (:host github :repo "wdavew/org-excalidraw"))
```

### 4. Doom — `config/doom/config.el`

`org-excalidraw-initialize` (the filewatcher) is guarded on the deps + dir existing,
so a partial/missing install never aborts Doom boot (cf. libgccjit boot-abort, ADR-009):

```elisp
(use-package! org-excalidraw
  :after org
  :commands (org-excalidraw-create-drawing)
  :config
  (setq org-excalidraw-directory "~/Documents/org/excalidraw")
  ;; Faithful exporter: real Excalidraw renderer, not excalidraw_export (garbles
  ;; bound/multi-line text — see Decision). Same signature; fswatch handler uses it.
  (defun org-excalidraw--shell-cmd-to-svg (path)
    (format "excalidraw-cli convert %s --format svg --output %s"
            (shell-quote-argument path)
            (shell-quote-argument (concat path ".svg"))))
  (when (and (file-directory-p org-excalidraw-directory)
             (executable-find "excalidraw-cli")
             (executable-find "fswatch"))
    (org-excalidraw-initialize)
    (when (fboundp 'org-link-preview-file)
      (org-link-set-parameters "excalidraw" :preview #'my/org-excalidraw-preview))))
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
preview (or use `SPC m D o`) → PWA; save there → fswatch regenerates the SVG.
Toggle/refresh inline images with `C-c C-x C-v` (they're on at startup via
`org-startup-with-inline-images`).

## Related

- ADR-001 — Emacs package rationale.
- `config/doom/packages.el` — Mermaid + `ob-mermaid` wiring.
- `docs/examples/excalidraw-org-example.org` — hands-on create/edit/rename worksheet.
