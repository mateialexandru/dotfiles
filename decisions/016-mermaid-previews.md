# ADR-016: Natural Mermaid previews in Org and Markdown

## Context

The repository already installed Mermaid CLI, `mermaid-mode`, and `ob-mermaid`,
but the experience stopped at syntax highlighting and manual Babel execution.
Markdown's Doom `+grip` preview syntax-highlights Mermaid fences but does not
render them as diagrams. The long-lived daemon also exposed two correctness
issues: Mermaid disappeared from `org-babel-load-languages` after startup, and
Mermaid's default SVG labels use `foreignObject`, which Emacs does not render
reliably.

## Decision

- Keep `ob-mermaid` as the canonical Org integration and use SVG file results.
- Use a shared Mermaid config with HTML labels disabled so SVG text is native
  and visible in Emacs.
- Repair Mermaid's Babel registration from `org-mode-hook` as well as initial
  package setup, making it resilient to later language-list resets.
- Refresh Org inline images after Babel execution, but never execute Mermaid
  blocks merely because a document was opened.
- Keep Doom's mature `markdown-mode` and add pinned `md-mermaid` solely for
  asynchronous, same-buffer live overlays.
- Store live PNGs under `doom-cache-dir`, leave source visible, scan the buffer,
  and serialize `mmdc` jobs. Buffer scope is intentional: the package's visible
  scope calls `window-end` from `markdown-mode-hook`, which can re-enter NS
  redisplay and abort an Emacs 30 daemon while a client frame is being created.
  Its existing line and fence limits keep buffer-wide work bounded.
- Do not auto-enable live rendering for TRAMP buffers.
- Derive live-preview background and Mermaid theme from the active Emacs frame,
  and restart active previews after a Doom theme change.
- Keep source visible by default for unsurprising editing and diagnostics, with
  `SPC m v` as an explicit pristine-view toggle in both Org and Markdown. The
  Org toggle folds Mermaid blocks only.
- Fit previews to 90% of the narrowest window displaying the buffer, center
  them, and refit after window-state changes. Org uses its native
  `org-image-max-width` and `org-image-align`; Markdown rescales only the display
  spec, retaining the full-resolution cached PNG.

## Consequences

Org documents keep portable Mermaid source plus explicit image results.
Markdown documents remain ordinary GitHub-compatible fenced source while Emacs
adds non-persistent preview overlays. Opening a Markdown file can start local
Python and headless Chromium processes through `mmdc`, but it cannot dirty the
project and does not happen for remote files. The new Elisp package is pinned
because it is young and its API is not yet release-stable.

Changes to `config.el` can be evaluated in a running client. Installing or
updating `md-mermaid` requires `doom sync` and a full daemon restart.
