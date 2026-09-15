# Dotfiles

Cross-platform Doom Emacs configuration for macOS, Windows, and Linux (Bluefin/Fedora).

## Install

All installers are idempotent — safe to re-run.

### macOS / Linux

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/mateialexandru/dotfiles/main/install.sh)"
```

To install the editor/tooling baseline without Ollama, LM Studio, or the
optional local model set (about 32 GB), append `-- --skip-llm`.

Bootstraps prerequisites (Git, Curl, Unzip), clones the repo to `~/Source/dotfiles`, then installs Homebrew, Doom Emacs and its dependencies. On macOS, sets up `emacs-plus@30` as a daemon + `Emacs Client.app`. It also builds the `hack` worktree tool, appends a `Host *.ts.net` SSH ControlMaster block to `~/.ssh/config` so TRAMP and the Emacs remote-terminal workflow reconnect fast to tailnet hosts (see ADR-011), and links `config/git/ignore` to `~/.config/git/ignore`. On macOS it installs the optional local LLM layer unless `--skip-llm` is supplied.

### Windows

Prerequisite: Developer Mode enabled (for symlinks) or run as Administrator.

```powershell
git clone https://github.com/mateialexandru/dotfiles C:\avd\dotfiles
cd C:\avd\dotfiles
.\install.ps1
```

Run `scripts\doctor.ps1` afterwards to verify the environment.

## Repository worktrees

`hack` lazily indexes existing repositories beneath `~/Source` and creates isolated task
checkouts from the latest remote base branch:

```text
hack product/fix-login
hack repos --refresh
hack list
hack remove product/fix-login
```

Each spawn fetches first and starts a new task at `origin/develop` when that branch
exists, otherwise at the remote default branch. An existing remote task branch is
resumed. Hack only manages Git state; opening the worktree in Emacs or delegating work
happens separately. Use local `git config hack.baseBranch BRANCH` for a repository whose
base is not inferred correctly. A private layer can set `HACK_SOURCE_ROOTS` to add more
checkout roots without exposing their paths here. Normal commands read
`~/.cache/hack/repos`; an unknown name triggers one filesystem-only refresh, so Windows
does not pay for a directory walk or a series of Git processes on every invocation.

## Mermaid diagrams in Emacs

Mermaid is rendered locally with `mmdc`; diagram source is never sent to a
remote service.

- In Org, use a file result such as `#+begin_src mermaid :file diagram.svg`.
  `C-c C-c` renders it and refreshes the inline image. `SPC m M` does the same
  but refuses to run unless point is in a Mermaid block.
- In Markdown, fenced `mermaid` blocks receive Mermaid syntax highlighting and
  live inline previews. `SPC m m` toggles the live overlays and `SPC m M`
  renders the visible window once. Preview PNGs live in Doom's cache, so opening
  a README does not create project files.
- In either Org or Markdown, `SPC m v` toggles a pristine reading view. It hides
  Mermaid source while keeping the rendered diagram visible; press it again to
  return to editing. In Org, non-Mermaid source blocks are left alone.
- Inline previews use 90% of the narrowest window showing their buffer, preserve
  aspect ratio, and stay centered when frames or splits are resized.
- In standalone `.mmd` or `.mermaid` files, Mermaid mode compiles to SVG.

Automatic Markdown previews are disabled over TRAMP. Org blocks remain
explicitly executed rather than running merely because a document was opened.
See `docs/decisions/016-mermaid-previews.md` for the design and safety rationale.

## Capture into Emacs org (macOS)

Capture the current page into `~/Documents/org/inbox.org` from the macOS Share Menu, using the one-time **Scrim + Captee for Emacs** App Store bundle (`scripts/install-scrim-captee-mac.sh` opens the page). A private layer may override the destination. Both apps are notarized, one-time purchases — no developer account, no unsigned-extension toggle.

Pipeline: Share Menu → **Captee** → `org-protocol://` → **Scrim** → `emacsclient` → capture template `L` → `inbox.org`.

One-time setup:

1. Buy + install the bundle, then **launch Scrim once** (it relays to the Emacs server via the auth file `~/.config/emacs/server/server`, which lives at Emacs's default location so Scrim finds it automatically; if not, Scrim menu → **Setup** → select that file).
2. **Pin the `org-protocol://` scheme to Scrim:** `duti -s com.yummymelon.scrim org-protocol` (or re-run `scripts/install-scrim-captee-mac.sh`). emacs-plus's **Emacs Client.app also claims `org-protocol://`** — without this pin, LaunchServices may route captures to it, which fails under the TCP server and spawns a stray Emacs.
3. **Captee → Settings** → Format **Org**, Payload **Capture**, Use **Protocol**, capture template key **L**.
4. Capture: in Safari, **Share button → Captee → Share to Emacs** (optionally select text first — it rides along in the body). Bind a global hotkey via **System Settings → Keyboard → Shortcuts** if you want one-key capture.

Requires the Doom `+org-protocol` flag, capture template `L`, and the Emacs server on **TCP** (`server-use-tcp t`) so the sandboxed Scrim can connect — all configured in `config/doom/config.el` / `config/shell/init.zsh`. Open Emacs frames via **Emacs Client.app / `emacsclient`**, not a second full `Emacs.app`. See `docs/decisions/010-safari-org-capture.md` for the rationale.

## Private configuration layers

The public configuration is complete on its own and uses portable defaults.
Private repositories can extend it without being named by this repository. Add
ordered directory symlinks beneath `~/.config/dotfiles/layers.d/`; each layer
may provide `doom/pre.el`, `doom/post.el`, and `shell/init.zsh`. Early Doom
settings load before Org, while late commands load after the public config.
See `docs/examples/profile/` for the layer contract.

For a complete rebuild, install this public repository first, then clone each
private repository and run its profile activation script. Private layers are
intentionally not cloned here: authentication, repository names, credentials,
and machine-specific tools remain private. After activation, restart Emacs and
open a new shell. Restore credentials separately through Keychain,
`auth-source`, or environment variables.

## Wallpaper

To match the `retro-gnu-meditate-levitate` Emacs icon (Nevrax Design Team), use [`assets/gnu/meditate-fs.jpg`](assets/gnu/meditate-fs.jpg) as desktop wallpaper. It is the unmodified [GNU original](https://www.gnu.org/graphics/meditate-fs.jpg).

## Further reading

- `AGENTS.md` — architecture, key packages, scripts, and tooling.
- `docs/decisions/` — Architecture Decision Records (ADRs) documenting design rationale.
