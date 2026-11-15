# Dotfiles

Cross-platform Doom Emacs configuration for macOS, Windows, and Linux (Bluefin/Fedora).

## Install

Installers are intended to be rerunnable, but some steps can download packages,
rebuild tools, or request elevation. Review the platform-specific behavior below.

### macOS / Linux

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/mateialexandru/dotfiles/main/install.sh)"
```

To install the editor/tooling baseline without LM Studio or the
optional local model set, append `-- --skip-llm`.

Bootstraps prerequisites (Git, Curl, Unzip), clones the repo to `~/Source/dotfiles`, then installs Homebrew, Doom Emacs and its dependencies. Doom's official Ghostel module provides modern terminal emulation through `libghostty-vt`; macOS installation also provisions its pinned native module automatically. On macOS, the installer sets up `emacs-plus@30` as a daemon + `Emacs Client.app`. It also builds the `hack` worktree tool and the cross-platform `sys` operations CLI, appends a `Host *.ts.net` SSH ControlMaster block to `~/.ssh/config` so TRAMP and the Emacs remote-terminal workflow reconnect fast to tailnet hosts (see ADR-011), and links `config/git/ignore` to `~/.config/git/ignore`. On macOS it installs the optional local LLM layer unless `--skip-llm` is supplied.

### Windows

Prerequisites: PowerShell 7.3+ (`winget install Microsoft.PowerShell`) and
Developer Mode enabled (for symlinks) or run as Administrator. Run the commands
below from `pwsh`, not Windows PowerShell 5.1.

```powershell
git clone https://github.com/mateialexandru/dotfiles C:\avd\dotfiles
cd C:\avd\dotfiles
.\install.ps1
```

Run `sys check` afterwards to verify the environment.

Windows prerequisite installs preserve existing WinGet packages (`--no-upgrade`)
and existing npm tools. Missing packages can still require machine-wide elevation;
WinGet catalog membership is not a substitute for organizational approval.
Other installer stages can still rebuild or provision tools; this is not an
offline or universally non-admin operation.
The Doom font presets are installed for the current user from their upstream
font archives; only matching font files are extracted, and fonts already
available on the machine are reused. macOS uses
Homebrew font casks, and Linux installs the same preset families under
`~/.local/share/fonts`.

The Windows installer repairs PATH for an existing standard LLVM installation,
and Doom setup runs synchronously with checked exit codes without appending to
the public Emacs configuration. Bash language server is not required on Windows.
Managed Windows commands always use the dotfiles-built Emacs, not a system
installation. The existing installer builds or reuses it; source/bootstrap pins
and native-comp/AOT options live in `scripts\build-emacs-windows.ps1`.

```powershell
.\scripts\build-emacs-windows.ps1       # build or reuse; optional -Jobs 4 / -Rebuild
.\scripts\setup-doom.ps1                # normal Doom install/sync and launch integration
.\scripts\test-build-emacs-windows.ps1 -Doom  # separate Doom/Ghostel/GUI smoke check
```

The pinned, unpatched upstream build and its isolated MSYS2 compiler live under
ignored `.build\emacs\`, alongside logs and `accepted.json`. Bootstrap downloads
are hash-verified; pacman signatures remain enabled. Initial toolchain setup
performs a full update; later updates require `-UpdateTools` and a cleanly stopped
source Emacs. Changed source/options/toolchain or build-recipe version trigger a
new candidate; orchestration-only changes do not. Changed native tests revalidate
the accepted binary without rebuilding it. Failed checks preserve acceptance.
Building does not install or synchronize Doom.

The matching compiler remains necessary for native compilation. The launcher
sets its environment per invocation, never globally. It uses the last accepted
build even after build inputs change. Doom's environment snapshot excludes PATH
and compiler selection. For a daemon restart, save and quit normally, then:

```powershell
.\scripts\emacs-windows.ps1 -EmacsArguments @('--daemon')
.\scripts\emacs-windows.ps1 -Client -Gui -EmacsArguments @('-c', '-n')
```

Shell helpers accept ordinary flags (`emacs --daemon`, `emacsclient -c -n`);
the script takes `-EmacsArguments` to avoid PowerShell parameter binding.
Use `-Gui -EmacsArguments @('-Q')` for a clean diagnostic window.
Offline build/launcher regressions: `.\scripts\test-build-emacs-windows.ps1`.

Windows also provisions Graphviz and shfmt through WinGet. Existing working tools
and Emacs are preserved rather than replaced. Docker formatting is not a Windows
installation requirement; enabling the Docker module does not install a formatter.

On Windows, local builds use standard Emacs compilation because the pinned
Ghostel compilation backend requires `/bin/sh` and Unix PTYs. The workaround
lives in `config/doom/config-windows.el`; Ghostel terminals, comint integration
and remote compilation routing remain unchanged. No private layer is required.

Doctor treats missing Defender exclusions as an advisory, not a broken install.
Only with organizational approval, run `scripts\setup-doom.ps1 -AddDefenderExclusions`
from an elevated shell to exclude the active Emacs installation and Doom runtime
(including its symlink target). Existing exclusions are preserved, and rejected
changes stop with an error. This does not change network protection.

Windows health checks and the Doom wrapper refresh the child process PATH from
the current user/machine settings while preserving process-only entries. They
restore the caller's PATH and never change persistent PATH settings. This avoids
reporting newly installed tools as missing from a terminal opened before installation.

Doom Doctor uses upstream diagnostics without warning rewriting or advice.
Some checks target optional or Unix-oriented tools; `scripts\doctor.ps1` verifies
the Windows toolchain. `config/doom/cli.el` only keeps runtime-specific
environment values out of Doom's snapshot.

Gnuplot plotting is opt-in, because its WinGet installer declares machine scope.
After reviewing an exact version and approving elevation, install just that tool:

```powershell
.\scripts\install-gnuplot.ps1 -Version '6.0 patchlevel 4' -AllowElevation
```

The helper preserves an existing working Gnuplot, repairs its standard Windows
installation's PATH entry if needed, and stops on cancellation. To include it in
a full install, pass `-GnuplotVersion '6.0 patchlevel 4' -AllowGnuplotElevation`
to `install.ps1`. These flags express local consent, not an IT-policy exemption.

Working Pyright and Ruff installations are reused regardless of package manager.
`scripts/install-python-tools.ps1` checks their versions without forcing uv
ownership. Missing tools require explicit download consent. Both `uv` and
`uvx` must meet the reviewed security baseline, currently 0.13.4. Provisioning
can retire stale `~\.local\bin` copies only when a patched WinGet pair already
exists, preserving each old executable under
`%LOCALAPPDATA%\dotfiles\retired-tools` for rollback. Custom installations are
never moved automatically. Doctor reports unsafe selected versions as failures.

On Windows, new Ruff installations delegate to `astral-sh.ruff` in WinGet,
including WinGet's architecture selection and artifact verification. The
pinned catalog version is 0.17.2; already-working versions, including an
existing standalone 0.17.3 installation, are preserved without downgrading.
Approve the distribution under your organizational policy first:

```powershell
.\scripts\install-ruff-windows.ps1 -AllowDownloads
# Or include the Windows Ruff route in the full installation:
.\install.ps1 -AllowRuffDownloads -RuffVersion '0.17.2'
```

The helper reuses existing working Ruff versions, repairs PATH without duplicate
entries, and does not change PowerShell execution policy. Missing versions or
failed/cancelled WinGet operations stop explicitly; there is no alternate
download source, installer execution, or local checksum manifest.

For missing Pyright, the Python helper retains its separate approved uv opt-in:
`-AllowUvDownloads -PyrightVersion <exact-version>`. That flag does not authorize
Ruff downloads. Never retry a Defender-blocked source or switch sources to evade
policy; use a separately approved distribution or seek organizational guidance.
uv continues to manage project environments independently of global Ruff.

Offline installer-policy checks:
`pwsh -NoProfile -File scripts\test-install-tool-policy.ps1`.
Offline font-archive checks:
`pwsh -NoProfile -File scripts\test-install-fonts-windows.ps1`.
Offline Ruff package-manager delegation checks:
`pwsh -NoProfile -File scripts\test-install-ruff-windows.ps1`.
Offline Windows Ghostel compilation checks:
`emacs -Q --batch -l scripts\test-windows-ghostel.el -f ert-run-tests-batch-and-exit`.
Offline uv selection checks:
`pwsh -NoProfile -File scripts\test-uv-security.ps1`.

## Python projects in Emacs

Press `SPC p n` from any buffer. Choose **Python app** (the default, with a
straightforward `main.py`), **Python CLI package** (an installable command), or
**Python library**, then enter a project directory. The directory prompt starts
beside the current project. Emacs runs `uv init`, adds pytest and Ruff as project
development dependencies, and creates `.venv` and `uv.lock`. When setup finishes,
it switches to the new project workspace and opens `main.py` for an app or the
generated package source for a CLI package or library.

Afterward, use `SPC m t a` for project tests. From a terminal in the project,
use `uv run main.py` for a simple app, `uv run <project-name>` for a CLI package,
`uv add <package>` for runtime dependencies, and `uv run ruff check .` for
linting. `SPC c c` runs the current Python file, which is most useful for
standalone scripts and simple apps. Doom activates
the project's environment when a Python file opens and uses Pyright for
completion and types.

In a Python buffer, `SPC e` sends the current line to the project REPL, or
the selected region when one is active. `SPC E` evaluates the enclosing
function or class, including its decorators. The first evaluation starts the
project REPL and immediately sends the requested code. Visual `SPC e`,
`SPC m e`, and insert-mode `C-c e` use the same line-or-region behavior;
`SPC m b` evaluates the whole buffer, and `SPC m r` opens the project REPL.
These commands leave `.` available for repeating edits and use the
same interactive REPL, so definitions and variables remain available there;
short output appears inline, with longer output in a popup. For long-running
code, use `SPC c s` to send it to the REPL without waiting for inline output.
The infrequent uv adoption commands remain available through `M-x`.

## System operations

The installers build one native command for routine operations on every platform:

```text
sys install
sys update
sys check
sys doom sync
sys doom doctor
```

`install` converges the machine on this checkout, `update` upgrades the package and
tool ecosystems already under management, and `check` validates the complete setup.
Doom's narrower operations live under `sys doom`. `sys update` deliberately does not
pull this repository; choosing and integrating Git changes remains explicit.

`sys restart` closes every Emacs process, starts one fresh launchd daemon, and
opens Emacs Client.app connected to it. The restart countdown lets you save
work in standalone windows before they close. `sys llm` also
exposes a macOS-specific service; other
platforms receive an unsupported-operation error. Set `SYS_DOTFILES_DIR` only when the
checkout does not live at the conventional `~/Source/dotfiles` location.

## Trying Nushell

Nushell is installed and configured as a parallel interactive shell. Nothing changes the
login shell, terminal default, `.zshrc`, or existing Bash/PowerShell scripts. Start it
when wanted:

```text
nu
```

The public configuration supplies the same editor variables, user-tool paths, aliases,
zoxide, and fzf integration. It is linked through Nu's user autoload directory, leaving
an existing `config.nu` untouched. See `docs/decisions/020-nushell-parallel.md`.

## Pi coding agent

Pi is installed on every platform with portable defaults and the ChatGPT subscription
provider selected by default:

```text
pi
/login openai-codex    authorize ChatGPT Plus/Pro once
/model                 choose a model
```

The installer also installs provider-native web search and composes `config/pi/`
with optional `pi/` fragments from each activated private layer. Web search
reuses the selected supported model provider, so it needs no
separate search credential. A layer may add model providers,
narrow the visible model list, append private agent context, or run a
platform-specific setup hook. Runtime credentials, sessions, and trust decisions
stay in `~/.pi/agent/` and are never stored here.

Pi has no tool sandbox: an approved `bash` or PowerShell tool call runs with the user's
permissions. Project-local configuration therefore uses the `ask` trust policy.

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
  Type `<merm` then `TAB` at the beginning of a line to name and scaffold one.
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
The hands-on Org workflow is available as
[`docs/howto/howto-mermaid.org`](docs/howto/howto-mermaid.org).

## YouTube links in Org

Use `[[yt:VIDEO-ID][title]]` for a clickable title and put a bare
`[[yt:VIDEO-ID]]` link on the next line for an inline thumbnail. Both `RET` on
the link and a click on the thumbnail open the video in a browser. For a watch
list, put the titled link inside a `TODO` heading; it stays visible above the
thumbnail and can be marked `DONE` after watching. Thumbnails are downloaded
once into Doom's preview cache. HTML and Markdown export the title as a video
link and the thumbnail as a linked image, copying the cached image into the
adjacent `-assets` directory when available. This uses Org's native link
preview support and needs no additional package or `doom sync`.

## Excalidraw diagrams in Org

At the beginning of a line in Org, type `<excali` then `TAB` to name a new
Excalidraw drawing, insert its link, and open the editable `.excalidraw` file in
the PWA. Type `<merm` then `TAB` to name and insert a basic Mermaid flowchart
block. Saving in Excalidraw regenerates one opaque SVG for the inline Org
preview, including pasted images at their intended size. HTML export generates
a transparent SVG when needed, copies it into an adjacent `-assets` directory,
and fits it to the page width. Markdown export (`org-md-export-to-markdown`)
copies an opaque SVG into the same document-specific `-assets` directory and
inserts a relative image link, so an exported `readme.md` contains the drawing
preview without referring to the private `.excalidraw` source. While its Org
document is open, saving the drawing updates both its inline preview and any
existing HTML/Markdown asset copies. Re-export after reopening a closed
document to bring its assets up to date. Left-click the Org preview to reopen
it for editing. The `SPC m D`
(diagram) menu provides new, open-at-point, and rename actions. See
[`docs/howto/howto-excalidraw.org`](docs/howto/howto-excalidraw.org)
for a worksheet where you can bootstrap a diagram on the spot and follow the
complete edit/save/refresh loop. The same workflow runs on macOS and Windows;
on Windows, associate `.excalidraw` files with the installed Chrome Excalidraw
PWA once so `SPC m D o` can open the editor. Emacs's native file notifications
drive export on both platforms—no external watcher is required.

## API, JSON, and data tools in Emacs

- `.http` files use REST Client; send the request at point with `SPC m e` and
  process responses with jq. REST Client blocks also work from Org Babel.
- `.jq` files use `jq-mode`. In a JSON buffer, `SPC m q` interactively filters
  the active region or the whole buffer; `RET` applies and `C-g` cancels. See
  [`docs/howto/howto-jq.org`](docs/howto/howto-jq.org).
- JSON files use tree-sitter editing. `SPC m v` renders the selected JSON (or
  the whole buffer) as a local PlantUML structure preview; `SPC m V` exports it
  as SVG or PNG.
- In an Org buffer, `SPC m a p` saves a clipboard image as an attachment,
  `SPC m a P` attaches an image path or URL from the kill ring, and images can
  be dragged directly into the buffer. The installers supply `pngpaste`,
  `wl-clipboard`, or ImageMagick as the platform clipboard backend.
- `M-x sqlite-mode-open-file` opens a database with editable table navigation
  and ad-hoc query commands from `sqlite-mode-extras`.
- Org-roam adds content search and link previews under `SPC m m`: `s` searches
  note contents, `b` previews backlinks, and `l` previews forward links.

## Public how-tos in org-roam

Org-roam keeps captures and daily notes in the active personal or work context,
but also indexes the provisioned guides under `docs/howto/`. This produces one
searchable graph without copying public operating knowledge into a private
repository. Private notes may link to these stable public nodes; backlinks stay
private because public guides never link back to private IDs.

## gptel in Emacs

The everyday paths are visual `SPC r` for an auto-applied rewrite and `SPC o l e`
for a quick explanation. Use Pi in a terminal for full-project agent work.
See [`docs/howto/howto-gptel.org`](docs/howto/howto-gptel.org) for a
short hands-on guide to rewrites, follow-up chats, context, and
choosing between a layer-selected primary provider and local LM Studio/MLX. The
public configuration is provider-neutral; a private `doom/post.el` registers the
subscription backend with `my/gptel-register-primary-backend`.
On the Apple Silicon local path, `sys llm start` loads the configured Qwen3.6
MLX model for gptel quick help and Pi, with one inference slot and a one-hour
idle unload timer. Pi advertises a 128K context for local requests; LM Studio
currently auto-sizes the MLX runtime context when loading it.

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
may provide Doom, shell, Nushell, and Pi fragments. Early Doom settings load before Org,
while late commands load after the public config. Pi JSON is composed in layer order and
private agent context is appended to the public context.
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
