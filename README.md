# Dotfiles

Cross-platform Doom Emacs configuration for macOS, Windows, and Linux (Bluefin/Fedora).

## Glove80 programming layout

[`config/glove80.json`](config/glove80.json) is a macOS layout for **Colemak
selected in the operating system**. Base letters send QWERTY keycodes. Import
the JSON into the [MoErgo Layout Editor](https://my.moergo.com/glove80), build
the firmware, and follow the editor's flashing instructions to apply it.
The installers do not apply keyboard firmware.

The outer left key directly below Esc (originally Grave/backtick) is sticky
Shift: tap and release, then press the key to shift. It uses the firmware's
default sticky-key timeout (ZMK defaults to one second). Grave/backtick remains
available on Symbols at physical P. The upper-left thumb remains dedicated
Symbols, and the right upper thumb remains ordinary Shift.

The thumb assignments are:

| Key | Tap | Hold |
| --- | --- | --- |
| Left upper Shift position | Symbols immediately while pressed | Symbols |
| Left Backspace | Backspace | Repeat deletion |
| Left thumb Tab | Tab | Numbers |
| Right Space | Space | Navigation |
| Right Enter | Enter | Command |
| Left Layer | Symbols immediately while pressed | Symbols |

Release a layer hold to return to Base. Within these three typing layers,
the thumb Backspace, Tab, and Space keys produce plain keypresses, so you can enter
complete expressions without activating another layer. The outer left Tab
remains a dedicated Tab. Backspace repeats normally; the right upper Shift
remains Shift. Enter uses tap/hold in Base and the typing layers: hold Enter
for Command, tap the left thumb Tab to cycle apps, then release Enter to choose.
Tap/hold uses the existing firmware defaults; try quick typing and corrections
before adjusting timing. Release typing-layer holds before using Magic,
because Navigation and Numbers have higher layer priority than Magic.

The five main columns on each hand are shown by physical position:

```text
                 Left hand           Right hand
Upper row      !  @  #  $  %         ^  &  *  ~  `
Home row       ;  [  (  {  <         >  }  )  ]  :
Lower row      \  /  -  =  +         _  "  '  ,  .
```

The outer key beside the upper-right row produces `|`; the outer key beside
the lower-left row produces `?`. Each symbol needs only Layer plus its key,
without an additional Shift press. Semicolon and colon intentionally send
P and Shift+P for standard host Colemak; switching the OS to QWERTY would
produce p and P instead.

Symbols replaces the factory Lower layer's navigation/numpad mappings while
retaining its media row. Other positions inherit Base, including digits,
arrows and modifiers. Magic and Factory are unchanged.

Navigation uses a J/K/L/semicolon arrow row on the four resting columns,
shown by physical QWERTY position:

```text
 Y           U             I             O           P
Line start  Word left   Word right    Line end     Page Up
 H           J             K             L           ;
Line end    Left          Down           Up        Right
 N           M             ,             .           /
Backspace Cmd+Backspace  Reserved       Delete     Page Down
```

U/I send Option+Left/Right. The macOS Doom configuration binds these Alt-arrow
events to backward/forward word, while preserving Command as Meta. Y/O
send Ctrl+A/E for line movement; Ctrl+E uses the physical K keycode because
the host applies Colemak. Doom retains its existing indentation/comment-aware
line commands. Physical F holds Shift to extend movement into a selection
in supporting contexts. P/slash give
Page Up/Down. N gives Backspace and period gives Delete, both repeatable when held.
M sends Command+Backspace: Meta+Backspace (backward word deletion) in Doom,
and deletion to line start in standard macOS text fields. H also provides line end,
using the same Ctrl+E binding as O. Comma is reserved for a future editing
command and currently disabled.

The left side of Navigation provides app and window controls, also labelled
by physical QWERTY position:

```text
       W                 E                 R
 Ctrl+Shift+Tab        Ctrl+Tab       Hold Ctrl+Option
 A                 S                 D                 F
Ctrl             Option          Reserved           Shift
```

While holding right-thumb Space, W/E switch tabs in apps supporting Ctrl+Tab.
Hold F with right-hand movement to select text. Hold R and use the right-hand
arrows for Rectangle window halves; R plus a tap of thumb Enter maximizes
with Rectangle's recommended shortcuts. D and V are reserved and disabled.
App switching uses the thumb Enter/Command and Tab keys instead of a
dedicated Navigation shortcut.
These are macOS shortcuts; Doom still interprets Command as Meta when the
OS does not intercept a shortcut. Rectangle settings are managed locally,
not installed by this repository.

The thumb Enter remains available. Reload Doom's configuration or restart Emacs after
updating the macOS bindings; no `doom sync` is needed.

Numbers uses standard number keycodes in a right-hand numpad:

```text
Physical positions      7  8  9             7  8  9
                     Y  U  I  O  P       /  4  5  6  *
                     H  J  K  L  ;       -  1  2  3  +
                     N  M  ,  .  /       0  —  0  .  .
```

The comma/less-than key gives 0 without Shift. M is reserved and disabled.
The outer right backslash and quote positions give equals and comma.
Keep host Colemak selected when reading these physical QWERTY labels.
Before relying on the layout, check `::`, `->`, `!=`, `&&`, `<=`, `>=`,
`[]`, `{}`, `()`, quotes, and Python underscores with the intended OS input
source, locally and through remote desktop where used. The JSON can be
checked locally, but firmware build and typing behavior need verification
in the editor and on the keyboard.

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

Prerequisite: Developer Mode enabled (for symlinks) or run as Administrator.

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
font archives; fonts already available on the machine are reused. macOS uses
Homebrew font casks, and Linux installs the same preset families under
`~/.local/share/fonts`.
The `prose-libron` Fontaine preset pairs JetBrains Mono for code with
[Libron](https://github.com/nicoverbruggen/libron/releases) for Org and Markdown
body text. The installers add Libron for the current user; select the preset
with `SPC t F`. It does not change the default coding font.

The Windows installer repairs PATH for an existing standard LLVM installation,
and Doom setup runs synchronously with checked exit codes without appending to
the public Emacs configuration. Bash language server is not required on Windows.
When `EMACS_NATIVE_COMP_ROOT` selects an approved MinGW compiler runtime,
Windows Doom setup and `sys doom sync|doctor` add its `bin` directory only to
the Emacs process environment, limit native compilation to two workers, and
surface nonzero Emacs exits. Doctor also checks native-comp availability.
These commands do not download or update that runtime.

Doctor treats missing Defender exclusions as an advisory, not a broken install.
Only with organizational approval, run `scripts\setup-doom.ps1 -AddDefenderExclusions`
from an elevated shell to exclude the active Emacs installation and Doom runtime
(including its symlink target). Existing exclusions are preserved, and rejected
changes stop with an error. This does not change network protection.

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
`scripts/install-python-tools.ps1` checks their versions without downloading or
forcing uv ownership. If either is missing, the default is an actionable error.
On machines where uv downloads are permitted, explicitly supply
`-AllowUvDownloads -PyrightVersion <exact-version> -RuffVersion <exact-version>`
to that helper. Never retry a Defender-blocked source or switch sources to evade
policy; seek organizational guidance instead. uv continues to manage project
environments independently of these global tool installations.

Offline installer-policy checks:
`pwsh -NoProfile -File scripts\test-install-tool-policy.ps1`.

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
