# ADR-015: Python in Doom — uv owns environments, ruff owns style

**Status:** Accepted
**Date:** 2026-08-03

## Context

Python was half-provisioned. `install.sh` installed `uv` plus six global Python tools
(`pyright`, `black`, `isort`, `pyflakes`, `pytest`, `ruff`) as `uv tool`s, and
`sys check` checked for them — but Doom's `:lang python` module was enabled **with no
flags**. That combination gives a mode with syntax highlighting and nothing else: no LSP
hook, no virtualenv detection, no tree-sitter. The tools were on PATH and unreachable from
the editor.

Two further problems were latent in the tool list itself:

- **Global tools shadow project tools.** A `uv tool`-installed `pytest` lives in its own
  isolated environment and cannot see a project's dependencies. Running it against a real
  project fails on the first `import`.
- **`pyright` is a language server, but installed the same isolated way.** Left alone it
  type-checks against its own environment, so every third-party import in a project
  resolves to nothing.

Meanwhile Homebrew's `python@3.14` is marked externally managed (PEP 668), so the
system-wide `pip install` path that would otherwise paper over this is closed anyway —
which is the correct outcome, not an obstacle.

## Decision

**uv is the only thing that installs Python or resolves dependencies. Global tools are
limited to the two that must run outside a project. ruff is the single style/lint binary.**

### Environments: the `+uv` module flag

Doom's `:lang python` carries a `+uv` flag (`uv-mode`), which registers
`+python-uv-mode-set-auto-h` on `python-mode-local-vars-hook` and `doom-switch-buffer-hook`.
Opening any file under a uv project activates that project's `.venv` with no prompt and
surfaces `UV:<version>` in the modeline. `uv.lock` is mapped to `conf-toml-mode`.

This replaces the `+pyenv` / `+poetry` / `+conda` alternatives. `+uv` and `+pyenv` are
mutually exclusive per the module's own documentation, and uv already subsumes what pyenv
does (it installs and pins interpreters).

Module line:

```elisp
(python +lsp +pyright +uv +tree-sitter)
```

### Style and lint: ruff only

`ruff format` is black-compatible and `ruff check --select I --fix` does what isort does, in
one binary, roughly two orders of magnitude faster. `config/doom/config-python.el` points apheleia
at `(ruff-isort ruff)` for both `python-mode` and `python-ts-mode` — both are needed because
`+tree-sitter` means `python-ts-mode` is what actually runs. `python-check-command` and the
flycheck checker follow suit; `python-flake8` and `python-pylint` are disabled so flycheck
does not shell out to tools that are no longer installed.

`black`, `isort`, and `pyflakes` are therefore uninstalled by `install.sh`, not merely
dropped from the install list — a stale shim in `~/.local/bin` would keep winning on PATH.

### Type checking: pyright, pointed at the project venv

`lsp-pyright-venv-directory` is set to `.venv`, so pyright resolves imports against the
environment `uv sync` created rather than its own. This single setting is what makes the
LSP experience correct; without it the server runs but reports phantom import errors.

The `lsp` module is bare (lsp-mode, Doom's default), so `+pyright` applies — the module
gates `lsp-pyright` on `:tools lsp -eglot`.

Astral's `ty` is the direction of travel and is what Doom's module README now recommends.
It is still preview-grade, so pyright stays for now; the swap is one line in `install.sh`
plus an lsp-mode priority setting.

### Tests: `uv run pytest`

`python-pytest-executable` is set to `"uv run pytest"`. uv resolves the project venv itself,
so `SPC m t a` works from any buffer without the venv being active in the Emacs process, and
pytest comes from the project's own lockfile at the project's own pinned version.

### Editor integration: lean on what already exists

Three things people commonly hand-roll for uv are already handled, and are deliberately
*not* reimplemented here:

- **Venv activation.** The widely-circulated `uv-activate` function (mclare.blog) is exactly
  what `uv-mode-set` already does — `exec-path`, `PATH`, `VIRTUAL_ENV`, `pythonic-activate`
  — and `+uv` calls it automatically on every buffer switch, so there is nothing to invoke.
  The post predates the module flag.
- **Ruff quickfixes.** `lsp-ruff` ships in lsp-mode's default `lsp-client-packages` and is
  registered `:add-on? t`, so `ruff server` attaches *alongside* pyright with no
  configuration. Verified live: a buffer reports both `pyright:NNN` and `ruff:NNN`, and
  `SPC c a` offers "Remove unused import", "Fix all auto-fixable problems" and "Organize
  imports" from ruff plus pyright's own actions.
- **Imports.** pyright's `python.analysis.autoImportCompletions` defaults on, so completing
  an unimported symbol inserts its import; `SPC c a` / `SPC c o` cover the rest.

Two gaps are real, and both are small:

- **projectile had no uv type.** A uv project matched `python-toml`, so `SPC p t` ran
  `python -m unittest discover` and `SPC p c` ran `python -m build`. Registering
  `python-uv` on `uv.lock` (registration prepends, so it wins) maps these onto
  `uv run pytest`, `uv build`, `uv sync`, `uv run`.
- **`compile-command`** defaults to `uv run <file>` in python buffers, so `SPC c c` runs the
  file through the right interpreter whether it is a project member or a PEP 723 script.

### Bootstrapping: the only bespoke commands

uv cannot infer which distribution an `import` refers to, so that one step is scripted.
Three commands under `SPC m u`, and nothing else — `uv add`, `uv sync` and friends stay
shell commands rather than becoming editor wrappers:

| Key | Command | Effect |
|-----|---------|--------|
| `SPC m u n` | `+python/uv-new-project` | `uv init` + `uv sync`, opens `main.py` |
| `SPC m u p` | `+python/uv-adopt-file` | moves this file into a new isolated project with its dependencies |
| `SPC m u s` | `+python/uv-adopt-script` | gives this file a PEP 723 header instead — no project, `uv run` anywhere |
| `SPC m u v` | `uv-mode-set` | re-activate the venv by hand |

Import→distribution mapping is pipreqs run through `uvx` (nothing to install; it maps `PIL`
to `Pillow`, `bs4` to `beautifulsoup4`, and skips stdlib and sibling modules). Two details
matter:

- **The file is copied somewhere empty before scanning.** Adopting one script out of a
  directory of them must not pick up its neighbours' imports.
- **Modules found beside the file are subtracted from the result.** The isolation above
  also hides the file's own siblings from pipreqs, which then looks them up on PyPI. If a
  package of that name happens to exist, it lands in the install list — a local
  `helpers.py` silently becoming a dependency on some stranger's `helpers`. That is
  dependency confusion, so local module names are removed from both the seed and the
  warning.
- **Unresolvable imports are surfaced, not dropped.** pipreqs silently discards imports it
  cannot map — `cv2` has no PyPI package of that name and wants `opencv-python`. Those are
  named in the prompt, and the seeded list is editable before anything installs.
- **The prompt keeps `uv add:` adjacent to the editable text.** With the warning rendered
  as a bare prefix the seed reads as part of it; the first real-world use was reported as
  "unresolved edtrace regex tiktoken" when those three were in fact the correct seed and
  the unresolved pair was `lecture_util`/`references`.

Directory-wide scanning was tried and abandoned: pipreqs descends into `.venv` and reports
the optional imports buried inside installed libraries (`brotli`, `pyodide`, `cchardet`) as
project dependencies. Scanning is single-file only.

## Consequences

- Global Python tools drop from six to two (`pyright`, `ruff`). `sys check` checks the
  same two.
- Anything project-specific (pytest, mypy, nox, ...) is a project dependency invoked through
  `uv run`, which is where version pinning belongs.
- New project setup is `uv init && uv add <deps> && uv sync`; opening a file is then enough
  to get LSP, format-on-save, lint, and tests. Nothing to activate.
- A project without a `.venv` gets no venv activation and pyright falls back to the
  interpreter on PATH. This is the correct failure mode — `uv sync` fixes it.
- `+lsp`, `+pyright`, `+uv`, and `+tree-sitter` are module flags, so they need
  `doom sync` and a restart, unlike `config-python.el` itself.

## Alternatives considered

- **[uv.el](https://github.com/Ethan0456/uv.el)** — a transient over every uv subcommand.
  Rejected on maintenance grounds this repo weighs heavily: 23 commits all landed on
  2026-04-28, nothing since, no license file, not on MELPA. It also wouldn't cover the one
  thing that needed writing (adopting a file with its dependencies) — it wraps subcommands,
  which is the part deliberately left to the shell.
- **[pet](https://github.com/wyuenho/emacs-pet)** — detects venvs across poetry/pipenv/uv/
  pdm/hatch/pixi and configures every tool from them. Well-built, but it solves the problem
  `+uv` already solves for the one tool in use here, and wants `dasel`/`yq` for TOML.
- **A `SPC m u` wrapper per uv subcommand** (add/remove/sync/lock/upgrade). Built, then
  removed: they are one-line shell commands with no editor state to contribute, and the repo
  has vterm and eshell a keystroke away.

- **`+poetry` / `+pipenv` / `+conda`** — all solve environment management, none is what the
  rig already installs. uv is already in `install.sh` and is strictly faster.
- **Keep black + isort.** Two more binaries to install and health-check for output ruff
  already produces. Retained only as a fallback if a project's CI pins black specifically —
  in which case it belongs in that project's dev dependencies, not globally.
- **`ty` now.** Recommended by Doom, matches the Astral stack, but preview-grade; pyright is
  already installed, health-checked, and mature.
- **Global `pytest`.** Cannot see project dependencies. `uv run pytest` is the same
  keystroke with correct resolution.

## References

- [uv](https://github.com/astral-sh/uv) · [ruff](https://github.com/astral-sh/ruff) · [ty](https://github.com/astral-sh/ty)
- `~/.config/emacs/sources/doom+/modules/lang/python/README.org` — module flags
- [PEP 668](https://peps.python.org/pep-0668/) — externally managed environments
- ADR-012 (`sys` ops wrapper), ADR-001 (LSP client choice)
