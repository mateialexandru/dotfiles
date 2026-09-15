# Doom Emacs Developer Workflow Reference

## Move

| Key | What | Notes |
|-----|------|-------|
| `SPC SPC` | find file in project | projectile fuzzy-find |
| `SPC .` | find file | from current directory |
| `SPC /` | search project | ripgrep across all files |
| `s` + 2 chars | evil-snipe | jump to next match (`;`/`,` to repeat) |
| `gs SPC` + char | avy | jump to visible char anywhere on screen |
| `gd` | `+lookup/definition` | lsp-mode first, etags fallback |
| `gD` | `+lookup/references` | all usages across solution |
| `K` | hover docs | signature / docs at point |
| `C-o` / `C-i` | jump back / forward | navigate jump history |
| `*` / `#` | search word forward / back | word under cursor |
| `%` | matching delimiter | jump between `()`/`{}`/`[]` |
| `{` / `}` | prev / next paragraph | jump between blocks |
| `SPC c j` | `consult-imenu` | symbols in current buffer |
| `SPC c J` | `consult-imenu-multi` | symbols across project |
| `SPC o p` | treemacs | project sidebar |

## Code Intel (lsp-mode + OmniSharp)

| Key | What | Notes |
|-----|------|-------|
| `gd` | go to definition | `lsp-find-definition` |
| `gD` | find references | `lsp-find-references` |
| `gr` | find references | same as `gD` |
| `K` | hover | `lsp-describe-thing-at-point` |
| `SPC c a` | code action | `lsp-execute-code-action` — generate stub, implement interface, add using |
| `SPC c r` | rename | `lsp-rename` — solution-wide |
| `SPC c d` | diagnostics list | buffer errors/warnings |
| `]d` / `[d` | next / prev diagnostic | jump to squiggle |
| `SPC c T` | find type definition | jump to type of symbol at point |
| `SPC c h` | inlay hints | `lsp-inlay-hints-mode` toggle — useful for LINQ / generics |

## Edit

| Key | What | Notes |
|-----|------|-------|
| `SPC c a` | code action | quickfix, extract method/class, organize imports |
| `SPC c r` | rename symbol | solution-wide via lsp-mode |
| `SPC c f` | format buffer | `+format/buffer` (CSharpier via apheleia) |
| `SPC c o` | organize imports | clean up usings |
| `za` | toggle fold | fold/unfold at point |
| `zM` / `zR` | close / open all folds | |
| `gc` + motion | comment | e.g. `gcc` for line, `gcap` for paragraph |
| `SPC ;` | comment line | toggle comment |

## Test & Build

| Key | What | Notes |
|-----|------|-------|
| `SPC p t` | `projectile-test-project` | runs last test command; `C-u` to change |
| `SPC m n` | `sharper-main-transient` | dotnet CLI menu — test, build, run, add package |
| `SPC o e` | eshell | toggle eshell popup |
| `SPC c C` | `compile` | run compile command |
| `SPC c c` | `recompile` | re-run last compile |
| `]e` / `[e` | next / prev error | in compilation output |
| `RET` | jump to location | on a line in compilation buffer |

## Git (Magit)

| Key | What | Notes |
|-----|------|-------|
| `SPC g g` | magit status | main entry point |
| `s` / `u` | stage / unstage | in magit buffer |
| `c c` | commit | write message, `C-c C-c` to confirm |
| `P p` | push | push to remote |
| `F p` | pull | pull from remote |
| `SPC g B` | blame | `magit-blame-addition` |
| `SPC g l` | log | git log for current file |
| `SPC g t` | time machine | step through file history |

## Buffers & Windows

| Key | What | Notes |
|-----|------|-------|
| `SPC ,` | switch buffer | in current workspace |
| `SPC b k` | kill buffer | |
| `SPC b N` | new buffer | scratch buffer |
| `SPC w v` | vertical split | |
| `SPC w s` | horizontal split | |
| `SPC w w` | cycle windows | or `C-w C-w` |
| `SPC w q` | close window | |
| `SPC TAB n` | new workspace | persp-mode |
| `SPC TAB d` | delete workspace | |
| `SPC TAB [` / `]` | prev / next workspace | |
| `SPC TAB TAB` | switch workspace | |

## Search & Replace

| Key | What | Notes |
|-----|------|-------|
| `SPC /` | project search | ripgrep via consult |
| `SPC s s` | search buffer | `consult-line` |
| `SPC s p` | search project | same as `SPC /` |
| `SPC s d` | search directory | pick a directory first |
| `SPC s e` | search/replace | `query-replace` via vertico |
| `C-c C-e` | edit search results | in grep buffer → wgrep for bulk replace |

## Tags Fallback

| Key | What | Notes |
|-----|------|-------|
| `SPC c t` | `my/create-tags` | regenerate TAGS via Universal Ctags — before LSP connects |
