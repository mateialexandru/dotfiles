#!/usr/bin/env bash
# sys check — one-shot confidence pass over the Doom/dotfiles rig.
# Plain KISS lines: [ok] pass, [X] fail (red), [..] info (never fails).
# Hard exit 1 if any check fails. Remediation for every failure: `sys install`.
set -uo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DOOM="$HOME/.config/emacs/bin/doom"
EMACSCLIENT="emacsclient"
IS_MAC=false; [[ "$(uname)" == "Darwin" ]] && IS_MAC=true

# colour only on a tty
if [[ -t 1 ]]; then RED=$'\033[31m'; DIM=$'\033[2m'; RST=$'\033[0m'; else RED=; DIM=; RST=; fi

fails=0
pass() { printf '[ok] %-18s %s\n'         "$1" "$2"; }
info() { printf "${DIM}[..] %-18s %s${RST}\n" "$1" "$2"; }
skip() { printf "${DIM}[--] %-18s %s${RST}\n" "$1" "$2"; }
fail() { printf "${RED}[X]  %-18s %s${RST}\n" "$1" "$2"; fails=$((fails+1)); }

printf 'sys check — %s\n\n' "$(date '+%Y-%m-%d %H:%M')"

# 1. doom symlink points at this repo
want="$REPO/config/doom"; got="$(readlink "$HOME/.config/doom" 2>/dev/null || true)"
# shellcheck disable=SC2088  # tilde here is literal display text, not a path
if [[ "$got" == "$want" ]]; then pass "doom symlink" "~/.config/doom → $got"
else fail "doom symlink" "is '${got:-missing}', want '$want' — run \`sys install\`"; fi

# 2. daemon inherited the login shell's env. launchd starts it with a bare
# /usr/bin:/bin:/usr/sbin:/sbin, so exec-path-from-shell has to run at daemon boot
# (config/doom/config-macos.el). Without it Emacs can't see Homebrew tools and
# libgccjit loses libemutls_w.a (ADR-009).
if $IS_MAC; then
  if $EMACSCLIENT --eval t >/dev/null 2>&1; then
    denv="$($EMACSCLIENT --eval '(list (and (executable-find "cmake") t) (and (getenv "LIBRARY_PATH") t))' 2>/dev/null)"
    case "$denv" in
      "(t t)") pass "daemon env" "shell PATH + LIBRARY_PATH inherited" ;;
      *)       fail "daemon env" "not inherited from login shell ${denv:-?} — run \`sys restart\`" ;;
    esac
  else skip "daemon env" "daemon down"; fi
fi

# 3. emacs-plus daemon — reachability (emacsclient ping) is the source of truth.
# brew services can show a stale 'error'/'stopped' label while the daemon still serves
# (launchd last-exit artifact), so ping first; the service label is only informational.
daemon_up=false
if $EMACSCLIENT --eval t >/dev/null 2>&1; then
  daemon_up=true
  if $IS_MAC; then
    svc="$(brew services list 2>/dev/null | awk '/emacs-plus@30/{print $2}')"
    if [[ "$svc" == "started" ]]; then pass "daemon" "emacs-plus@30 started + ping ok"
    else info "daemon" "reachable, but brew service='${svc:-unknown}' (stale label — \`sys restart\` to clear)"; fi
  else pass "daemon" "reachable"; fi
elif $IS_MAC; then
  svc="$(brew services list 2>/dev/null | awk '/emacs-plus@30/{print $2}')"
  fail "daemon" "not reachable (brew service: ${svc:-not started}) — run \`sys restart\`"
else info "daemon" "not running (start emacs --fg-daemon when needed)"; fi

# 4. native-comp queue (info only — >0 means baking, not broken)
if $daemon_up; then
  q="$($EMACSCLIENT --eval '(if (boundp (quote comp-files-queue)) (length comp-files-queue) 0)' 2>/dev/null | tr -d '"')"
  case "$q" in
    0)  pass "native-comp" "queue drained" ;;
    ''|*[!0-9]*) skip "native-comp" "n/a" ;;
    *)  info "native-comp" "$q baking (transient — rerun later)" ;;
  esac
else skip "native-comp" "daemon down"; fi

# 5. doom doctor — hard-fail on errors and stale module names. Symbola is an
# optional Doom recommendation and is not auto-installed because current
# releases use a separate font license.
if [[ -x "$DOOM" ]]; then
  if "$DOOM" doctor >/tmp/sys-doctor.log 2>&1; then
    warns="$(grep -oE '[0-9]+ warning' /tmp/sys-doctor.log | grep -oE '[0-9]+' | head -1)"
    if grep -q 'module was moved' /tmp/sys-doctor.log; then
      fail "doom doctor" "stale module name — run \`sys doom doctor\`"
    elif [[ -z "$warns" || "$warns" == 0 ]]; then pass "doom doctor" "clean"
    else info "doom doctor" "$warns known optional warnings (Symbola, Zig compile fallback, pipenv/nose — \`sys doom doctor\` for detail)"; fi
  else fail "doom doctor" "errors — see /tmp/sys-doctor.log or run \`sys doom doctor\`"; fi
else fail "doom doctor" "doom binary missing at $DOOM — run \`sys install\`"; fi

# 6. core tools on PATH
missing=()
for t in rg fd node dotnet nu sys emacsclient mmdc hack; do command -v "$t" >/dev/null 2>&1 || missing+=("$t"); done
[[ -x "$HOME/.dotnet/tools/csharpier" ]] || missing+=(csharpier)
if [[ ${#missing[@]} -eq 0 ]]; then pass "tools" "rg fd node dotnet nu sys csharpier mmdc hack"
else fail "tools" "missing: ${missing[*]} — run \`sys install\`"; fi

# Nushell remains optional as the login shell, but its managed autoload entry must work.
if command -v nu >/dev/null 2>&1; then
  nu_autoload="$(nu -n -c '$nu.user-autoload-dirs | first' 2>/dev/null)"
  nu_want="$REPO/config/nushell/dotfiles.nu"
  nu_got="$(readlink "$nu_autoload/10-dotfiles.nu" 2>/dev/null || true)"
  if [[ "$nu_got" == "$nu_want" ]]; then pass "nushell config" "autoload linked"
  else fail "nushell config" "missing or stale — run scripts/install-nushell.sh"; fi
fi

# Ghostel is the terminal backend. Its native module lives outside the package
# checkout so package upgrades cannot truncate a module mapped by the daemon.
ghostel_module="$HOME/.config/emacs/.local/etc/ghostel/ghostel-module"
if $daemon_up; then
  ghostel_state="$($EMACSCLIENT --eval \
    '(condition-case err
         (progn (require (quote ghostel))
                (if (featurep (quote ghostel-module)) "ready" "not loaded"))
       (error (format "error: %s" (error-message-string err))))' \
    2>/dev/null | tr -d '"')"
  if [[ "$ghostel_state" == ready ]]; then
    pass "ghostel" "native libghostty module loaded"
  else
    fail "ghostel" "$ghostel_state — run \`sys install\` or open M-x ghostel"
  fi
elif compgen -G "${ghostel_module}.*" >/dev/null; then
  pass "ghostel" "native libghostty module present (daemon down)"
else
  fail "ghostel" "native module missing — run \`sys install\` or open M-x ghostel"
fi

# 7. Roslyn C# LSP DLL present
if [[ -f "$HOME/.local/share/roslyn-lsp/Microsoft.CodeAnalysis.LanguageServer.dll" ]]; then
  pass "roslyn lsp" "DLL present"
else fail "roslyn lsp" "DLL missing — run \`sys install\`"; fi

# 8. LSP servers on PATH (Roslyn covered above; csharpier under tools)
lsp_missing=()
for s in rust-analyzer lua-language-server bash-language-server yaml-language-server \
         typescript-language-server vscode-json-language-server pyright; do
  command -v "$s" >/dev/null 2>&1 || lsp_missing+=("$s")
done
if [[ ${#lsp_missing[@]} -eq 0 ]]; then pass "lsp servers" "7 present"
else fail "lsp servers" "missing: ${lsp_missing[*]} — \`sys install\` (uv tools → ~/.local/bin on PATH)"; fi

# 9. formatters required by enabled Doom modules
fmt_missing=()
for f in shfmt ruff clang-format; do command -v "$f" >/dev/null 2>&1 || fmt_missing+=("$f"); done
if [[ ${#fmt_missing[@]} -eq 0 ]]; then pass "formatters" "shfmt ruff clang-format"
else fail "formatters" "missing: ${fmt_missing[*]} — run \`sys install\`"; fi

# 10. PlantUML jar where Doom's module expects it
if [[ -f "$HOME/.config/emacs/.local/etc/plantuml.jar" ]]; then
  pass "plantuml" "Doom jar present"
else fail "plantuml" "jar missing — run \`sys install\`"; fi

# 11. Emacs Client.app — GUI/Spotlight entry (ADR-009), copied not symlinked; macOS only
if $IS_MAC; then
  if [[ -d "/Applications/Emacs Client.app" ]]; then pass "emacs client.app" "present in /Applications"
  else fail "emacs client.app" "absent — run \`sys install\`"; fi
fi

# 12. org-protocol → Scrim handler pin — Safari capture pipeline (ADR-010); macOS only
if $IS_MAC; then
  h="$(duti -d org-protocol 2>/dev/null)"
  if [[ "$h" == "com.yummymelon.scrim" ]]; then pass "org-protocol" "→ Scrim"
  else fail "org-protocol" "handler '${h:-none}' not Scrim — run scripts/install-scrim-captee-mac.sh"; fi
fi

# 13. excalidraw export toolchain (ADR-008) — installed on macOS only
if $IS_MAC; then
  if command -v excalidraw-cli >/dev/null 2>&1; then pass "excalidraw" "excalidraw-cli"
  else fail "excalidraw" "missing: excalidraw-cli — run scripts/install-excalidraw-mac.sh"; fi
fi

# 14. Ollama local LLM (ADR-013) — optional and on-demand. A missing binary is
# informational because install.sh supports --skip-llm.
# LM Studio is user-managed, so it's not gated here.
if $IS_MAC; then
  if command -v ollama >/dev/null 2>&1; then
    if curl -fsS http://localhost:11434/api/tags >/dev/null 2>&1; then
      manifest="$REPO/scripts/ollama-models.txt"
      have="$(ollama list 2>/dev/null | awk 'NR>1{print $1}')"
      miss=()
      while IFS= read -r line; do
        m="${line%%#*}"; m="$(echo "$m" | xargs)"; [ -z "$m" ] && continue
        echo "$have" | grep -q "^${m%%:*}" || miss+=("$m")
      done < "$manifest"
      if [[ ${#miss[@]} -eq 0 ]]; then pass "ollama" "up + models present"
      else info "ollama" "up; models not pulled: ${miss[*]} (\`sys install\` to pull)"; fi
    else info "ollama" "installed, down — \`sys llm start\` to serve"; fi
  else info "ollama" "not installed (optional — \`sys install\` without --skip-llm)"; fi
fi

# 15. gptel LLM client (ADR-014) — packages built, and the global gitignore
# actually in effect. The second half is the one that bites silently: project
# transcripts live in <repo>/.gptel/, so a missing link makes them committable.
gptel_missing=()
for p in gptel gptel-agent; do
  compgen -G "$HOME/.config/emacs/.local/straight/build-*/$p" >/dev/null || gptel_missing+=("$p")
done
if [[ ${#gptel_missing[@]} -eq 0 ]]; then pass "gptel" "gptel + gptel-agent built"
else fail "gptel" "not built: ${gptel_missing[*]} — run \`sys doom sync\`"; fi

if git -C "$REPO" check-ignore -q .gptel/chat.org 2>/dev/null; then
  # shellcheck disable=SC2088  # literal display text
  pass "global gitignore" "~/.config/git/ignore in effect"
else
  fail "global gitignore" "'.gptel/' not ignored — run \`sys install\`"
fi

# 16. Universal Ctags options are connected through the XDG preload directory
ctags_want="$REPO/config/ctags"; ctags_got="$(readlink "$HOME/.config/ctags" 2>/dev/null || true)"
# shellcheck disable=SC2088  # literal display text
if [[ "$ctags_got" == "$ctags_want" ]]; then pass "ctags config" "~/.config/ctags → $ctags_got"
else fail "ctags config" "is '${ctags_got:-missing}', want '$ctags_want' — run \`sys install\`"; fi

# 17. Compiled worktree tooling
if command -v hack >/dev/null 2>&1 && hack --help >/dev/null 2>&1; then
  pass "hack tooling" "compiled binary available"
else fail "hack tooling" "not installed — run scripts/install-hack.sh"; fi

# 18. ssh ControlMaster block for tailnet
if grep -q 'Host \*.ts.net' "$HOME/.ssh/config" 2>/dev/null; then pass "ssh controlmaster" "Host *.ts.net present"
else fail "ssh controlmaster" "block absent — run \`sys install\`"; fi

# summary + hard exit
echo
if [[ $fails -eq 0 ]]; then printf 'summary: all good ✓\n'; exit 0
else printf "summary: ${RED}%d failing${RST} — run \`sys install\` to repair\n" "$fails"; exit 1; fi
