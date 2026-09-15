# ADR-010: Safari → Emacs org capture via org-protocol

**Status:** Superseded in part — see [Revision 2026-05-22](#revision-2026-05-22-adopt-scrim--captee-retire-the-diy-stack). The org-protocol receiving end (Doom `+org-protocol`, capture template `L`) stands; the self-built `OrgProtocol.app` handler, the Safari Web Extension, and the Xcode auto-installer are retired.
**Date:** 2026-05-18

## Context

Capturing a Safari URL into org used to be a context-switch: focus Emacs, `SPC X`, paste, type a title, save. The friction was high enough that links accumulated in browser tabs and Apple Notes instead of `inbox.org`.

The user wanted a one-click capture: click a Safari bookmarklet → page URL + title (+ any selected text) appended to the configured Org inbox → Emacs stays in the background.

## Decision

Use `org-protocol` (Emacs-side) + a hand-rolled `OrgProtocol.app` URL handler (macOS-side) + a Safari bookmarklet.

### Concrete choices

| Aspect | Choice | Rationale |
|---|---|---|
| Capture transport | `org-protocol://` URL scheme | The org-mode community standard. Decoupled: any process that can `open` a URL can capture (Safari, Shortcuts, scripts). |
| Trigger UI | Safari bookmarklet on the Favorites Bar | Zero extra software. One-keystroke (`Cmd+1..9`) once placed. Carries `document.title` and `window.getSelection()` in query params. |
| Doom integration | `(org +roam2 +org-protocol)` flag | Pulls in `org-protocol.el` and binds it into `after-init`. Cheaper than `require`-ing manually because Doom handles autoloads. |
| Template | Single `L` template, `:immediate-finish t`, appends to `inbox.org` "Links" | Captures should not pop a buffer or steal focus — they should be silent. Triage happens later in `inbox.org`. |
| macOS handler | Custom `OrgProtocol.app` from `osacompile` + `PlistBuddy` | ~30-line install script (`scripts/install-org-protocol-mac.sh`). Mirrors `install-emacs-mac.sh`'s `lsregister` + idempotency pattern. No extra brew/cask dependency, no third-party app to vet. |
| App location | `~/Applications/OrgProtocol.app` | User-level — install script needs no `sudo`. LaunchServices searches user `~/Applications` for URL handlers. |
| `emacsclient` resolution | `command -v emacsclient` at install time, baked into the AppleScript | Avoids relying on `$PATH` at click time (Safari → LaunchServices → osascript runs with a near-empty environment). |

## Consequences

- Capture is silent: bookmarklet click → no Emacs frame appears, daemon writes the entry in the background.
- `inbox.org` is created on first capture; nothing pre-seeds it.
- Re-running `install.sh` is a no-op once `OrgProtocol.app` declares the URL scheme — the script short-circuits on the `PlistBuddy` check.
- Path-fragile across Homebrew prefix changes (e.g. moving from `/usr/local` to `/opt/homebrew`). Re-run the installer to re-bake `emacsclient`'s absolute path.
- A second app could register `org-protocol://` and shadow this one. LaunchServices' last-writer-wins for URL scheme handlers; if that happens, re-running `lsregister -f` on `OrgProtocol.app` restores it.
- The bookmarklet is in `README.md`, not synced automatically into Safari. Manual one-time step per machine.

## Alternatives considered

- **`org-mac-link`** (`SPC n l s`): pull-based — must focus Emacs first, then invoke. Defeats the "stay in the browser" workflow. Useful as a fallback when bookmarklets aren't installed.
- **Apple Shortcut → `emacsclient -e '(org-capture-string …)'`**: works, but Shortcuts are not file-tracked, harder to provision via a dotfiles installer, and the URL scheme approach is portable to any browser (Firefox, Chromium) without changing the Emacs side.
- **Third-party `OrgProtocol.app` from a Homebrew tap**: more dependency churn, less control. The AppleScript handler is ten lines and unlikely to drift.
- **A `.desktop`-style helper on Linux**: deferred — this ADR is macOS only. Linux can register `org-protocol` via `xdg-mime` + a small `.desktop` file when the use case arrives.

## Revision 2026-05-20: extension + Services entryways

The bookmarklet shipped first but is demoted to a fallback. Two better entryways added; both still emit the same `org-protocol://capture?template=L&…` URL into the unchanged `OrgProtocol.app` handler.

- **Safari Web Extension (primary).** A real toolbar button + popup (URL, editable title, note pre-filled from the page selection) replaces the bookmarklet as the main capture surface. Rationale: the user dislikes bookmarklets as a UX pattern; a toolbar button is more discoverable and lets the title/note be edited before capture. Safari is the user's primary browser, so this targets Safari directly — no cross-browser packaging.
- **Build path is opt-in.** `scripts/install-safari-extension-mac.sh` wraps the `safari-extension/` WebExtension source via `safari-web-extension-converter` + `xcodebuild` into `~/Applications/OrgCapture.app`. It requires full Xcode (Command Line Tools lacks the converter), so the script self-skips with instructions when Xcode is absent — `install.sh` can call it unconditionally without breaking CLT-only machines. Only the WebExtension source is committed; the generated Xcode project lives under the git-ignored `build/`.
- **Unsigned-extension caveat.** Ad-hoc-signed Safari extensions require **Develop → Allow Unsigned Extensions**, which Safari resets on each restart. This is an Apple constraint, fixable only with paid Developer ID signing; documented in README rather than worked around.
- **Services Shortcut (peer trigger).** A Shortcuts.app "Org Capture URL" entry runs `scripts/org-capture-url.sh`, exposing capture in the right-click **Services** menu for URLs found outside Safari (Mail, Messages, Notes). Shortcuts cannot be created from the CLI (`shortcuts` has no `create`), so its setup is a documented one-time GUI step; the helper script is the file-tracked part.
- **Dropped:** the global-hotkey Shortcut idea (user declined) and any Chrome/Brave extension variant (Safari-only focus).

## Revision 2026-05-22: adopt Scrim + Captee, retire the DIY stack

The self-built Safari Web Extension (`OrgCapture.app`), the hand-rolled `OrgProtocol.app` handler, the Xcode auto-installer, and the Services-shortcut helper are **retired**. They worked, but two costs were unacceptable for a daily driver:

- The extension was **ad-hoc signed**, so Safari required **Develop → Allow Unsigned Extensions** re-toggled after every restart. Removing that toggle needs a paid Apple Developer ID ($99/yr).
- Building the extension dragged in a **~10GB Xcode** dependency via `safari-web-extension-converter`.

**Replacement:** the **Scrim + Captee for Emacs** App Store bundle — two notarized, one-time purchases (perpetual, tied to the Apple ID, no subscription, no developer account):

- **Captee** constructs the `org-protocol://` capture request from the macOS **Share Menu** (URL + title + selection, Org or Markdown).
- **Scrim** is a notarized `org-protocol://` proxy that relays to `emacsclient`, sandbox-safe with no permission-relaxing toggles. It now **owns the `org-protocol://` scheme**, replacing `OrgProtocol.app`.

Trigger becomes **Safari Share button → Captee → Emacs** (two clicks, system-wide via the Share Menu), optionally bound to a global hotkey in System Settings.

**Kept:** the Doom `+org-protocol` flag and capture template `L` → `inbox.org`.

**One Emacs-side change forced by Scrim — TCP server.** Scrim is sandboxed and cannot reach Emacs's default unix-domain socket, so the server must run over TCP. Two edits:
- `doom/config.el`: `(setq server-use-tcp t)` before `(server-start)`. The server then writes a host/port/auth file to `~/.config/emacs/server/server` that Scrim (and emacsclient) read.
- `shell/init.zsh`: `export EMACS_SERVER_FILE="$HOME/.config/emacs/server/server"` so `EDITOR`/`e`/`et` keep working — emacsclient defaults to the unix socket, which no longer exists under TCP. Point Scrim at the same auth file in its settings.

Note: the old `OrgProtocol.app` needed no TCP (it shelled `emacsclient` over the socket). TCP is purely Scrim's sandbox tax — the price of a notarized, non-rotting handler.

**Trade-off accepted:** vendor lock-in. If Scrim/Captee are delisted or abandoned, they can bit-rot on a future macOS, with no source to patch. Mitigation: the Emacs end is ours, and this cleanup is a single commit — recovery is a `git revert`. We chose zero maintenance + a signed, sandbox-safe path over keeping a dormant DIY fallback in the tree.

**Removed files:** `safari-extension/`, `scripts/install-safari-extension-mac.sh`, `scripts/install-xcode-mac.sh`, `scripts/install-org-protocol-mac.sh`, `scripts/org-capture-url.sh`. **Added:** `scripts/install-scrim-captee-mac.sh` (opens the App Store bundle + prints setup).

## Revision 2026-05-25: pin the org-protocol handler to Scrim (Emacs Client.app conflict)

Captures via Captee were spawning a **fresh standalone Emacs** every time instead of attaching to the daemon. Root cause: emacs-plus's **`Emacs Client.app` also claims the `org-protocol://` scheme** (`CFBundleURLSchemes` in its `Info.plist`). With no explicit default handler recorded in LaunchServices, the Scrim-vs-Emacs-Client rank-tie resolved to Emacs Client.app, which ran `emacsclient` with no TCP server file (GUI apps don't inherit `EMACS_SERVER_FILE`); under `server-use-tcp t` there is **no unix socket**, so the connection failed and emacsclient's alternate-editor fallback launched a full Emacs. Scrim never ran.

Fixes:
- **Pin the scheme to Scrim** with `duti -s com.yummymelon.scrim org-protocol` (two-arg form = URL scheme). The binding is stored in LaunchServices (`com.apple.launchservices.secure` → `LSHandlers`) and survives emacs-plus upgrades, so it need not be re-applied when `Emacs Client.app` is re-copied. `install.sh` now installs `duti` (macOS); `scripts/install-scrim-captee-mac.sh` applies the pin once Scrim is present. Verified: capture lands in `inbox.org`, no stray Emacs.
- **Guard `server-start`** in `doom/config.el` with `(unless (server-running-p) ...)` so any stray second Emacs no longer collides with the daemon's server and warns.
- Note on Scrim's "Setup" file-grant: not needed in practice — the auth file is at Emacs's **default** location (`<user-emacs-directory>/server/server` = `~/.config/emacs/server/server`), which Scrim reads on its own. Setup is only required if the file is relocated.

## References

- [Captee](http://yummymelon.com/captee/) · [Scrim — Org Protocol Proxy](http://yummymelon.com/scrim/)
- [Scrim + Captee for Emacs bundle (App Store)](https://apps.apple.com/us/app-bundle/scrim-captee-for-emacs/id1810494622)

- [Emacs org-protocol manual](https://orgmode.org/worg/org-contrib/org-protocol.html)
- [Doom `org` module flags](https://docs.doomemacs.org/latest/modules/lang/org/)
- [Converting a web extension for Safari](https://developer.apple.com/documentation/safariservices/safari_web_extensions/converting_a_web_extension_for_safari)
- ADR-009 — same `lsregister` / LaunchServices pattern used for `Emacs Client.app`.
