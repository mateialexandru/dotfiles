;;; config-macos.el --- macOS-specific Doom configuration -*- lexical-binding: t; -*-

;; Native macOS settings
(setq ns-use-native-fullscreen t
      ns-appearance 'dark
      ;; Use Command as Meta, Option as Alt/Super
      mac-command-modifier 'meta
      mac-option-modifier 'alt
      mac-right-option-modifier 'alt)

;; Glove80 navigation sends Option+arrows, matching macOS word movement.
;; Option is Alt here; Command remains Meta for existing Emacs shortcuts.
(global-set-key (kbd "A-<left>") #'backward-word)
(global-set-key (kbd "A-<right>") #'forward-word)

;; Fix path issues on macOS (ensures Emacs has the same PATH as the login shell).
;; `window-system' is nil while the launchd-started daemon boots, so gate on
;; `daemonp' too — otherwise the daemon keeps launchd's bare
;; /usr/bin:/bin:/usr/sbin:/sbin and Homebrew tooling goes missing.
;; LIBRARY_PATH rides along so libgccjit's linker finds libemutls_w.a (ADR-009).
(use-package! exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "LIBRARY_PATH")
  (when (or (daemonp) (memq window-system '(mac ns)))
    (exec-path-from-shell-initialize)))

;; Dired: macOS 'ls' doesn't support --dired
(setq dired-use-ls-dired nil)

;; Git indexing is quick locally; avoid stale project file lists after external edits.
(after! projectile
  (setq projectile-enable-caching nil))

;; Homebrew installs Symbols Nerd Font as "SymbolsNerdFontMono-Regular"
(setq doom-symbol-font (font-spec :family "SymbolsNerdFontMono-Regular"))
