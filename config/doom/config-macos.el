;;; config-macos.el --- macOS-specific Doom configuration -*- lexical-binding: t; -*-

;; Native macOS settings
(setq ns-use-native-fullscreen t
      ns-appearance 'dark
      ;; Use Command as Meta, Option as Alt/Super
      mac-command-modifier 'meta
      mac-option-modifier 'alt
      mac-right-option-modifier 'alt)

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

;; Homebrew installs Symbols Nerd Font as "SymbolsNerdFontMono-Regular"
(setq doom-symbol-font (font-spec :family "SymbolsNerdFontMono-Regular"))
