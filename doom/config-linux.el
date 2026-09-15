;;; config-linux.el --- Linux-specific Doom configuration -*- lexical-binding: t; -*-

(setq doom-theme 'modus-operandi)

;; Dired: use native ls with directories first
(setq dired-listing-switches "-alh --group-directories-first")

;;; Notifications — D-Bus libnotify (local)
(after! alert
  (setq alert-default-style 'libnotify))

;; Alien indexing (fd/rg) is fast enough on Linux — disable cache to avoid stale results
(after! projectile
  (setq projectile-enable-caching nil))
(after! lsp-clangd
  (setq lsp-clients-clangd-executable "/home/linuxbrew/.linuxbrew/bin/clangd"))
