;;; config-linux.el --- Linux-specific Doom configuration -*- lexical-binding: t; -*-

;; Dired: use native ls with directories first
(setq dired-listing-switches "-alh --group-directories-first")

;;; Notifications — D-Bus libnotify (local)
(after! alert
  (setq alert-default-style 'libnotify))
