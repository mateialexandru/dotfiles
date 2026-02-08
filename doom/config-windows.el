;;; config-windows.el --- Windows-specific Doom configuration -*- lexical-binding: t; -*-

;; Use Git bash for POSIX compatibility (works with Windows paths)
(setq shell-file-name "C:/Program Files/Git/bin/bash.exe")

;; Dired: Windows lacks ls, use Emacs ls-lisp with directories first
(setq dired-use-ls-dired nil
      dired-listing-switches "-alh"
      ls-lisp-dirs-first t)
