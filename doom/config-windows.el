;;; config-windows.el --- Windows-specific Doom configuration -*- lexical-binding: t; -*-

;; Font configuration (DPI-aware float sizes)
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 11.0)
      doom-variable-pitch-font (font-spec :family "Segoe UI" :size 12.0)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))

;; Use Git bash for POSIX compatibility (works with Windows paths)
(setq shell-file-name "C:/Program Files/Git/bin/bash.exe")

;; Windows variable-pitch font (overrides Cantarell from shared config)
(setq doom-variable-pitch-font (font-spec :family "Segoe UI" :size 15))

;; Dired: Windows lacks ls, use Emacs ls-lisp with directories first
(setq dired-use-ls-dired nil
      dired-listing-switches "-alh"
      ls-lisp-dirs-first t)

;; Fix Windows drive letter encoding in eglot URIs
(after! eglot
  (aset eglot--uri-path-allowed-chars ?: t))

;;; DotNet LSP diagnostics (Windows-only: uses PowerShell)
(defun my/test-dotnet-lsp ()
  "Run DotNet LSP diagnostics for the current project."
  (interactive)
  (let* ((script (expand-file-name "../scripts/Test-DotNetLsp.ps1" (file-truename doom-user-dir)))
         (dir (if buffer-file-name
                  (file-name-directory (file-truename buffer-file-name))
                default-directory))
         (default-directory dir))
    (compile (format "powershell -NoProfile -ExecutionPolicy Bypass -File \"%s\" -Path \"%s\""
                     script dir))))

(map! :leader
      (:prefix ("c" . "code")
       :desc "Test DotNet LSP" "R" #'my/test-dotnet-lsp))

;;; Notifications — Windows toast (local)
(use-package! alert-toast
  :after alert
  :config
  (setq alert-default-style 'toast))

;;; Magit — performance tuning for Windows
;; Windows is ~56x slower than Unix for magit due to process spawning cost.
(after! magit
  ;; Don't auto-refresh status buffer on every magit action — only refresh
  ;; when the status buffer is the current buffer. Biggest single win.
  (setq magit-refresh-status-buffer nil)

  ;; Disable expensive diff decorations
  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil)

  ;; Remove slow status sections (each spawns a git subprocess)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))
