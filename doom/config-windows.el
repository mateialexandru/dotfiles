;;; config-windows.el --- Windows-specific Doom configuration -*- lexical-binding: t; -*-

;; Use Git bash for POSIX compatibility (works with Windows paths)
(setq shell-file-name "C:/Program Files/Git/bin/bash.exe")

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
