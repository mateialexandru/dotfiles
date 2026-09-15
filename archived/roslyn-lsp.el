;;; archived/roslyn-lsp.el --- Microsoft Roslyn C# LSP configuration (shelved)
;;
;; Shelved: 2026-04-22. Roslyn works well but adds installation overhead
;; (scripts/install-roslyn-lsp.sh, separate DLL download). Not currently
;; doing active .NET work. The csharp+lsp Doom module remains in init.el
;; and will fall back to OmniSharp or whatever the module defaults to.
;;
;; To restore: run scripts/install-roslyn-lsp.sh, paste the lsp-mode
;; block back into config.el, and re-read decisions/001-emacs-packages.md
;; for the full rationale (stdio transport, URI fix, solution discovery).
;;
;; See decisions/007-archived-packages.md

;;; config.el entry:

;; ;;; C# / Roslyn LSP (Official Microsoft Engine)
;; (after! lsp-mode
;;   (let ((roslyn-dll (expand-file-name "~/.local/share/roslyn-lsp/Microsoft.CodeAnalysis.LanguageServer.dll")))
;;     (when (file-exists-p roslyn-dll)
;;       (setq lsp-csharp-server-path roslyn-dll)
;;       (setq lsp-csharp-server-command (list "dotnet" roslyn-dll "--stdio"))
;;       (message "C# LSP: Roslyn at %s" roslyn-dll))))

;;; config-windows.el — URI fix (required for Roslyn on Windows):
;; (after! eglot
;;   (aset eglot--uri-path-allowed-chars ?: t))
;;
;; Note: this fix is already present in config-windows.el for eglot generally.
;; If restoring Roslyn with lsp-mode instead of eglot, it may not apply.
