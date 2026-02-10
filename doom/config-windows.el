;;; config-windows.el --- Windows-specific Doom configuration -*- lexical-binding: t; -*-

;; Use Git bash for POSIX compatibility (works with Windows paths)
(setq shell-file-name "C:/Program Files/Git/bin/bash.exe")

;; Dired: Windows lacks ls, use Emacs ls-lisp with directories first
(setq dired-use-ls-dired nil
      dired-listing-switches "-alh"
      ls-lisp-dirs-first t)

;;; Roslyn LSP via eglot (Windows-only — Linux uses devcontainers)
(after! eglot
  (setq eglot-connect-timeout 120)

  ;; Fix Windows drive letter encoding: eglot encodes ":" as "%3A" in
  ;; file URIs (github#639), but Roslyn fails to parse the decoded path.
  ;; Allow colons so URIs use file:///c:/path (not file:///c%3A/path).
  (aset eglot--uri-path-allowed-chars ?: t)

  ;; Roslyn workspace configuration: disable file-based programs so
  ;; Roslyn doesn't treat each .cs as a standalone program.  Enable
  ;; automatic NuGet restore so packages resolve without manual
  ;; `dotnet restore'.
  (setq-default eglot-workspace-configuration
                '(:projects\.dotnet_enable_file_based_programs :json-false
                  :projects\.dotnet_enable_automatic_restore t))

  ;; Roslyn language server via stdio (--stdio flag, added in v5.0.0)
  (let* ((roslyn-dll (expand-file-name
                      "roslyn-lsp/Microsoft.CodeAnalysis.LanguageServer.dll"
                      (getenv "LOCALAPPDATA")))
         (log-dir (expand-file-name "roslyn-lsp-logs" temporary-file-directory)))
    (when (file-exists-p roslyn-dll)
      (add-to-list 'eglot-server-programs
                   `(csharp-mode . ("dotnet" ,roslyn-dll
                                    "--logLevel" "Information"
                                    "--extensionLogDirectory" ,log-dir
                                    "--stdio")))))

  ;; Extra keybindings for Roslyn LSP capabilities
  (map! :map eglot-mode-map
        :leader
        (:prefix ("c" . "code")
         :desc "Find type definition"  "T" #'eglot-find-typeDefinition
         :desc "Toggle inlay hints"    "h" #'eglot-inlay-hints-mode
         :desc "Organize imports"      "o" #'eglot-code-action-organize-imports
         :desc "Quick fix"             "q" #'eglot-code-action-quickfix)))

;; Roslyn project discovery: the standalone Roslyn language server
;; (unlike VS Code's C# extension) does NOT auto-discover solutions.
;; We must send `solution/open' after connecting — same as roslyn.nvim.
(defun my/eglot-roslyn-open-solution ()
  "Send solution/open to Roslyn so it loads the .sln for the project."
  (when-let* ((server (eglot-current-server))
              ((eq major-mode 'csharp-mode))
              (root (project-root (eglot--project server)))
              (sln (car (directory-files root t "\\.sln\\'" t))))
    (jsonrpc-notify server :solution/open
                    (list :solution (eglot-path-to-uri sln)))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-roslyn-open-solution)

;;; DotNet LSP diagnostics
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
