;;; config-omnisharp.el --- OmniSharp via lsp-mode (cross-platform) -*- lexical-binding: t; -*-
;;
;; Points lsp-mode at the already-installed OmniSharp binary.
;; Install: bash scripts/install-omnisharp.sh  (Linux)
;;          scripts/install-omnisharp.ps1       (Windows)

(after! lsp-mode
  (let* ((binary (pcase system-type
                   ('windows-nt
                    (expand-file-name
                     (concat (getenv "LOCALAPPDATA") "/omnisharp/OmniSharp.exe")))
                   (_
                    (expand-file-name "~/.local/share/omnisharp/OmniSharp"))))
         ;; On Linux with Homebrew .NET, set DOTNET_ROOT so the net6.0 OmniSharp
         ;; binary can find whatever .NET version Homebrew has installed.
         (brew-prefix (unless (eq system-type 'windows-nt)
                        (let ((out (string-trim
                                    (shell-command-to-string "brew --prefix dotnet 2>/dev/null"))))
                          (unless (string-empty-p out) out))))
         (dotnet-root (when brew-prefix (concat brew-prefix "/libexec"))))
    (when (file-exists-p binary)
      (setq lsp-csharp-server-path binary)
      (when dotnet-root
        (setenv "DOTNET_ROOT" dotnet-root)
        (setenv "DOTNET_ROLL_FORWARD" "Major"))
      (message "C# LSP: OmniSharp at %s" binary))))
