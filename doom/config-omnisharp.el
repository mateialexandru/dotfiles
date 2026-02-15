;;; config-omnisharp.el --- OmniSharp contact fn for C# (cross-platform) -*- lexical-binding: t; -*-
;;
;; Defines `my/omnisharp-contact' for use by the dispatcher in config.el.
;; Always loaded on all platforms — activating OmniSharp requires no file edits:
;;   SPC t L  (or M-x my/csharp-toggle-lsp)  to switch backends live.
;;
;; Linux default: OmniSharp when installed.   Install: bash scripts/install-omnisharp.sh
;; Windows:       available for toggle when installed.  Install: scripts/install-omnisharp.ps1
;;                Binary expected at: %LOCALAPPDATA%\omnisharp\OmniSharp.exe

(after! eglot
  (let* ((binary (pcase system-type
                   ('windows-nt
                    (expand-file-name
                     (concat (getenv "LOCALAPPDATA") "/omnisharp/OmniSharp.exe")))
                   (_
                    (expand-file-name "~/.local/share/omnisharp/OmniSharp"))))
         ;; Only needed on Linux with Homebrew .NET (non-standard install prefix)
         (brew-prefix (unless (eq system-type 'windows-nt)
                        (let ((out (string-trim
                                    (shell-command-to-string "brew --prefix dotnet 2>/dev/null"))))
                          (unless (string-empty-p out) out))))
         (dotnet-root (when brew-prefix (concat brew-prefix "/libexec"))))
    (when (file-exists-p binary)
      ;; On Linux: flip the default to OmniSharp when the binary is present.
      ;; On Windows: OmniSharp is available for toggle but Roslyn remains default.
      (unless (eq system-type 'windows-nt)
        (setq my/csharp-lsp-backend 'omnisharp))
      (defun my/omnisharp-contact (&rest _)
        "Eglot contact for OmniSharp.  Supports file-creation refactorings
such as \"Move type to file\" that the standalone Roslyn LSP does not."
        (let* ((root (and (project-current) (project-root (project-current))))
               (sln  (and root (car (directory-files root t "\\.sln\\'" t))))
               (server-args (if sln
                                (list binary "-lsp" "-s" sln)
                              (list binary "-lsp"))))
          ;; Inject DOTNET_ROOT + roll-forward so the net6.0 binary runs on
          ;; whatever .NET version Homebrew has installed (Linux/macOS only).
          (if dotnet-root
              (append (list "env"
                            (concat "DOTNET_ROOT=" dotnet-root)
                            "DOTNET_ROLL_FORWARD=Major")
                      server-args)
            server-args)))
      (message "C# LSP: OmniSharp available at %s (SPC t L to toggle)" binary))))
