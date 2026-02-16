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
      (message "C# LSP: OmniSharp at %s" binary)))

  (defun my/fix-omnisharp-capabilities ()
    "Fix OmniSharp empty-object capabilities decoded as nil by plist mode.
OmniSharp returns {} for supported capabilities, which `lsp-use-plists'
decodes as nil.  Replace nil with t where the key exists in the plist."
    (when-let* ((workspace (car (lsp-workspaces)))
                ((eq (lsp--client-server-id (lsp--workspace-client workspace))
                     'omnisharp)))
      (let ((caps (lsp--workspace-server-capabilities workspace)))
        (dolist (cap '(:workspaceSymbolProvider :documentSymbolProvider
                       :hoverProvider :definitionProvider :referencesProvider
                       :documentHighlightProvider :implementationProvider
                       :typeDefinitionProvider))
          (when (and (plist-member caps cap) (not (plist-get caps cap)))
            (plist-put caps cap t))))))

  (add-hook 'lsp-after-initialize-hook #'my/fix-omnisharp-capabilities))
