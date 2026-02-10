;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-horizon)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Windows: Use Git bash for POSIX compatibility (works with Windows paths)
;;(setq shell-file-name "C:/Program Files/Git/bin/bash.exe")

;; Font configuration
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 14)
      doom-variable-pitch-font (font-spec :family "Segoe UI" :size 13)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))

;; Start Emacs server for emacsclient support (context menu integration)
(server-start)

;; Dired configuration
(setq dired-use-ls-dired nil              ; Use Emacs lisp for ls (Windows compatible)
      dired-listing-switches "-alh")      ; Human-readable sizes
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; Tags - Universal Ctags + built-in xref
;; Generate TAGS file at project root
(defun my/create-tags ()
  "Create TAGS file at project root using Universal Ctags."
  (interactive)
  (let* ((root (or (projectile-project-root) default-directory))
         (default-directory root)  ; compile runs from here
         (tags-file (expand-file-name "TAGS" root)))
    (compile "ctags -e -R -f TAGS .")  ; use relative paths (Windows-safe)
    ;; Visit TAGS after compilation finishes
    (add-hook 'compilation-finish-functions
              (lambda (_buf _status)
                (when (file-exists-p tags-file)
                  (visit-tags-table tags-file)
                  (message "Loaded TAGS from %s" tags-file)))
              nil t)))

;; Auto-visit TAGS file when found in project
(defun my/visit-project-tags ()
  "Visit TAGS file in project root if it exists."
  (when-let* ((root (projectile-project-root))
              (tags-file (expand-file-name "TAGS" root))
              ((file-exists-p tags-file)))
    (visit-tags-table tags-file t)))

(add-hook 'find-file-hook #'my/visit-project-tags)

;; Don't prompt when switching TAGS tables
(setq tags-add-tables nil)

;; Add etags to Doom's +lookup backends
(after! xref
  (add-to-list 'xref-backend-functions #'etags--xref-backend t))

(map! :leader
      (:prefix ("c" . "code")
       :desc "Create tags" "t" #'my/create-tags
       :desc "Test DotNet LSP" "R" #'my/test-dotnet-lsp))

;; Bind gd to Doom's +lookup (eglot -> etags -> other backends)
(after! evil
  (map! :map prog-mode-map
        :n "gd" #'+lookup/definition    ; eglot -> etags -> other backends
        :n "gD" #'xref-find-definitions)) ; direct xref (always uses etags)

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

;;; LSP - eglot configuration
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
