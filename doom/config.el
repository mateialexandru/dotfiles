;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Source/mindmap/org/"
      org-roam-directory "~/Source/mindmap/roam/")

;; Start Emacs server for emacsclient support
(server-start)

;; EWW popup rule — open EWW in a real window, not a popup
(after! eww
  (set-popup-rule! "^\\*eww\\*" :ignore t))

;;; EWW + persp-mode session restore

;; Mark eww buffers as "real" globally (not per-buffer)
(after! doom
  (defun +my/doom-real-eww-buffer-p (buf)
    (eq (buffer-local-value 'major-mode buf) 'eww-mode))
  (add-to-list 'doom-real-buffer-functions #'+my/doom-real-eww-buffer-p))

(after! persp-mode
  (require 'eww)

  ;; 1. HELPER: Ensure new eww buffers are added to the perspective
  (defun +my/persp-add-current-buffer-to-current-persp-h ()
    (when (bound-and-true-p persp-mode)
      (persp-add-buffer (current-buffer) (get-current-persp) nil)))
  (add-hook 'eww-mode-hook #'+my/persp-add-current-buffer-to-current-persp-h)

  ;; 2. HELPER: Restore point after EWW renders
  (defvar-local +my/eww-restore-point nil)

  (defun +my/eww-restore-point-after-render-h ()
    "Hook to restore point after EWW finishes rendering the page."
    (when (integerp +my/eww-restore-point)
      (goto-char (min (point-max) (max 1 +my/eww-restore-point)))
      (setq +my/eww-restore-point nil)
      ;; Remove self from hook so it doesn't run on subsequent navigations
      (remove-hook 'eww-after-render-hook #'+my/eww-restore-point-after-render-h t)))

  ;; 3. CONFIGURATION: Define how to Save and Load
  (persp-def-buffer-save/load
   :mode 'eww-mode
   :tag-symbol 'def-eww
   :save-vars '(point) ; We rely mostly on :save-function, but this is required by the macro syntax

   ;; --- SAVE FUNCTION ---
   :save-function
   (lambda (b tag lvars)
     (with-current-buffer b
       (let ((url   (or (plist-get eww-data :url) (bound-and-true-p eww-current-url)))
             (title (or (plist-get eww-data :title) (bound-and-true-p eww-current-title)))
             (pt    (point))
             (bname (buffer-name b)))
         ;; Add custom data to lvars
         (when (stringp url)
           (push (cons 'eww-url url) lvars))
         (when (stringp title)
           (push (cons 'eww-title title) lvars))
         (push (cons 'eww-buffer-name bname) lvars)
         (push (cons 'point pt) lvars)
         ;; Return the list format expected by persp-mode
         (list tag bname lvars))))

   ;; --- LOAD FUNCTION ---
   ;; We use :load-function because it receives the full `savelist`
   :load-function
   (lambda (savelist &rest _)
     (cl-destructuring-bind (_tag buffer-name vars-list &rest _rest) savelist
       (let ((url   (alist-get 'eww-url vars-list))
             (pt    (alist-get 'point vars-list))
             (bname (alist-get 'eww-buffer-name vars-list)))

         (if (not (and (stringp url) (not (string-empty-p url))))
             ;; Fallback if no URL found: just create a basic buffer
             (get-buffer-create (or bname buffer-name))

           ;; Create/reuse the buffer and initialize EWW
           (let ((buff (get-buffer-create (or bname buffer-name))))
             (with-current-buffer buff
               (unless (eq major-mode 'eww-mode)
                 (eww-mode))

               ;; Set the point we want to restore to
               (setq-local +my/eww-restore-point (or pt 1))

               ;; Add the hook BEFORE browsing
               (add-hook 'eww-after-render-hook #'+my/eww-restore-point-after-render-h nil t)

               ;; Trigger the navigation
               ;; We use `ignore-errors` because network issues during session
               ;; restore shouldn't crash the whole perspective load.
               (ignore-errors (eww-browse-url url)))

             ;; Return the buffer object (required by persp-mode)
             buff)))))))

;; Devcontainer configuration
(after! devcontainer
  (setq devcontainer-engine 'docker)

  (defun my/devcontainer-rewrite-paths ()
    "Rewrite container workspace paths to host paths in compilation output."
    (when (bound-and-true-p devcontainer-mode)
      (let* ((project-root (or (doom-project-root) default-directory))
             (dir-name (file-name-nondirectory (directory-file-name project-root)))
             (container-path (concat "/workspaces/" dir-name "/"))
             (inhibit-read-only t))
        (save-excursion
          (goto-char compilation-filter-start)
          (while (search-forward container-path nil t)
            (replace-match (file-name-as-directory project-root) t t))))))
                                        ;(add-hook 'compilation-filter-hook #'my/devcontainer-rewrite-paths))

  ;; .NET/C# test error pattern for compilation buffer
  (after! compile
    ;; Add .NET test error pattern: "at method in /path/to/file.cs:line 123"
    ;; Pattern captures: file path and line number
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(dotnet-test
                   "^[ \t]*at .+ in \\(/[^:]+\\.cs\\):line \\([0-9]+\\)"
                   1 2))

    ;; Override patterns in each compilation buffer to avoid false matches
    (defun my/set-dotnet-compilation-patterns ()
      "Set minimal compilation patterns for .NET test output."
      (setq-local compilation-error-regexp-alist '(dotnet-test)))

    (add-hook 'compilation-mode-hook #'my/set-dotnet-compilation-patterns))

  ;; Generic auto-recompile on save (works with any project/compile command)
  ;; Fix recompile to prevent devcontainer.el double-wrapping by unwrapping before recompile
  (defun my/unwrap-docker-exec (command)
    "Extract the original command from docker exec wrapper."
    ;; Pattern: docker exec --workdir /path --user name container-id ACTUAL-COMMAND
    ;; We want to extract ACTUAL-COMMAND
    (if (string-match "^docker exec .* \\([a-f0-9]\\{12\\}\\) \\(.*\\)$" command)
        (match-string 2 command)
      command))

  (defvar-local my/original-compile-command nil
    "Store the unwrapped compile command.")

  (defun my/store-unwrapped-command (orig-fun command &optional mode name-function highlight-regexp)
    "Store the unwrapped compile command in the compilation buffer."
    (let* ((unwrapped (my/unwrap-docker-exec command))
           (result (funcall orig-fun command mode name-function highlight-regexp)))
      (when (buffer-live-p result)
        (with-current-buffer result
          (setq-local my/original-compile-command unwrapped)))
      result))

                                        ;(advice-add 'compilation-start :around #'my/store-unwrapped-command)

  (defun my/recompile-with-unwrapped-command (orig-fun &optional edit-command)
    "Recompile using the unwrapped command to prevent double-wrapping."
    (if (and (not edit-command)
             (bound-and-true-p my/original-compile-command))
        (progn
          (save-some-buffers (not compilation-ask-about-save)
                             compilation-save-buffers-predicate)
          (let ((default-directory (or compilation-directory default-directory)))
            (compile my/original-compile-command)))
      (funcall orig-fun edit-command)))

                                        ;(advice-add 'recompile :around #'my/recompile-with-unwrapped-command)

  ;; Smart project test: remembers command, only prompts with C-u
  (defvar-local my/project-test-cmd nil
    "Cached test command for this project.")

  (defun my/smart-project-test (arg)
    "Run project tests. Prompts for command only first time or with prefix ARG."
    (interactive "P")
    (let* ((default-cmd (or my/project-test-cmd
                            (projectile-test-command (projectile-project-type))))
           (cmd (if (or arg (not my/project-test-cmd))
                    (read-shell-command "Test command: " default-cmd)
                  default-cmd)))
      (setq my/project-test-cmd cmd)
      (projectile-run-compilation cmd)))

  ;; Keybindings
  (map! :leader
        :desc "Run project tests (C-u to change command)"
        "p t" #'my/smart-project-test)

;;; C# Keybindings (local leader: SPC m)
  (after! csharp-mode
    (map! :localleader
          :map csharp-mode-map
          :desc "LSP code actions (extract method/class)"
          "a" #'eglot-code-actions
          :desc "Format buffer manually"
          "f" #'+format/region-or-buffer
          :desc "Go to definition"
          "d" #'eglot-find-declaration
          :desc "Find references"
          "r" #'eglot-find-references
          :desc "Rename symbol"
          "R" #'eglot-rename
          :desc "Sharper menu (dotnet CLI)"
          "n" #'sharper-main-transient))

;;; Devcontainer Setup Command for .NET Projects
  (defun my/setup-dotnet-devcontainer ()
    "Create .devcontainer/devcontainer.json for .NET development in current project."
    (interactive)
    (let* ((project-root (or (doom-project-root) default-directory))
           (devcontainer-dir (expand-file-name ".devcontainer" project-root))
           (devcontainer-file (expand-file-name "devcontainer.json" devcontainer-dir))
           (template-file (expand-file-name "devcontainer/dotnet/devcontainer.json" doom-user-dir)))
      (when (file-exists-p devcontainer-file)
        (unless (y-or-n-p "devcontainer.json already exists. Overwrite? ")
          (user-error "Aborted")))

      ;; Verify template exists
      (unless (file-exists-p template-file)
        (user-error "Template not found: %s" template-file))

      ;; Create .devcontainer directory if needed
      (unless (file-directory-p devcontainer-dir)
        (make-directory devcontainer-dir t))

      ;; Copy template
      (copy-file template-file devcontainer-file t)

      (message "✓ Created %s from template" devcontainer-file)
      (find-file devcontainer-file)))

  ;; Add keybinding for setup command
  (map! :leader
        :desc "Setup .NET devcontainer"
        "p D" #'my/setup-dotnet-devcontainer)

  ;; Dired: hide details by default on all platforms
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; Apheleia + CSharpier (TRAMP-aware)
  (after! apheleia
    ;; Allow formatting in TRAMP buffers (run formatter on remote host)
    (setq apheleia-remote-algorithm 'remote)

    ;; Configure CSharpier formatter (supports stdin/stdout)
    ;; Use full path to ensure it's found in TRAMP containers
    (setf (alist-get 'csharpier apheleia-formatters)
          '("/home/vscode/.dotnet/tools/csharpier" "--write-stdout"))

    ;; Fallback: if using dotnet global tool wrapper
    ;; (setf (alist-get 'csharpier apheleia-formatters)
    ;;       '("dotnet" "csharpier" "--write-stdout"))

    ;; Use CSharpier for csharp-mode
    (setf (alist-get 'csharp-mode apheleia-mode-alist)
          'csharpier)

    ;; Auto-enable apheleia for C# files
    (add-hook 'csharp-mode-hook #'apheleia-mode)))

;;; Projectile Commander — action menu after switching projects
(after! projectile
  (setq projectile-switch-project-action #'projectile-commander)

  (def-projectile-commander-method ?m
                                   "Open magit-status."
                                   (magit-status)))

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
       :desc "Create tags" "t" #'my/create-tags))

;; Bind gd to Doom's +lookup (eglot -> etags -> other backends)
(after! evil
  (map! :map prog-mode-map
        :n "gd" #'+lookup/definition    ; eglot -> etags -> other backends
        :n "gD" #'xref-find-definitions)) ; direct xref (always uses etags)

;;; C# LSP backend selection — toggle with SPC t L (or M-x my/csharp-toggle-lsp)
(defvar my/csharp-lsp-backend 'roslyn
  "Active C# LSP backend. Either \\='roslyn or \\='omnisharp.
Toggle interactively with `my/csharp-toggle-lsp'.")

;;; Roslyn LSP via eglot (cross-platform)
(after! eglot
  (setq eglot-connect-timeout 120)

  ;; Roslyn workspace configuration: disable file-based programs so
  ;; Roslyn doesn't treat each .cs as a standalone program.  Enable
  ;; automatic NuGet restore so packages resolve without manual
  ;; `dotnet restore'.
  (setq-default eglot-workspace-configuration
                '(:projects\.dotnet_enable_file_based_programs :json-false
                  :projects\.dotnet_enable_automatic_restore t))

  ;; Platform-aware Roslyn contact function (defined inside let to capture paths)
  (let* ((roslyn-dll (expand-file-name
                      (pcase system-type
                        ('windows-nt (concat (getenv "LOCALAPPDATA") "/roslyn-lsp/Microsoft.CodeAnalysis.LanguageServer.dll"))
                        (_ "~/.local/share/roslyn-lsp/Microsoft.CodeAnalysis.LanguageServer.dll"))))
         (log-dir (expand-file-name "roslyn-lsp-logs" temporary-file-directory)))
    (when (file-exists-p roslyn-dll)
      (defun my/roslyn-contact (&rest _)
        "Eglot contact for the standalone Roslyn language server."
        (list "dotnet" roslyn-dll
              "--logLevel" "Information"
              "--extensionLogDirectory" log-dir
              "--stdio"))))

  ;; Single eglot-server-programs entry that dispatches to the active backend.
  ;; config-linux-omnisharp.el defines my/omnisharp-contact when OmniSharp is installed.
  (defun my/csharp-eglot-contact (&rest _)
    (pcase my/csharp-lsp-backend
      ('omnisharp (my/omnisharp-contact))
      (_          (my/roslyn-contact))))

  (setf (alist-get 'csharp-mode eglot-server-programs) #'my/csharp-eglot-contact)

  ;; Toggle between backends, reconnecting eglot in the current buffer if active
  (defun my/csharp-toggle-lsp ()
    "Toggle C# LSP backend between Roslyn and OmniSharp and reconnect."
    (interactive)
    (setq my/csharp-lsp-backend
          (if (eq my/csharp-lsp-backend 'roslyn) 'omnisharp 'roslyn))
    (message "C# LSP backend: %s" my/csharp-lsp-backend)
    (when (and (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))
      (eglot-reconnect (eglot-current-server) t)))

  ;; Toggle eglot event logging (off by default for performance)
  (defun my/eglot-toggle-events ()
    "Toggle eglot LSP event logging on/off."
    (interactive)
    (if (> (plist-get eglot-events-buffer-config :size) 0)
        (progn (setq eglot-events-buffer-config '(:size 0 :format full))
               (message "Eglot events: OFF"))
      (setq eglot-events-buffer-config '(:size 2000000 :format full))
      (message "Eglot events: ON (reconnect eglot to start capturing)")))

  ;; Extra keybindings for eglot LSP capabilities
  (map! :map eglot-mode-map
        :leader
        (:prefix ("c" . "code")
         :desc "Find type definition"  "T" #'eglot-find-typeDefinition
         :desc "Toggle inlay hints"    "h" #'eglot-inlay-hints-mode
         :desc "Organize imports"      "o" #'eglot-code-action-organize-imports
         :desc "Quick fix"             "q" #'eglot-code-action-quickfix))

  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Eglot event logging" "e" #'my/eglot-toggle-events
         :desc "C# LSP backend"      "L" #'my/csharp-toggle-lsp)))

;; Roslyn project discovery: the standalone Roslyn language server
;; (unlike VS Code's C# extension) does NOT auto-discover solutions.
;; We must send `solution/open' after connecting — same as roslyn.nvim.
;; Guard is a no-op when OmniSharp is active (it discovers .sln itself).
(defun my/eglot-roslyn-open-solution ()
  "Send solution/open to Roslyn so it loads the .sln for the project."
  (when-let* (((eq my/csharp-lsp-backend 'roslyn))
              (server (eglot-current-server))
              ((eq major-mode 'csharp-mode))
              (root (project-root (eglot--project server)))
              (sln (car (directory-files root t "\\.sln\\'" t))))
    (jsonrpc-notify server :solution/open
                    (list :solution (eglot-path-to-uri sln)))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-roslyn-open-solution)

;;; Notifications — alert.el + ntfy.sh
(use-package! alert
  :config
  ;; Custom ntfy.sh style using Emacs-native url-retrieve (async, no curl)
  (defvar my/ntfy-topic "xmachine"
    "ntfy.sh topic for Emacs notifications.")

  (alert-define-style 'ntfy
                      :title "ntfy.sh push notification"
                      :notifier (lambda (info)
                                  (let* ((title (or (plist-get info :title) "Emacs"))
                                         (body (plist-get info :message))
                                         (priority (pcase (plist-get info :severity)
                                                     ('urgent "5") ('high "4") ('moderate "3")
                                                     (_ "2")))
                                         (url-request-method "POST")
                                         (url-request-extra-headers
                                          `(("Title" . ,title)
                                            ("Priority" . ,priority)
                                            ("Tags" . "emacs")))
                                         (url-request-data (encode-coding-string body 'utf-8)))
                                    (message "[ntfy] Sending: %s — %s" title body)
                                    (url-retrieve
                                     (format "https://ntfy.sh/%s" my/ntfy-topic)
                                     (lambda (status)
                                       (if (plist-get status :error)
                                           (message "[ntfy] FAILED: %S" (plist-get status :error))
                                         (message "[ntfy] Sent OK to topic '%s'" my/ntfy-topic))
                                       (when (buffer-live-p (current-buffer))
                                         (kill-buffer (current-buffer))))
                                     nil t t))))  ; silent, inhibit-cookies

  ;; --- Toggle state ---
  (defvar my/notify-on-compilation nil
    "When non-nil, send alerts on compilation finish.")
  (defvar my/notify-use-ntfy nil
    "When non-nil, push to ntfy.sh in addition to local notifications.")

  ;; --- Compilation hook ---
  (defun my/alert-compilation-finish (buf status)
    "Send alert when compilation finishes, if enabled."
    (when my/notify-on-compilation
      (let* ((clean-status (string-trim status))
             (success (string-match-p "finished" clean-status))
             (project (or (when-let ((proj (project-current nil)))
                            (project-name proj))
                          (file-name-nondirectory
                           (directory-file-name default-directory))))
             (cmd (with-current-buffer buf
                    (bound-and-true-p compile-command)))
             (title (format "[%s] Compilation %s" project
                            (if success "succeeded" "FAILED")))
             (body (or cmd clean-status))
             (severity (if success 'normal 'high)))
        (message "[notify] %s: %s" title body)
        (alert body :title title :severity severity)
        (when my/notify-use-ntfy
          (alert body :title title :severity severity :style 'ntfy)))))

  (add-hook 'compilation-finish-functions #'my/alert-compilation-finish)

  ;; --- Transient menu: SPC t n ---
  (require 'transient)
  (transient-define-prefix my/notify-transient ()
    "Notification settings."
    [:description
     (lambda ()
       (format "Notifications  ntfy:%s  compile:%s"
               (if my/notify-use-ntfy "on" "off")
               (if my/notify-on-compilation "on" "off")))
     ("c" "Toggle compile notifications"
      (lambda () (interactive)
        (setq my/notify-on-compilation (not my/notify-on-compilation))
        (message "Compile notifications: %s" (if my/notify-on-compilation "ON" "OFF"))))
     ("n" "Toggle ntfy push"
      (lambda () (interactive)
        (setq my/notify-use-ntfy (not my/notify-use-ntfy))
        (message "ntfy push: %s" (if my/notify-use-ntfy "ON" "OFF"))))
     ("t" "Send test notification"
      (lambda () (interactive)
        (message "[notify] Sending test — local style: %s, ntfy: %s"
                 alert-default-style (if my/notify-use-ntfy "yes" "no"))
        (alert "Happy hacking!" :title "Emacs" :severity 'normal)
        (when my/notify-use-ntfy
          (alert "Happy hacking!" :title "Emacs" :severity 'normal :style 'ntfy))))
     ("q" "Quit" transient-quit-one)])

  (map! :leader
        (:prefix ("t" . "toggle")
         :desc "Notifications" "n" #'my/notify-transient)))

;; Load platform-specific configuration
(pcase system-type
  ('gnu/linux   (load! "config-linux"))
  ('windows-nt  (load! "config-windows")))
;; OmniSharp contact fn — cross-platform; defines my/omnisharp-contact if binary is present
(load! "config-omnisharp")

;; Font configuration
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 14.0)
      doom-variable-pitch-font (font-spec :family "Inter" :size 15.0)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono"))


(setq fancy-splash-image "~/Downloads/EmacsIcon.png")
