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

(setq display-line-numbers-type nil)

;; Portable defaults plus optional private layers. This runs before Org loads so
;; a layer may safely supply its directories and contexts.
(load! "config-profile")
(setq org-directory my/org-directory)

;; --- Roam context registry ---
(defvar my/roam-contexts nil "Alist of (NAME . SETTINGS).")

(defun my/org-roam-directories ()
  "Return the primary and additional roots in the active roam context."
  (delete-dups
   (seq-filter #'file-directory-p
               (cons (file-name-as-directory
                      (expand-file-name org-roam-directory))
                     my/roam-extra-directories))))

(defun my/org-roam-list-files-a (list-files-fn)
  "Return roam files from every root using LIST-FILES-FN."
  (delete-dups
   (apply #'append
          (mapcar (lambda (directory)
                    (let ((org-roam-directory directory))
                      (funcall list-files-fn)))
                  (my/org-roam-directories)))))

(defun my/org-roam-file-p-a (file-p-fn &optional file)
  "Ask FILE-P-FN whether FILE belongs to any configured roam root."
  (seq-some (lambda (directory)
              (let ((org-roam-directory directory))
                (funcall file-p-fn file)))
            (my/org-roam-directories)))

(defun my/roam-register-context (name roam-dir &optional agenda-files)
  "Register roam context NAME with ROAM-DIR and optional AGENDA-FILES."
  (setf (alist-get name my/roam-contexts)
        `((org-roam-directory  . ,roam-dir)
          (org-roam-db-location . ,(expand-file-name "org-roam.db" roam-dir))
          (org-agenda-files . ,agenda-files))))

(defun my/roam-switch-context (ctx)
  "Switch org-roam to context CTX."
  (interactive
   (list (intern (completing-read "Roam context: "
                                  (mapcar #'car my/roam-contexts) nil t))))
  (let ((settings (alist-get ctx my/roam-contexts)))
    (unless settings (user-error "Unknown context: %s" ctx))
    (setq my/roam-context ctx)
    (dolist (pair settings)
      (set (car pair) (cdr pair)))
    (org-roam-db--close)
    (message "Roam context: %s" ctx)))

(defun my/roam-toggle-context ()
  "Cycle through registered roam contexts."
  (interactive)
  (let* ((names (mapcar #'car my/roam-contexts))
         (pos (cl-position my/roam-context names))
         (next (nth (mod (1+ (or pos 0)) (length names)) names)))
    (my/roam-switch-context next)))

(dolist (spec my/roam-context-specs)
  (apply #'my/roam-register-context spec))

(after! org-roam
  ;; Org-roam has one native root. Keep captures anchored there while extending
  ;; discovery and save-time membership checks to provisioned public how-tos.
  (unless (advice-member-p #'my/org-roam-list-files-a #'org-roam-list-files)
    (advice-add #'org-roam-list-files :around #'my/org-roam-list-files-a))
  (unless (advice-member-p #'my/org-roam-file-p-a #'org-roam-file-p)
    (advice-add #'org-roam-file-p :around #'my/org-roam-file-p-a))
  (my/roam-switch-context my/roam-context)
  (org-roam-db-sync))

(map! :leader
      :desc "Toggle roam context" "t r" #'my/roam-toggle-context)

;; Consult views over the currently selected roam context. Because these read
;; `org-roam-directory' at invocation time, they follow `my/roam-switch-context'.
(use-package! consult-org-roam
  :after org-roam
  :init
  (setq consult-org-roam-grep-func #'consult-ripgrep
        consult-org-roam-buffer-narrow-key ?n
        consult-org-roam-buffer-after-buffers t
        consult-org-roam-buffer-enabled nil)
  :config
  (consult-org-roam-mode +1)
  (consult-customize consult-org-roam-forward-links :preview-key "M-.")
  (map! :map org-mode-map
        :localleader
        (:prefix ("m" . "org-roam")
         :desc "Find roam file"        "e" #'consult-org-roam-file-find
         :desc "Search roam contents"  "s" #'consult-org-roam-search
         :desc "Preview backlinks"     "b" #'consult-org-roam-backlinks
         :desc "Recursive backlinks"   "B" #'consult-org-roam-backlinks-recursive
         :desc "Preview forward links" "l" #'consult-org-roam-forward-links)))

;; Start Emacs server for emacsclient support.
;; TCP socket required so the sandboxed Scrim org-protocol proxy can connect
;; (it can't reach the default unix-domain socket). server-use-tcp writes the
;; host/port/auth file under server-auth-dir for Scrim to read.
(setq server-use-tcp t)
;; Guard so a stray second Emacs (e.g. a directly-launched Emacs.app) doesn't
;; re-run server-start and collide with the daemon's already-running server.
;; `require' server: its autoloads aren't set up this early in daemon boot, so
;; a bare `server-running-p' call throws void-function and aborts config load.
(require 'server)
(unless (server-running-p)
  (server-start))

;; --- org-capture: Safari → inbox.org via org-protocol ---
;; Captee shares a page → org-protocol://capture?template=L&url=…&title=…&body=…
;; → Scrim relays to emacsclient → this template.
(after! org
  (require 'org-protocol)
  (setq org-capture-templates
        (append (or org-capture-templates '())
                `(("L" "Link from Safari" entry
                   (file+headline ,my/org-capture-inbox "Links")
                   "* %:description\n  %:link\n  %U\n  %i"
                   :empty-lines 1
                   :immediate-finish t)))))

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

  ;; Dired: hide details by default on all platforms
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;;; TLA+ — tree-sitter editing + TLC/PlusCal compile integration
;; One-time per machine: M-x treesit-install-language-grammar RET tlaplus RET
(use-package! tla-ts-mode
  :mode "\\.tla\\'"
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(tlaplus "https://github.com/tlaplus-community/tree-sitter-tlaplus" "main")))
  :config
  (add-to-list 'treesit-load-name-override-list
               '(tla "libtree-sitter-tlaplus" "tree_sitter_tlaplus")))

(use-package! tla-tools
  :after compile
  :config
  (tla-tools-error-regexp-add))

;;; Duplicate the enclosing defun directly below the original.
;; `mark-defun' grabs the whole function via the active mode's
;; beginning-/end-of-defun, so this works in c++-mode/c++-ts-mode/elisp/etc.
;; without language-specific code. Lands point on the new copy's first line.
(defun my/duplicate-defun ()
  "Copy the enclosing defun and paste it directly below."
  (interactive)
  (let (text)
    (save-mark-and-excursion
      (mark-defun)
      (setq text (string-trim
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))))
    (end-of-defun)
    (unless (bolp) (insert "\n"))
    (insert "\n" text "\n")
    (beginning-of-defun)))

(map! :leader :desc "Duplicate defun" "c y" #'my/duplicate-defun)

;;; Apheleia + CSharpier (TRAMP-aware)
(after! apheleia
  (setq apheleia-remote-algorithm 'remote)

  ;; Override Doom's bare "csharpier" with full path (not on PATH)
  (setf (alist-get 'csharpier apheleia-formatters)
        '((if (file-remote-p default-directory)
              "/home/vscode/.dotnet/tools/csharpier"
            (expand-file-name "~/.dotnet/tools/csharpier"))
          "format" "--write-stdout"))
  (setf (alist-get 'csharp-mode apheleia-mode-alist) 'csharpier)
  (setf (alist-get 'csharp-ts-mode apheleia-mode-alist) 'csharpier))

;; SPC p t → run project tests
(map! :leader
      (:prefix "p"
       :desc "Test project" "t" #'projectile-test-project))

;;; Project TLDR — "SPC p ?" = "what do I do here again?"
;; A per-project cheatsheet for the things that aren't in the code and that a
;; stale upstream README gets wrong: the command that actually builds, the port
;; something is served on, the gotcha that cost an afternoon. Lives in the repo
;; it describes, so it travels with a worktree and is never hunted for.

(defvar my/project-tldr-files '("TLDR.org" "TLDR.md")
  "Candidate filenames, relative to the project root, holding its cheatsheet.
The first that exists is opened; the first in the list is what gets created.")

(defun my/project-tldr ()
  "Pop up this project's TLDR, offering to start one if it has none."
  (interactive)
  (let* ((root (or (projectile-project-root)
                   (user-error "Not inside a project")))
         (existing (seq-find #'file-exists-p
                             (mapcar (lambda (f) (expand-file-name f root))
                                     my/project-tldr-files)))
         (file (or existing (expand-file-name (car my/project-tldr-files) root))))
    (unless existing
      (unless (y-or-n-p (format "No TLDR in %s.  Start one? "
                                (projectile-project-name)))
        (user-error "Aborted"))
      (with-temp-file file
        (insert (format "#+title: %s — how to work here\n\n* Setup\n\n* Daily loop\n\n* Gotchas\n"
                        (projectile-project-name)))))
    (find-file file)
    (goto-char (point-min))))

;; Side window rather than a full split — it's a reference you read *while*
;; working. `:ttl nil' because this is a real file buffer, not scratch output.
(set-popup-rule! "^TLDR\\.\\(org\\|md\\)$" :side 'right :size 0.4 :select t :quit t :ttl nil)

(map! :leader
      (:prefix "p"
       :desc "Project TLDR" "?" #'my/project-tldr))

;; Feature modules
(load! "config-super-save")
(load! "config-compile-on-close")
(load! "config-notify")
(load! "config-jsonviz")

;;; SQLite browser — edit cells/rows and run ad-hoc queries in sqlite-mode
(use-package! sqlite-mode-extras
  :hook (sqlite-mode . sqlite-extras-minor-mode))

;; When Corfu auto-opens on a YASnippet trigger, TAB should expand the exact
;; snippet rather than merely select the next completion candidate.
(after! corfu
  (setq +corfu-want-tab-prefer-expand-snippets t))

;;; winpulse — flash focused window
(use-package! winpulse
  :config
  (winpulse-mode +1))

;; --- Diagrams ---
(defun my/org-diagram--slug (name &optional fallback)
  "Return a filesystem-safe slug for NAME.
Use FALLBACK when NAME is empty, or an Org UUID when FALLBACK is nil."
  (let ((slug (replace-regexp-in-string
               "\\`-+\\|-+\\'" ""
               (replace-regexp-in-string
                "[^a-z0-9]+" "-" (downcase (string-trim name))))))
    (if (string-empty-p slug) (or fallback (org-id-uuid)) slug)))

;; --- Mermaid diagrams ---
(defconst my/mermaid-config-file
  (expand-file-name "mermaid-config.json" doom-user-dir)
  "Mermaid config shared by Org and Markdown renderers.")

(defun my/org-babel-language-setup ()
  "Keep configured languages registered after Org rebuilds its Babel list."
  (setf (alist-get 'mermaid org-babel-load-languages) t
        (alist-get 'python org-babel-load-languages) t)
  (require 'ob-mermaid)
  (require 'ob-python))

(defun my/org-babel-redisplay-inline-images ()
  "Refresh inline images after a Babel result changes."
  (when (derived-mode-p 'org-mode)
    (org-redisplay-inline-images)))

(defun my/org-mermaid-execute ()
  "Execute the Mermaid source block at point and refresh its inline result."
  (interactive)
  (let ((info (org-babel-get-src-block-info 'light)))
    (unless (equal (car info) "mermaid")
      (user-error "Point is not in a Mermaid source block"))
    (org-babel-execute-src-block)
    (org-redisplay-inline-images)))

(defun my/org-mermaid-toggle-pristine-view ()
  "Toggle all Mermaid source blocks between editing and pristine views.
Only Mermaid blocks are folded; other Org source blocks are left alone."
  (interactive)
  (let* ((blocks
          (org-element-map (org-element-parse-buffer) 'src-block
            (lambda (block)
              (when (string= (org-element-property :language block) "mermaid")
                block))))
         (hide
          (seq-some
           (lambda (block)
             (let ((start
                    (save-excursion
                      (goto-char (org-element-post-affiliated block))
                      (line-end-position))))
               (not (org-fold-folded-p start 'block))))
           blocks))
         (origin (copy-marker (point)))
         (first-block (and blocks (org-element-property :begin (car blocks)))))
    (unless blocks
      (user-error "No Mermaid source blocks in this buffer"))
    (unwind-protect
        (progn
          (dolist (block blocks)
            (goto-char (org-element-property :begin block))
            (org-fold-hide-block-toggle (if hide t 'off) t block))
          (goto-char origin)
          (when (invisible-p (point))
            (goto-char first-block))
          (message "Mermaid source %s" (if hide "hidden" "visible")))
      (set-marker origin nil))))

(use-package! mermaid-mode
  :mode (("\\.mmd\\'" . mermaid-mode)
         ("\\.mermaid\\'" . mermaid-mode))
  :init
  (setq mermaid-mmdc-location (or (executable-find "mmdc") "mmdc")
        mermaid-output-format ".svg"
        mermaid-flags (format "-c %s" my/mermaid-config-file)))

(use-package! ob-mermaid
  :after org
  :init
  (setq ob-mermaid-cli-path (executable-find "mmdc")
        ob-mermaid-default-config-file my/mermaid-config-file)
  :config
  (my/org-babel-language-setup)
  ;; The mode hook repairs the registration if another package resets the
  ;; language list later in a long-lived daemon session.
  (add-hook 'org-mode-hook #'my/org-babel-language-setup)
  (add-hook 'org-babel-after-execute-hook
            #'my/org-babel-redisplay-inline-images)
  (setq org-babel-python-command "python3"))

(defun my/md-mermaid-live-apply-theme ()
  "Make live Mermaid previews follow the active Emacs frame theme."
  (let* ((raw-bg (face-background 'default nil t))
         ;; A daemon without a GUI frame reports "unspecified-bg" until its
         ;; first client frame loads a theme. Use a readable light fallback;
         ;; `doom-load-theme-hook' below refreshes it once the real theme lands.
         (bg (if (or (null raw-bg)
                     (string-prefix-p "unspecified" raw-bg))
                 "#ffffff"
               raw-bg))
         (rgb (color-values bg))
         (dark (and rgb (< (apply #'+ rgb) (* 3 32768)))))
    (setq-local md-mermaid-live-theme (if dark "dark" "default")
                md-mermaid-live-background bg)))

(defun my/md-mermaid-live-maybe-enable ()
  "Enable local Markdown Mermaid previews when their renderer is available."
  (when (and (not (file-remote-p default-directory))
             (executable-find "mmdc"))
    (require 'md-mermaid-live)
    (my/md-mermaid-live-apply-theme)
    (md-mermaid-live-mode 1)))

(defun my/md-mermaid-live-refresh-theme ()
  "Re-render active Markdown Mermaid overlays after a theme change."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p md-mermaid-live-mode)
        (my/md-mermaid-live-apply-theme)
        (md-mermaid-live-restart)))))

(defun my/mermaid-preview-window-width (buffer)
  "Return a comfortable preview width in pixels for BUFFER's windows.
Use the narrowest displayed window so one overlay fits everywhere BUFFER is
shown.  Leave a small margin for a centered, page-like presentation."
  (let ((widths
         (mapcar (lambda (window) (window-body-width window t))
                 (get-buffer-window-list buffer nil t))))
    (if widths
        (max 100 (floor (* 0.9 (apply #'min widths))))
      md-mermaid-live-width)))

(defun my/md-mermaid-live-apply-fitted-image (ov file sig)
  "Attach FILE to OV, fitted and centered, and record SIG.
This replaces `md-mermaid-live--apply-image', whose fixed 1400-pixel display
is cropped in split windows.  The cached PNG remains full resolution."
  (when (and (overlay-buffer ov) (file-exists-p file))
    (let* ((width (my/mermaid-preview-window-width (overlay-buffer ov)))
           (image (create-image file nil nil :max-width width))
           (center (propertize
                    " " 'display
                    `(space :align-to (- center (0.5 . ,image)))))
           (display (propertize " " 'display image)))
      (overlay-put ov 'after-string (concat "\n" center display "\n"))
      (overlay-put ov 'md-mermaid-sig sig)
      (overlay-put ov 'md-mermaid-file file)
      (overlay-put ov 'md-mermaid-job nil)
      (overlay-put ov 'md-mermaid-target-sig nil)
      (overlay-put ov 'md-mermaid-job-start nil)
      (overlay-put ov 'md-mermaid-pending nil)
      (overlay-put ov 'md-mermaid-attempt nil)
      (md-mermaid-live--cache-put sig file)
      t)))

(defvar my/mermaid-preview-refit-timer nil
  "Idle timer used to coalesce Mermaid preview resize events.")

(defun my/mermaid-refit-previews ()
  "Refit visible Markdown and Org previews after window geometry changes."
  (setq my/mermaid-preview-refit-timer nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       ((bound-and-true-p md-mermaid-live-mode)
        (dolist (ov md-mermaid-live--overlays)
          (when-let* ((file (overlay-get ov 'md-mermaid-file))
                      (sig (overlay-get ov 'md-mermaid-sig)))
            (my/md-mermaid-live-apply-fitted-image ov file sig))))
       ((and (derived-mode-p 'org-mode)
             (get-buffer-window-list buffer nil t))
        ;; Org calculates proportional image width from the selected window.
        ;; Select the narrowest window temporarily so the overlay fits every
        ;; place this buffer is displayed; `with-selected-window' restores it.
        (let ((window
               (car (sort (get-buffer-window-list buffer nil t)
                          (lambda (a b)
                            (< (window-body-width a t)
                               (window-body-width b t)))))))
          (with-selected-window window
            (org-redisplay-inline-images))))))))

(defun my/mermaid-schedule-preview-refit (&optional _frame)
  "Schedule one preview refit after window resizing settles."
  (when (timerp my/mermaid-preview-refit-timer)
    (cancel-timer my/mermaid-preview-refit-timer))
  (setq my/mermaid-preview-refit-timer
        (run-with-idle-timer 0.2 nil #'my/mermaid-refit-previews)))

(use-package! md-mermaid
  :commands (md-mermaid-render-current md-mermaid-transient)
  :init
  ;; Live previews are cache artifacts, not project assets. Keeping them under
  ;; Doom's cache prevents merely opening a README from dirtying its worktree.
  (setq md-mermaid-config my/mermaid-config-file
        md-mermaid-live-assets-dir
        (expand-file-name "mermaid/" doom-cache-dir)
        ;; `visible' calls `window-end' while markdown-mode hooks are still
        ;; running.  In an Emacs 30 daemon, opening a Markdown file in a new
        ;; NS client frame can re-enter redisplay there and abort Emacs in
        ;; `bidi_pop_it'.  Buffer scope avoids redisplay/window inspection;
        ;; the package's line/fence guards still bound the work.
        md-mermaid-live-scope 'buffer
        md-mermaid-live-max-procs 1
        md-mermaid-live-hide-code nil
        md-mermaid-live-hidden-placeholder
        "Mermaid source hidden — SPC m v to show"
        md-mermaid-live-debug nil)
  :config
  (advice-remove #'md-mermaid-live--apply-image
                 #'my/md-mermaid-live-apply-fitted-image)
  (advice-add #'md-mermaid-live--apply-image :override
              #'my/md-mermaid-live-apply-fitted-image)
  (map! :map markdown-mode-map
        :localleader
        :desc "Toggle Mermaid previews" "m" #'md-mermaid-live-mode
        :desc "Render visible Mermaid"  "M" #'md-mermaid-live-render-visible
        :desc "Toggle pristine view"    "v" #'md-mermaid-live-toggle-code-visibility))

(add-hook 'markdown-mode-hook #'my/md-mermaid-live-maybe-enable)
(add-hook 'doom-load-theme-hook #'my/md-mermaid-live-refresh-theme)
(add-hook 'window-size-change-functions #'my/mermaid-schedule-preview-refit)
(add-hook 'window-state-change-functions #'my/mermaid-schedule-preview-refit)

(after! org
  (setq org-image-max-width 0.9
        org-image-align 'center)
  (map! :map org-mode-map
        :localleader
        :desc "Render Mermaid block" "M" #'my/org-mermaid-execute
        :desc "Toggle pristine view" "v" #'my/org-mermaid-toggle-pristine-view))

;; --- Gnuplot ---
(use-package! gnuplot
  :mode ("\\.gp\\'" . gnuplot-mode)
  :commands (gnuplot-mode gnuplot-make-buffer))

;; Stop gnuplot inferior process from initialising the qt terminal on macOS.
;; Org-plot overrides `set term' per-plot, but the default qt init blocks/hangs
;; on first run. pngcairo is headless and fast.
(setenv "GNUTERM" "pngcairo")

;; Show inline images on org file open (drawings, exported SVGs, screenshots).
;; Refresh a changed image with `C-c C-x C-v' / `org-redisplay-inline-images'.
(setq org-startup-with-inline-images t)

;; Emacs 30's image-mode prefers ImageMagick for every format it advertises.
;; Its SVG path can collapse percentage-sized Mermaid exports to a blank 30x30
;; image even though the native librsvg renderer handles them correctly.
(after! image
  (dolist (type '(SVG SVGZ))
    (add-to-list 'imagemagick-types-inhibit type))
  (imagemagick-register-types))

;; --- Excalidraw ---
;; `excalidraw:' org links open the JSON in the Chrome PWA (File Handling API);
;; saving there triggers Emacs file notifications → excalidraw-cli → SVG.
;; The watcher and file opener below work on macOS, Windows, and Linux. See ADR-008.
(defvar my/org-excalidraw--watch-descriptor nil
  "File notification descriptor for `my/org-excalidraw-directory'.")

(defvar my/org-excalidraw--export-timers (make-hash-table :test #'equal)
  "Pending debounced Excalidraw exports, keyed by source path.")

(defun my/org-excalidraw-open-file (path)
  "Open Excalidraw file PATH with the platform's registered application."
  (setq path (expand-file-name path))
  (unless (file-exists-p path)
    (user-error "Excalidraw file does not exist: %s" path))
  (pcase system-type
    ('windows-nt
     (unless (fboundp 'w32-shell-execute)
       (user-error "This Emacs build has no Windows shell integration"))
     (w32-shell-execute "open" (convert-standard-filename path)))
    ('darwin (start-process "org-excalidraw-open" nil "open" path))
    (_ (start-process "org-excalidraw-open" nil "xdg-open" path))))

(defun my/org-excalidraw-follow (svg-path)
  "Open the editable Excalidraw source corresponding to SVG-PATH."
  (my/org-excalidraw-open-file (string-remove-suffix ".svg" svg-path)))

(defun my/org-excalidraw--refresh-previews (svg-path)
  "Redisplay Org buffers that link to SVG-PATH."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'org-mode)
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward svg-path nil t)))
        (org-redisplay-inline-images)))))

(defun my/org-excalidraw--export (path)
  "Export Excalidraw PATH to its adjacent SVG and refresh Org previews."
  (remhash path my/org-excalidraw--export-timers)
  (when (and (file-readable-p path) (executable-find "excalidraw-cli"))
    (let ((status (shell-command (org-excalidraw--shell-cmd-to-svg path))))
      (if (zerop status)
          (my/org-excalidraw--refresh-previews (concat path ".svg"))
        (message "Excalidraw export failed for %s (exit %s)" path status)))))

(defun my/org-excalidraw--handle-file-change (event)
  "Schedule an SVG export for a relevant file notification EVENT."
  (let* ((action (cadr event))
         (path (pcase action
                 ('renamed (or (cadddr event) (caddr event)))
                 ((or 'changed 'created) (caddr event)))))
    (when (and (stringp path) (string-suffix-p ".excalidraw" path t))
      ;; PWA saves can emit several events, including `changed' on Windows.
      ;; Wait for the write to settle and collapse them into one conversion.
      (setq path (expand-file-name path))
      (when-let* ((timer (gethash path my/org-excalidraw--export-timers)))
        (cancel-timer timer))
      (puthash path
               (run-at-time 0.4 nil #'my/org-excalidraw--export path)
               my/org-excalidraw--export-timers))))

(defun my/org-excalidraw--start-watcher ()
  "Start one Emacs-native watcher for the Excalidraw directory."
  (when (and (file-directory-p org-excalidraw-directory)
             (executable-find "excalidraw-cli"))
    (unless (and my/org-excalidraw--watch-descriptor
                 (file-notify-valid-p my/org-excalidraw--watch-descriptor))
      (condition-case err
          (setq my/org-excalidraw--watch-descriptor
                (file-notify-add-watch org-excalidraw-directory '(change)
                                       #'my/org-excalidraw--handle-file-change))
        (file-notify-error
         (message "Excalidraw watcher unavailable: %s"
                  (error-message-string err)))))))

(defun my/org-excalidraw-open-at-mouse (event)
  "Open the Excalidraw link beneath mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (org-open-at-point))

(defun my/org-excalidraw-preview (ov path link)
  "Preview Excalidraw SVG PATH in OV and make it clickable.
LINK is the Org link element passed to `org-link-preview-file'."
  (when (org-link-preview-file ov path link)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map image-map)
      (define-key map [mouse-1] #'my/org-excalidraw-open-at-mouse)
      (overlay-put ov 'keymap map)
      (overlay-put ov 'mouse-face 'highlight)
      (overlay-put ov 'help-echo "mouse-1: edit in Excalidraw"))
    t))

(use-package! org-excalidraw
  :after org
  :commands (org-excalidraw-create-drawing)
  :config
  (setq org-excalidraw-directory my/org-excalidraw-directory)
  ;; The package shells out to `excalidraw_export', whose reimplemented renderer
  ;; garbles bound/multi-line text. Swap in `@swiftlysingh/excalidraw-cli', which
  ;; uses the real Excalidraw `exportToSvg' → faithful layout. Same signature; the
  ;; Our file-notification handler calls this after a PWA save. See ADR-008.
  (defun org-excalidraw--shell-cmd-to-svg (path)
    "Command to convert the excalidraw file at PATH to `PATH'.svg."
    (format "excalidraw-cli convert %s --format svg --output %s"
            (shell-quote-argument path)
            (shell-quote-argument (concat path ".svg"))))
  (my/org-excalidraw--start-watcher)
  ;; Upstream's opener falls back to xdg-open on Windows, and Org 9.7+ dropped
  ;; its `:image-data-fun' preview API. Register both portable replacements.
  (org-link-set-parameters
   "excalidraw"
   :follow #'my/org-excalidraw-follow
   :preview (and (fboundp 'org-link-preview-file) #'my/org-excalidraw-preview)))

;; The upstream `org-excalidraw-create-drawing' names files by UUID. These wrap
;; it to name a drawing up front (or rename one later), so the drawings dir stays
;; browsable. Defined top-level (not in `:config') so the localleader binds work
;; before the package loads; each `require's it, which runs the `:config' above.

(defun my/org-excalidraw--unique (slug)
  "Absolute .excalidraw path for SLUG in the drawings dir, uniquified."
  (let ((path (expand-file-name (concat slug ".excalidraw") org-excalidraw-directory))
        (n 1))
    (while (file-exists-p path)
      (setq path (expand-file-name (format "%s-%d.excalidraw" slug n)
                                   org-excalidraw-directory)
            n (1+ n)))
    path))

(defun my/org-excalidraw-create-link (name)
  "Create and open a named Excalidraw drawing, returning its Org link."
  (require 'org-excalidraw)
  (unless (file-directory-p org-excalidraw-directory)
    (user-error "org-excalidraw-directory %s does not exist" org-excalidraw-directory))
  (let* ((path (my/org-excalidraw--unique (my/org-diagram--slug name)))
         (link (format "[[excalidraw:%s.svg]]" path)))
    (with-temp-file path (insert org-excalidraw-base))
    (my/org-excalidraw-open-file path)
    link))

(defun my/org-excalidraw-create-named (name)
  "Create a named Excalidraw drawing and insert its link at point."
  (interactive "sDrawing name: ")
  (insert (my/org-excalidraw-create-link name)))

(defun my/org-excalidraw-create-drawing ()
  "Create an UUID-named drawing through the portable Excalidraw workflow."
  (interactive)
  (insert (my/org-excalidraw-create-link "")))

(after! org-excalidraw
  ;; Keep the upstream command name useful while avoiding its xdg-open fallback
  ;; on Windows. The named command remains the normal UI under `SPC m D n'.
  (unless (advice-member-p #'my/org-excalidraw-create-drawing
                           'org-excalidraw-create-drawing)
    (advice-add 'org-excalidraw-create-drawing :override
                #'my/org-excalidraw-create-drawing)))

(defun my/org-excalidraw-rename (new-name)
  "Rename the excalidraw drawing linked at point to NEW-NAME.
Renames the .excalidraw and .excalidraw.svg files and rewrites the link."
  (interactive "sNew name: ")
  (require 'org-excalidraw)
  (let ((ctx (org-element-context)))
    (unless (equal (org-element-property :type ctx) "excalidraw")
      (user-error "Point is not on an excalidraw link"))
    (let* ((old-svg (org-element-property :path ctx))
           (old-excal (string-remove-suffix ".svg" old-svg))
           (new-excal (my/org-excalidraw--unique (my/org-diagram--slug new-name)))
           (new-svg (concat new-excal ".svg")))
      (when (file-exists-p old-excal) (rename-file old-excal new-excal))
      (when (file-exists-p old-svg) (rename-file old-svg new-svg))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward old-svg nil t)
          (replace-match new-svg t t)))
      (when (fboundp 'org-link-preview) (org-link-preview '(16)))
      (message "Excalidraw → %s" (file-name-nondirectory new-excal)))))

(map! :after org
      :map org-mode-map
      :localleader
      (:prefix ("D" . "diagram")
       :desc "New Excalidraw diagram"    "n" #'my/org-excalidraw-create-named
       :desc "Open diagram at point"     "o" #'org-open-at-point
       :desc "Rename diagram at point"   "r" #'my/org-excalidraw-rename))

;; Eshell inline-image tooling (cat/rinku), shared across platforms
(load! "config-eshell")

;; Doom's :term ghostel module owns package pinning and integration. Keep the
;; native module outside its package checkout so upgrades can replace Ghostel
;; safely while a daemon has the old module mapped, and use upstream binaries
;; rather than requiring an exact Zig toolchain on every machine.
(setq ghostel-module-directory (expand-file-name "ghostel/" doom-data-dir)
      ghostel-module-auto-install 'download)

;; Remote editing: TRAMP tuning + tailnet host picker / persistent tmux terminals
(load! "config-tramp")
(load! "config-remote")

;; Python: uv-owned venvs, ruff formatting/linting, pyright against the .venv
(load! "config-python")

;; LLM client: lightweight chat/rewrite plus Pi-backed full project agents
(load! "config-agent-shell")
(load! "config-gptel")

;; Load platform-specific configuration
(pcase system-type
  ('darwin      (load! "config-macos"))
  ('gnu/linux   (load! "config-linux"))
  ('windows-nt  (load! "config-windows")))

;; Unified toggle menu (references vars from modules above)
(load! "config-toggles")

;;; drag-stuff — move lines/regions with M-up / M-down
(use-package! drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (map! "M-<up>"   #'drag-stuff-up
        "M-<down>" #'drag-stuff-down))

;;; --- Font configuration (Shared) ---
;; `doom-font` is kept so Doom can set the initial face on the first frame and
;; pick its symbol-fallback font; fontaine then layers named presets on top
;; that switch at runtime (`SPC t F`) and survive across sessions.
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 14.0)
      doom-variable-pitch-font (font-spec :family "Inter" :size 15.0))

(use-package! fontaine
  :config
  (setq fontaine-latest-state-file
        (locate-user-emacs-file "fontaine-latest-state.eld"))

  ;; Heights are tenths of a point: 140 = 14pt. Presets are scenarios — each
  ;; one bundles family + size + weight + spacing for a specific way of
  ;; working. Where possible, mono is paired with a designed-together sans
  ;; (Iosevka↔Aile, FiraCode↔Fira Sans, Plex Mono↔Plex Sans) so mixed-pitch
  ;; (auto-enabled in org/markdown via Doom's `(org +pretty)' flag) renders
  ;; cleanly. Override only what differs from the `t' fallback.
  (setq fontaine-presets
        '(;; --- Coding scenarios (same size band, different flavor) ---
          (coding-clean             ; default: JBM, neutral, ligatures
           :default-family "JetBrainsMono NF"
           :default-height 125)
          (coding-dense             ; Iosevka + Iosevka Aile (paired sans)
           :default-family "Iosevka Nerd Font"
           :variable-pitch-family "Iosevka Aile"
           :default-height 130          ; +0.5pt; Iosevka is narrower than the rest
           :line-spacing 0.05)
          (coding-rounded           ; FiraCode + Fira Sans (paired sans)
           :default-family "FiraCode Nerd Font"
           :variable-pitch-family "Fira Sans"
           :default-height 125)
          (coding-italic            ; Cascadia: hand-drawn italics shine
           :default-family "CaskaydiaCove Nerd Font"
           :default-height 125
           :line-spacing 0.15)
          (coding-cursive           ; Victor Mono: ligatures + cursive italics
           :default-family "VictorMono Nerd Font"
           :default-height 125
           :line-spacing 0.1)
          (coding-modern            ; Monaspace: Neon + metrics-compatible Argon for mixed-pitch
           :default-family "MonaspiceNe Nerd Font"
           :variable-pitch-family "MonaspiceAr Nerd Font"
           :default-height 125)
          (coding-maple             ; Maple Mono NF: round, modern, ligature-heavy
           :default-family "Maple Mono NF"
           :default-height 125)
          ;; --- Prose / reading (mixed-pitch friendly, airy) ---
          (prose                    ; JBM + Inter, clean reading
           :default-family "JetBrainsMono NF"
           :default-height 145
           :variable-pitch-height 1.15
           :line-spacing 0.25)
          (prose-iosevka            ; Iosevka + Iosevka Etoile (serif companion)
           :default-family "Iosevka Nerd Font"
           :variable-pitch-family "Iosevka Etoile"
           :default-height 145
           :variable-pitch-height 1.15
           :line-spacing 0.25)
          (prose-plex               ; Plex Mono + Plex Sans, distinctive
           :default-family "IBM Plex Mono"
           :default-height 140
           :variable-pitch-family "IBM Plex Sans"
           :variable-pitch-height 1.15
           :line-spacing 0.25)
          ;; --- Situational ---
          (small                    ; laptop / narrow windows / 11pt
           :default-height 110)
          (presentation             ; screensharing, projector
           :default-weight medium
           :default-height 220)
          (tired-eyes               ; lighter weight + more space, less retinal load
           :default-weight semilight
           :default-height 165
           :bold-weight bold
           :line-spacing 0.2)
          ;; --- Shared fallback ---
          (t
           :default-family "JetBrainsMono NF"
           :default-weight regular
           :default-slant normal
           :default-height 140

           :fixed-pitch-family nil          ; nil → inherit `default'
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Inter"
           :variable-pitch-weight regular
           :variable-pitch-height 1.05

           :mode-line-active-height 0.95
           :mode-line-inactive-height 0.95
           :header-line-height 1.0
           :line-number-height 0.9
           :tab-bar-height 1.0
           :tab-line-height 1.0

           :bold-weight bold
           :italic-slant italic
           :line-spacing 0.1)))

  (fontaine-mode 1)
  ;; Restore the last preset, but defend against stale state pointing at a
  ;; preset we've since renamed or removed.
  (let* ((saved  (ignore-errors (fontaine-restore-latest-preset)))
         (preset (if (and saved (assq saved fontaine-presets))
                     saved
                   'coding-clean)))
    (fontaine-set-preset preset)))

(defun my/fontaine--preset-annotation (name-str)
  "Annotate fontaine preset NAME-STR with its family, size, and paired sans."
  (let* ((preset   (cdr (assq (intern name-str) fontaine-presets)))
         (fallback (cdr (assq t fontaine-presets)))
         (family   (or (plist-get preset :default-family)
                       (plist-get fallback :default-family)))
         (height   (or (plist-get preset :default-height)
                       (plist-get fallback :default-height)))
         (vp       (plist-get preset :variable-pitch-family)))
    (concat
     "  "
     (format "%-26s · %4spt"
             (or family "")
             (if height (number-to-string (/ height 10.0)) "?"))
     (when vp (concat "  + " vp)))))

(defvar my/fontaine-preset-history nil
  "MRU list of fontaine presets, most recent first.
Updated only on explicit commits (preview scrolls don't pollute it).")

(defun my/fontaine--push-history (preset)
  "Push PRESET onto `my/fontaine-preset-history', dedup, cap at 5."
  (when preset
    (setq my/fontaine-preset-history
          (cons preset (delq preset my/fontaine-preset-history)))
    (when (> (length my/fontaine-preset-history) 5)
      (setq my/fontaine-preset-history
            (seq-take my/fontaine-preset-history 5)))))

(defun my/fontaine--announce (preset)
  "Echo PRESET name plus its family/size annotation in the minibuffer."
  (when preset
    (let ((annot (string-trim (my/fontaine--preset-annotation
                               (symbol-name preset)))))
      (message "Font: %s — %s" preset annot))))

(defun my/fontaine-set-preset ()
  "Pick a fontaine preset with consult-style live preview.
Each candidate is applied as you move through the list; abort with
`C-g' restores the preset that was active before the picker opened.
On commit, the previously-active preset is recorded so `\\[my/fontaine-toggle-recent]'
can flip back instantly."
  (interactive)
  (require 'consult nil t)
  (let* ((presets (mapcar #'car
                          (cl-remove-if (lambda (p) (eq (car p) t))
                                        fontaine-presets)))
         (candidates (mapcar #'symbol-name presets))
         (original (or (and (boundp 'fontaine-current-preset)
                            fontaine-current-preset)
                       (car presets)))
         (selected
          (if (fboundp 'consult--read)
              (consult--read
               candidates
               :prompt "Preset: "
               :require-match t
               :sort nil
               :annotate #'my/fontaine--preset-annotation
               :state
               (lambda (action cand)
                 (pcase action
                   ('preview
                    (fontaine-set-preset
                     (or (and cand (intern cand)) original)))
                   ('return
                    (let ((target (or (and cand (intern cand)) original)))
                      (fontaine-set-preset target)
                      ;; Only announce on real commit, not on C-g abort.
                      (when cand (my/fontaine--announce target)))))))
            ;; consult missing — degrade to plain completing-read, no preview.
            (let ((completion-extra-properties
                   `(:annotation-function ,#'my/fontaine--preset-annotation)))
              (completing-read "Preset: " candidates nil t)))))
    (when (and selected (not (eq (intern selected) original)))
      (unless (fboundp 'consult--read)
        (fontaine-set-preset (intern selected))
        (my/fontaine--announce (intern selected)))
      (my/fontaine--push-history original))))

(defun my/fontaine-toggle-recent ()
  "Flip to the most recently-active fontaine preset.
Calling repeatedly ping-pongs between the current and previous preset."
  (interactive)
  (if-let ((prev (car my/fontaine-preset-history))
           (current (and (boundp 'fontaine-current-preset)
                         fontaine-current-preset)))
      (progn
        (fontaine-set-preset prev)
        (setq my/fontaine-preset-history
              (cons current (delq prev my/fontaine-preset-history)))
        (my/fontaine--announce prev))
    (user-error "No previous fontaine preset to flip to")))

(map! :leader
      :desc "Toggle recent font preset"  "t f" #'my/fontaine-toggle-recent
      :desc "Set font preset (fontaine)" "t F" #'my/fontaine-set-preset)

;; Render prose buffers in the preset's `:variable-pitch-family' while keeping
;; code blocks, tables, and inline code monospaced via the `fixed-pitch' face.
(use-package! mixed-pitch
  :hook ((org-mode markdown-mode) . mixed-pitch-mode))

;; Constrain those same prose buffers to a readable measure — mixed-pitch picks
;; the face, olivetti picks the column width. Olivetti clamps the body to
;; `(max minimum-body-width (min body-width window-width))', so a window narrower
;; than `olivetti-body-width' gets no margins at all rather than being squeezed
;; further; `olivetti-minimum-body-width' is deliberately left at its default 40
;; for that reason. Toggle per buffer with `SPC t o'.
(use-package! olivetti
  :hook ((org-mode markdown-mode) . olivetti-mode)
  :init (setq olivetti-body-width 92))

(map! :leader
      (:prefix "t"
       :desc "Olivetti (centered prose)" "o" #'olivetti-mode))

(use-package! spacious-padding
  :config
  (spacious-padding-mode 1))

;;; --- Graceful daemon restart (driven by `sys restart`) ---
;; Called over emacsclient by the compiled sys CLI. Runs entirely inside
;; Emacs so it can (a) warn via a native OS notification, (b) save everything without
;; prompting, and (c) tear the daemon down cleanly — binding the confirm/persp vars
;; that would otherwise pop an interactive prompt (e.g. persp auto-save when no
;; workspace exists, or "Active processes exist"). launchd (KeepAlive) then respawns
;; a fresh daemon.

(defun my/os-notify (title message)
  "Post a native desktop notification, falling back to `alert' then the echo area."
  (cond
   ((eq system-type 'darwin)
    (call-process "osascript" nil 0 nil "-e"
                  (format "display notification %S with title %S sound name \"Submarine\""
                          message title)))
   ((and (eq system-type 'gnu/linux) (executable-find "notify-send"))
    (call-process "notify-send" nil 0 nil title message))
   ((fboundp 'alert) (alert message :title title))
   (t (message "%s: %s" title message))))

(defun my/graceful-restart (&optional delay)
  "Warn, wait DELAY seconds, save everything, then kill this daemon cleanly.
Returns immediately (schedules a timer); launchd brings up a fresh daemon."
  (let ((delay (or delay 10)))
    (my/os-notify "Doom Emacs"
                  (format "Daemon restarting in %ds — save now if you need to." delay))
    (message "Doom: daemon restarting in %ds…" delay)
    (run-with-timer
     delay nil
     (lambda ()
       (save-some-buffers t)
       (when (bound-and-true-p recentf-mode) (ignore-errors (recentf-save-list)))
       (when (bound-and-true-p savehist-mode) (ignore-errors (savehist-save)))
       (when (bound-and-true-p desktop-save-mode)
         (ignore-errors (desktop-save-in-desktop-dir)))
       (let ((confirm-kill-processes nil)
             (confirm-kill-emacs nil)
             (kill-emacs-query-functions nil)
             (persp-auto-save-opt 0))
         (kill-emacs 0))))
    (format "restart scheduled in %ds" delay)))

;; Private layers may add commands and late overrides after the public config is
;; fully defined. See config-profile.el.
(my/dotfiles-load-layers "doom/post.el")
