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
                                        ;(setq projectile-switch-project-action #'projectile-commander)

  (def-projectile-commander-method ?m
                                   "Open magit-status."
                                   (magit-status)))

;; SPC p t → run project tests
(map! :leader
      (:prefix "p"
       :desc "Test project" "t" #'projectile-test-project))

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
