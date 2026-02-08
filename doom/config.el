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

  (add-hook 'compilation-filter-hook #'my/devcontainer-rewrite-paths))

;; Dired: hide details by default on all platforms
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Load platform-specific configuration
(pcase system-type
  ('gnu/linux   (load! "config-linux"))
  ('windows-nt  (load! "config-windows")))
