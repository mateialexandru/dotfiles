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

;; Make gd use xref when TAGS available
(defun my/xref-find-definitions ()
  "Find definitions using xref (for TAGS)."
  (interactive)
  (xref-find-definitions (thing-at-point 'symbol)))

(map! :leader
      (:prefix ("c" . "code")
       :desc "Create tags" "t" #'my/create-tags))

;; Bind gd to xref in prog-mode (after evil loads)
(after! evil
  (map! :map prog-mode-map
        :n "gd" #'xref-find-definitions
        :n "gD" #'+lookup/definition))
