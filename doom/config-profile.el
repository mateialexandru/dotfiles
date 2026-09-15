;;; config-profile.el --- optional private configuration layers -*- lexical-binding: t; -*-

(require 'seq)

;; The public configuration is complete on its own.  Private repositories may
;; add ordered layers by placing directory symlinks beneath this directory:
;;
;;   ~/.config/dotfiles/layers.d/10-personal -> /path/to/private/profile
;;   ~/.config/dotfiles/layers.d/20-work     -> /path/to/work/profile
;;
;; A layer may provide doom/pre.el (values needed during early startup) and
;; doom/post.el (commands and late overrides).  The public repository never
;; needs to know a private repository's name or location.

(defconst my/dotfiles-layers-directory
  (expand-file-name "~/.config/dotfiles/layers.d/")
  "Directory containing ordered private configuration layers.")

(defvar my/dotfiles-current-layer nil
  "Layer directory dynamically bound while a private file loads.")

(defun my/dotfiles-layer-directories ()
  "Return configured layer directories in lexical order."
  (when (file-directory-p my/dotfiles-layers-directory)
    (seq-filter #'file-directory-p
                (directory-files my/dotfiles-layers-directory t "^[^.].*" t))))

(defun my/dotfiles-load-layers (relative-file)
  "Load RELATIVE-FILE from each configured private layer."
  (dolist (layer (my/dotfiles-layer-directories))
    (let ((file (expand-file-name relative-file layer)))
      (when (file-readable-p file)
        (let ((my/dotfiles-current-layer (file-truename layer)))
          (load file nil 'nomessage))))))

;; Portable defaults.  A layer's doom/pre.el may replace these before the rest
;; of config.el consumes them.
(defvar my/org-directory (expand-file-name "~/Documents/org/"))
(defvar my/roam-context 'default)
(defvar my/roam-context-specs nil)
(defvar my/org-capture-inbox nil)
(defvar my/org-excalidraw-directory nil)

(my/dotfiles-load-layers "doom/pre.el")

(setq my/org-directory (file-name-as-directory
                        (expand-file-name my/org-directory))
      my/org-capture-inbox
      (expand-file-name (or my/org-capture-inbox "inbox.org")
                        my/org-directory)
      my/org-excalidraw-directory
      (file-name-as-directory
       (expand-file-name (or my/org-excalidraw-directory "excalidraw/")
                         my/org-directory)))

(unless my/roam-context-specs
  (setq my/roam-context-specs
        `((default ,(expand-file-name "roam/" my/org-directory) nil))))
