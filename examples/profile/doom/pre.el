;;; pre.el --- example early private layer -*- lexical-binding: t; -*-

;; `my/dotfiles-current-layer' is dynamically bound to this profile directory
;; while the file loads, so paths can be derived without exposing them publicly.
(let ((data-root (expand-file-name "../" my/dotfiles-current-layer)))
  (setq my/org-directory (expand-file-name "org/" data-root)
        my/roam-context 'private
        my/roam-context-specs
        `((private ,(expand-file-name "roam/" data-root) nil))
        my/org-excalidraw-directory
        (expand-file-name "excalidraw/" data-root)))
