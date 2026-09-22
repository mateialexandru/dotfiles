;;; config-devdocs.el --- DevDocs integration -*- lexical-binding: t; -*-

;; DevDocs data is machine-local and downloaded on demand.  Keep it in Doom's
;; profile data rather than in the dotfiles checkout.
(use-package! devdocs
  :commands (devdocs-install devdocs-lookup devdocs-peruse devdocs-update-all)
  :init
  (setq devdocs-data-dir (doom-profile-data-dir t "devdocs/")
        devdocs-window-select t)
  :config
  ;; DevDocs is a dedicated reference pane at the top of the frame.  The side
  ;; window remains reserved for DevDocs instead of being reused by popups.
  (add-to-list
   'display-buffer-alist
   '("\\*devdocs\\*\\'"
     (display-buffer-in-side-window)
     (side . top)
     (slot . 0)
     (window-height . 0.35)
     (dedicated . t)
     (inhibit-same-window . t)))
  ;; Keep K useful after entering a DevDocs buffer: search the current
  ;; document instead of falling through to Doom's provider picker.
  (map! :map devdocs-mode-map :n "K" #'devdocs-lookup))

;; Map major modes to DevDocs slugs.  Versioned documents (notably Python) are
;; matched by prefix, so installing python~3.13 or python~3.14 both works.
(defconst my/devdocs-mode-docs
  '((c-mode              "c")
    (c-ts-mode           "c")
    (c++-mode            "cpp")
    (c++-ts-mode         "cpp")
    (cmake-mode          "cmake")
    (css-mode            "css")
    (css-ts-mode         "css")
    (dockerfile-mode     "docker")
    (html-mode           "html")
    (html-ts-mode        "html")
    (js-mode             "javascript")
    (js-ts-mode          "javascript")
    (js2-mode            "javascript")
    (markdown-mode       "markdown")
    (python-mode         "python")
    (python-ts-mode      "python")
    (rust-mode           "rust")
    (rust-ts-mode        "rust")
    (sh-mode             "bash")
    (bash-ts-mode        "bash")
    (typescript-mode     "typescript")
    (typescript-ts-mode  "typescript")
    (web-mode            "html" "css" "javascript"))
  "Alist of major modes and their DevDocs document slug prefixes.")

(defun my/devdocs--installed-slugs (prefix)
  "Return installed DevDocs slugs matching PREFIX."
  (when (file-directory-p devdocs-data-dir)
    (seq-filter
     (lambda (slug)
       (and (or (string= slug prefix)
                (string-prefix-p (concat prefix "~") slug))
            (file-readable-p (expand-file-name "metadata"
                                               (expand-file-name slug devdocs-data-dir)))))
     (directory-files devdocs-data-dir nil "^[^.]"))))

(defun my/devdocs-set-current-docs ()
  "Set the installed DevDocs documents relevant to the current major mode."
  (when-let ((prefixes (cdr (assq major-mode my/devdocs-mode-docs))))
    (setq-local devdocs-current-docs
                (delete-dups (mapcan #'my/devdocs--installed-slugs prefixes)))))

(defun my/devdocs-lookup (identifier)
  "Look up IDENTIFIER in an installed DevDocs document for this buffer.
Return nil when no matching document is installed so Doom can try its next
  documentation backend."
  (when (require 'devdocs nil t)
    (when (null devdocs-current-docs)
      (my/devdocs-set-current-docs))
    (when devdocs-current-docs
      (devdocs-lookup nil identifier)
      t)))

;; Keep Doom's LSP/major-mode documentation handlers ahead of DevDocs.  This
;; backend is the offline fallback before Doom's generic online lookup.
(add-hook '+lookup-documentation-functions #'my/devdocs-lookup)

(dolist (mode (mapcar #'car my/devdocs-mode-docs))
  (add-hook (intern (format "%s-hook" mode)) #'my/devdocs-set-current-docs))

;; For C++, K means a context search in cppreference.  LSP hover remains
;; available from the language's existing `SPC c h' binding.
(after! cc-mode
  (map! :map c++-mode-map :n "K" #'devdocs-lookup))
(after! c-ts-mode
  (map! :map c++-ts-mode-map :n "K" #'devdocs-lookup))

(map! :leader
      (:prefix ("h" . "help")
       :desc "DevDocs lookup"  "D" #'devdocs-lookup
       :desc "Install DevDocs" "I" #'devdocs-install
       :desc "Update DevDocs"  "U" #'devdocs-update-all
       :desc "Browse DevDocs"  "P" #'devdocs-peruse))

;;; config-devdocs.el ends here
