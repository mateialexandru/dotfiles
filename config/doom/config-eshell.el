;;; config-eshell.el --- Eshell inline-image tooling -*- lexical-binding: t; -*-

;; Inline image rendering in eshell — `cat' an image and see it, plus a
;; `rinku' link-preview command. Adapted from Álvaro Ramírez's (xenodium)
;; work on rich eshell output: https://xenodium.com
;;
;; Adaptations here make both commands TRAMP-aware by separating three
;; concerns that a remote eshell otherwise conflates:
;;   - render  : always local (Emacs GUI lives on this machine)
;;   - compute : `call-process' (always local) vs `process-file' (follows
;;               `default-directory', i.e. remote under TRAMP)
;;   - data    : read wherever the file actually lives
;;
;; `cat' is file-bound, so it honours TRAMP: it reads bytes from whatever the
;; path points at (local path -> local bytes, /ssh:host: path -> remote bytes)
;; and renders them locally via `create-image' :data. `rinku' is a network
;; fetch and host-agnostic, so it is pinned to local tooling regardless of the
;; eshell's remote `default-directory'.

(defun adviced:eshell/cat (orig-fun &rest args)
  "Like `eshell/cat' but renders images inline, local OR remote.
Reads bytes via `insert-file-contents-literally' (TRAMP-transparent) and
builds the image from `:data', so it works for remote files under TRAMP."
  (if (seq-every-p (lambda (arg)
                     (and (stringp arg)
                          (file-exists-p arg)
                          (image-supported-file-p arg)))
                   args)
      (with-temp-buffer
        (insert "\n")
        (dolist (path args)
          (let* ((data (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally (expand-file-name path))
                         (buffer-string)))
                 (spec (create-image data (image-type-from-file-name path)
                                     t :max-width 350)))
            (image-flush spec)
            (insert-image spec))
          (insert "\n"))
        (insert "\n")
        (buffer-string))
    (apply orig-fun args)))

(advice-add #'eshell/cat :around #'adviced:eshell/cat)

(defun eshell/rinku (&rest args)
  "Fetch link preview with LOCAL rinku and render the image inline.
Host-agnostic: pins `default-directory' local so the binary, its cache, and
the temp image stay on this machine even when the eshell is on a remote host.

rinku caches the preview PNG locally (~/Library/Caches/link-previews) and
prints JSON {url,title,image}; image is an absolute local path.
Flags: --no-cache --preview --width --height.

Usage: rinku https://soundcloud.com/shehackedyou
       rinku --preview https://soundcloud.com/shehackedyou"
  (unless args
    (error "rinku: no arguments provided"))
  (let* ((default-directory temporary-file-directory)
         (output (with-temp-buffer
                   (apply #'call-process "rinku" nil t nil args)
                   (buffer-string)))
         (metadata (ignore-errors (json-read-from-string output))))
    (if metadata
        (concat
         (if (map-elt metadata 'image)
             ;; Resolve the path while `default-directory' is still local, so
             ;; a remote `cat' can't misread a relative path against the host.
             (eshell/cat (expand-file-name (map-elt metadata 'image)))
           "\n")
         (when (map-elt metadata 'title)
           (concat (map-elt metadata 'title) "\n\n")))
      output)))

(provide 'config-eshell)
;;; config-eshell.el ends here
