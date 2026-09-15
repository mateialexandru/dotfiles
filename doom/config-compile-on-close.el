;;; config-compile-on-close.el --- Auto-bury compilation buffers -*- lexical-binding: t; -*-

(defvar my/compilation-auto-close t
  "When non-nil, auto-bury compilation buffers on success and
auto-navigate to the first error on failure.")

(defun my/compilation--dismiss (buf)
  "Close BUF's windows, then bury it if it still exists.
Doom's popup rule for `*compilation*' carries `(ttl . 0)', so closing the
window kills the buffer outright — re-check liveness before burying."
  (when (buffer-live-p buf)
    (delete-windows-on buf)
    (when (buffer-live-p buf)
      (bury-buffer buf))))

(defun my/compilation-auto-close-h (buf status)
  "Bury compilation buffer on success. On failure, jump to the first
error and leave the buffer on screen."
  (when my/compilation-auto-close
    (if (string-match-p "finished" status)
        ;; Success: bury after 2s
        (run-at-time 2 nil #'my/compilation--dismiss buf)
      ;; Failure: jump to first error, leave the buffer on screen
      (run-at-time 0.5 nil
                   (lambda (b)
                     (when (buffer-live-p b)
                       (with-current-buffer b
                         (goto-char (point-min)))
                       (ignore-errors (next-error 1 t))))
                   buf))))
(add-hook 'compilation-finish-functions #'my/compilation-auto-close-h)

;; Wrap long lines in compilation buffers
(add-hook 'compilation-mode-hook #'visual-line-mode)
