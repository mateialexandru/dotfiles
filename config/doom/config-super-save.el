;;; config-super-save.el --- Auto-save on buffer switch and idle -*- lexical-binding: t; -*-

(defvar my/super-save-enabled t
  "When non-nil, super-save auto-saves buffers on switch and idle.")

(use-package! super-save
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5
        super-save-remote-files nil
        super-save-exclude '(".gpg"))
  (super-save-mode +1)

  ;; Don't save while apheleia is mid-format to avoid conflicts
  (add-to-list 'super-save-predicates
               (lambda () (not (bound-and-true-p apheleia--current-process))))

  ;; Respect the toggle — skip saving when disabled
  (add-to-list 'super-save-predicates
               (lambda () my/super-save-enabled)))
