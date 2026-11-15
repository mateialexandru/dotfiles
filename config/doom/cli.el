;;; cli.el --- Windows Doom environment handling -*- lexical-binding: t; -*-

(defcli! (:root :before doom env) (&args _)
  "Keep Windows runtime selection in the launching process, not its snapshot."
  (when (eq system-type 'windows-nt)
    (dolist (regexp '("^PATH$" "^EMACS_NATIVE_COMP_ROOT$"))
      (add-to-list 'doom-env-deny regexp))))
