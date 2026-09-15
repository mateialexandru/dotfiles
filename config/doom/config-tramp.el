;;; config-tramp.el --- TRAMP performance tuning for remote editing -*- lexical-binding: t; -*-

;; The "edit lane" of the remote workflow: local GUI Emacs editing files on a
;; remote host over TRAMP. A bare remote stat is
;; ~50-100ms vs ~1ms locally, so the wins are (1) reuse the SSH connection,
;; (2) run remote subprocesses async and directly, (3) stop probing things that
;; cost a round-trip per file. Settings follow Core Dumped's "Making TRAMP go
;; Brrrr" (2025-06): https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
;;
;; Connection sharing is delegated to ~/.ssh/config (the `Host *.ts.net'
;; ControlMaster block installed by install.sh), so it benefits plain `ssh' and
;; the Ghostel->tmux helper in config-remote.el too — not just TRAMP.

(after! tramp
  (setq tramp-default-method "ssh"                 ; rides ssh ControlMaster
        tramp-use-connection-share nil             ; defer multiplexing to ~/.ssh/config
        remote-file-name-inhibit-locks t           ; we're the only writer
        tramp-use-scp-direct-remote-copying t      ; scp host->host, skip the local hop
        remote-file-name-inhibit-auto-save-visited t
        tramp-copy-size-limit (* 1024 1024)        ; inline up to 1MB, out-of-band above
        tramp-verbose 1))

;; Run remote subprocesses directly and async (the old `tramp-direct-async-process'
;; connection property is deprecated; set it via a connection-local profile).
;; This is the big win for compile/magit/projectile over TRAMP.
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'remote-direct-async-process)

;; Keep Git locally, but kill VC entirely on remote files: a global
;; `vc-handled-backends' of (Git) still shells out to `git' per directory/file
;; over TRAMP, which is the main per-visit dired stall once dirvish is off.
(setq vc-handled-backends '(Git))

(connection-local-set-profile-variables
 'remote-without-vc
 '((vc-handled-backends . nil)))

(connection-local-set-profiles
 '(:application tramp)
 'remote-without-vc)

;; Projectile's root detection walks the tree bottom-up + top-down on every
;; find-file (incl. each dired dir visit), stat-ing project markers along the
;; way. Over TRAMP each stat is an ssh round-trip — dozens per navigation, no
;; cache hit since default-directory changes — so it dominates remote dired
;; (~80% of profiled time). Short-circuit it to nil for remote paths.
(after! projectile
  (defadvice! +projectile-skip-remote-a (orig-fn &optional dir)
    "Skip projectile root resolution on remote files."
    :around #'projectile-project-root
    (unless (file-remote-p (or dir default-directory))
      (funcall orig-fn dir))))

;; diff-hl (vc-gutter +pretty) runs `git diff' per buffer; pointless over TRAMP.
(after! diff-hl
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (diff-hl-mode -1)))))

(provide 'config-tramp)
;;; config-tramp.el ends here
