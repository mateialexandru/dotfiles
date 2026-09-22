;;; config-remote.el --- Tailnet host picker + persistent remote terminals -*- lexical-binding: t; -*-

;; The "run lane" of the remote workflow. Pick a live tailnet host, then open
;; either dired on it (edit lane, via TRAMP — see config-tramp.el) or a Ghostel
;; attached to a persistent tmux session there.
;;
;; tmux is the durable thing: it lives on the remote host and survives
;; disconnect, sleep, and network changes, and owns the scrollback. The Ghostel
;; buffer is throwaway transport — kill it (or `C-b d') and the session keeps
;; running; re-invoking reattaches to identical state via `tmux new -A'.
;;
;; Hosts come from the tailnet itself (`tailscale status --json'), so there is
;; no host list to maintain: a device that joins the tailnet just shows up in
;; the picker, and the `Host *.ts.net' ControlMaster block in ~/.ssh/config
;; (installed by install.sh) makes ssh to it fast.
;;
;; First time we open a remote terminal on a host we offer to install the shared
;; tmux.conf (remote/tmux.conf) onto it, remembering the answer so we ask once.

;;; --- Tailnet host discovery ----------------------------------------------

(defun my/tailscale-bin ()
  "Return the path to the Tailscale CLI, or nil if not found."
  (or (executable-find "tailscale")
      (let ((mac "/Applications/Tailscale.app/Contents/MacOS/Tailscale"))
        (and (file-executable-p mac) mac))))

(defun my/tailscale-hosts ()
  "Return online tailnet host FQDNs (trailing dot stripped).
Reads `tailscale status --json' and collects `Self' plus every online peer."
  (let ((bin (my/tailscale-bin)))
    (unless bin (user-error "Tailscale CLI not found"))
    (with-temp-buffer
      (unless (zerop (call-process bin nil t nil "status" "--json"))
        (user-error "tailscale status failed: %s" (string-trim (buffer-string))))
      (goto-char (point-min))
      (let* ((data  (json-parse-buffer :object-type 'alist))
             (peer  (alist-get 'Peer data))
             (nodes (append (list (alist-get 'Self data))
                            (when (consp peer) (mapcar #'cdr peer))))
             hosts)
        (dolist (n nodes (nreverse hosts))
          (when (and n (eq (alist-get 'Online n) t))
            (let ((dns (alist-get 'DNSName n)))
              (when (and (stringp dns) (> (length dns) 0))
                (push (string-remove-suffix "." dns) hosts)))))))))

(defun my/remote-pick-host ()
  "Prompt for a tailnet host FQDN."
  (completing-read "Tailnet host: " (my/tailscale-hosts) nil t))

;;; --- tmux.conf provisioning -----------------------------------------------

(defvar my/remote-tmux-config
  (expand-file-name "remote/tmux.conf" doom-user-dir)
  "Local path to the tmux.conf shipped to remote hosts.")

(defvar my/remote-seen-file
  (expand-file-name "remote-seen-hosts.el" doom-cache-dir)
  "File persisting hosts we've already offered tmux.conf to (so we ask once).")

(defun my/remote--seen-hosts ()
  "Return the persisted list of hosts already offered provisioning."
  (when (file-exists-p my/remote-seen-file)
    (with-temp-buffer
      (insert-file-contents my/remote-seen-file)
      (ignore-errors (read (current-buffer))))))

(defun my/remote--mark-seen (host)
  "Record HOST as having been offered provisioning."
  (let ((hosts (delete-dups (cons host (my/remote--seen-hosts)))))
    (with-temp-file my/remote-seen-file
      (prin1 hosts (current-buffer)))))

;;;###autoload
(defun my/remote-provision-tmux (host)
  "Copy the shared tmux.conf to HOST's ~/.tmux.conf over TRAMP."
  (interactive (list (my/remote-pick-host)))
  (unless (file-exists-p my/remote-tmux-config)
    (user-error "Missing tmux.conf at %s" my/remote-tmux-config))
  (copy-file my/remote-tmux-config (format "/ssh:%s:~/.tmux.conf" host) t)
  (my/remote--mark-seen host)
  (message "Installed tmux.conf on %s" host))

(defun my/remote--maybe-offer-provision (host)
  "Offer to install tmux.conf on HOST the first time we connect to it."
  (unless (member host (my/remote--seen-hosts))
    (if (y-or-n-p (format "Set up tmux config on %s? " host))
        (my/remote-provision-tmux host)
      (my/remote--mark-seen host))))

;;; --- Entry points ---------------------------------------------------------

;;;###autoload
(defun my/pi-spawn (&optional directory session-name prompt)
  "Open a fresh Pi session in a new Ghostel buffer.
The session starts in DIRECTORY, defaulting to the current buffer's
`default-directory'.  SESSION-NAME is passed to Pi as its display name;
when omitted, it is derived from the directory name.  PROMPT, when non-empty,
is submitted as Pi's initial prompt.

Interactively, `C-c P' in a Ghostel buffer uses the current directory and
starts an empty session immediately; `C-c C-P' prompts for an initial
instruction.  With a prefix argument it also prompts when invoked through
`M-x'.  This is deliberately a narrow launcher: it starts
`pi' with the existing layered configuration, rather than accepting an
arbitrary command.

The child process is independent of the current Pi session; no conversation
history is copied."
  (interactive
   (list default-directory nil
         (when current-prefix-arg
           (read-string "Initial Pi prompt (optional): "))))
  (require 'ghostel)
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (_ (unless (file-directory-p directory)
              (user-error "Pi working directory does not exist: %s" directory)))
         (project-name (file-name-nondirectory
                        (directory-file-name directory)))
         (session-name (or (and (stringp session-name)
                                (unless (string-empty-p session-name)
                                  session-name))
                           (format "Pi · %s" project-name)))
         (buffer-name (format "*pi %s*" project-name))
         (args (append (list "--name" session-name)
                       (when (and (stringp prompt)
                                  (not (string-empty-p prompt)))
                         (list "--" prompt))))
         (buffer (generate-new-buffer buffer-name)))
    ;; Ghostel buffers may be displayed in dedicated windows.  Use an explicit
    ;; popup action rather than `pop-to-buffer': the latter can leave the new
    ;; buffer displayed in both the source and target windows without creating
    ;; a second process.
    (with-current-buffer buffer
      (setq default-directory directory))
    (let ((window (display-buffer
                   buffer
                   '((display-buffer-pop-up-window)
                     (inhibit-same-window . t)))))
      (unless (window-live-p window)
        (user-error "Could not display the new Pi Ghostel buffer"))
      (select-window window)
      (with-current-buffer buffer
        (ghostel-exec
         buffer "pi" args
         `((kind . pi)
           (directory . ,directory)
           (session-name . ,session-name)
           (command . ("pi" ,@args))))))
    buffer))

;;;###autoload
(defun my/pi-spawn-with-prompt ()
  "Open a fresh Pi session after prompting for its initial instruction.
This is the prompted companion to `my/pi-spawn' for Ghostel's `C-c C-P'
binding."
  (interactive)
  (my/pi-spawn default-directory nil
               (read-string "Initial Pi prompt (optional): ")))

;;;###autoload
(defun my/ghostel-spawn ()
  "Start a fresh Ghostel terminal in the current directory.
Unlike `+ghostel/toggle', this always creates a new terminal buffer."
  (interactive)
  (require 'ghostel)
  ;; A non-numeric argument is Ghostel's documented "new instance" form.
  (ghostel t))

;;;###autoload
(defun my/remote--sessions (host)
  "Best-effort list of existing tmux session names on HOST."
  (ignore-errors
    (with-temp-buffer
      (when (zerop (call-process "ssh" nil t nil "-o" "BatchMode=yes" host
                                 "tmux" "list-sessions" "-F" "#{session_name}"))
        (split-string (buffer-string) "\n" t)))))

;;;###autoload
(defun my/remote-tmux (host session)
  "Open Ghostel attached to (or creating) tmux SESSION on HOST.
Uses `tmux new-session -A' so the same session is reattached every time.
Offers to install the shared tmux.conf the first time HOST is seen."
  (interactive
   (let ((host (my/remote-pick-host)))
     (list host
           (completing-read "tmux session: " (my/remote--sessions host)
                            nil nil nil nil "main"))))
  (require 'ghostel)
  (my/remote--maybe-offer-provision host)
  (let* ((name (format "*tmux %s:%s*" host session))
         (existing (get-buffer name))
         (live (and existing
                    (with-current-buffer existing
                      (and (derived-mode-p 'ghostel-mode)
                           (bound-and-true-p ghostel--process)
                           (process-live-p ghostel--process))))))
    (if live
        (pop-to-buffer existing)
      (let* ((buffer (if (and existing
                              (with-current-buffer existing
                                (derived-mode-p 'ghostel-mode)))
                         existing
                       (generate-new-buffer name)))
             ;; OpenSSH joins remote command arguments through a shell. Quote
             ;; the user-selected session name explicitly; the local ssh
             ;; invocation itself still bypasses a shell via `ghostel-exec'.
             (remote-command
              (format "exec tmux new-session -A -s %s"
                      (shell-quote-argument session))))
        (pop-to-buffer buffer)
        (ghostel-exec
         buffer "ssh" (list "-t" host remote-command)
         `((kind . remote-tmux)
           (host . ,host)
           (session . ,session)
           (command . ("ssh" "-t" ,host ,remote-command))))))))

;;;###autoload
(defun my/remote-dired (host)
  "Open dired on HOST's home directory over TRAMP."
  (interactive (list (my/remote-pick-host)))
  (dired (format "/ssh:%s:~" host)))

;;; --- Plain dired (dirvish stutters over TRAMP) -----------------------------

;; dirvish's previews spawn a remote process on every cursor move and its
;; vc-state attribute runs git per file, so remote dired stalls (~3s per dir).
;; Plain dired is near-instant remotely and fully functional, so turn off the
;; dirvish takeover globally.
(after! dirvish
  (dirvish-override-dired-mode -1))

;; `C-c' is Ghostel's Emacs escape prefix in semi-char and line modes.
;; `C-c p' belongs to persp-mode, so keep Pi on uppercase `P'. These are
;; inherited by Ghostel's normal input modes without stealing keys from
;; full-screen char-mode TUIs.
(after! ghostel
  ;; Remove the earlier experimental binding as well when this file is
  ;; re-evaluated in a live daemon; persp-mode owns `C-c p'.
  (define-key ghostel-mode-map (kbd "C-c p") nil)
  (define-key ghostel-mode-map (kbd "C-c P") #'my/pi-spawn)
  (define-key ghostel-mode-map (kbd "C-c C-P") #'my/pi-spawn-with-prompt))

(map! :leader
      (:prefix "o"
       :desc "New Pi terminal"          "p" #'my/pi-spawn
       :desc "New Pi terminal + prompt"  "P" #'my/pi-spawn-with-prompt
       :desc "New Ghostel terminal"     "T" #'my/ghostel-spawn
       (:prefix ("x" . "remote")
        :desc "tmux terminal"        "t" #'my/remote-tmux
        :desc "Dired / files"        "f" #'my/remote-dired
        :desc "Provision tmux.conf"  "p" #'my/remote-provision-tmux)))

(provide 'config-remote)
;;; config-remote.el ends here
