;;; config-windows.el --- Windows-specific Doom configuration -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Font overrides for Windows DPI scaling
(setq doom-font (font-spec :family "JetBrainsMono NF" :size 11.0)
      doom-variable-pitch-font (font-spec :family "Segoe UI" :size 12.0))

(defcustom my/windows-font-backends '(harfbuzz gdi)
  "Preferred font renderers for Windows frames.
Prefer HarfBuzz for composed text and modeline icons.  GDI-first is an
optional experiment that can impair shaping.
Run `my/windows-apply-font-backends' after changing this."
  :type '(repeat symbol)
  :group 'faces)

(defun my/windows-apply-font-backends ()
  "Apply the Windows font preference to existing and future frames."
  (interactive)
  (setf (alist-get 'font-backend default-frame-alist) my/windows-font-backends)
  (dolist (frame (frame-list))
    (when (and (eq (framep frame) 'w32)
               (not (equal (frame-parameter frame 'font-backend)
                           my/windows-font-backends)))
      (with-selected-window (frame-selected-window frame)
        (save-excursion
          (save-window-excursion
            (set-frame-parameter frame 'font-backend my/windows-font-backends)
            (doom/reload-font)))))))

(my/windows-apply-font-backends)

(defun my/windows-real-executable (name)
  "Find NAME without accepting a Windows Store execution alias."
  (let ((exec-path
         (cl-remove-if
          (lambda (path)
            (and path
                 (let ((case-fold-search t))
                   (string-match-p "[/\\\\]WindowsApps\\(?:[/\\\\]\\|\\'\\)" path))))
          exec-path)))
    (executable-find name)))

(let* ((git (my/windows-real-executable "git"))
       (bash-on-path (my/windows-real-executable "bash"))
       (root (or (and git (locate-dominating-file git "bin/bash.exe"))
                 (and bash-on-path
                      (locate-dominating-file bash-on-path "bin/git.exe"))))
       (bash (and root (expand-file-name "bin/bash.exe" root))))
  (unless (and bash (file-executable-p bash))
    (error "Git Bash not found beside Git; install Git for Windows"))
  (let ((bin (expand-file-name "bin" root))
        (usr-bin (expand-file-name "usr/bin" root)))
    (setq exec-path (delete-dups (append (list bin usr-bin) exec-path))))
  (setenv "PATH" (mapconcat #'identity (delq nil (copy-sequence exec-path))
                           path-separator))
  ;; Short paths avoid a second layer of shell quoting when available.
  (setq shell-file-name (or (w32-short-file-name bash) bash)
        explicit-shell-file-name shell-file-name)
  (after! org
    (setq org-babel-sh-command (shell-quote-argument shell-file-name)))
  (let ((native-git (expand-file-name "mingw64/bin/git.exe" root)))
    (when (file-executable-p native-git)
      (after! magit (setq magit-git-executable native-git))
      (after! vc-git (setq vc-git-program native-git)))))

(let ((fd (my/windows-real-executable "fd"))
      (rg (my/windows-real-executable "rg"))
      (pwsh (my/windows-real-executable "pwsh")))
  (when fd
    (setq doom-fd-executable fd)
    (after! projectile (setq projectile-fd-executable fd)))
  (when rg (setq doom-ripgrep-executable rg))
  (when pwsh (setq ghostel-shell (list pwsh "-NoLogo"))))

(defvar-local my/windows-vc-refresh-timer nil
  "Pending initial VC refresh for this buffer.")

(defvar-local my/windows-vc-refreshed nil
  "Non-nil after this buffer's deferred initial VC refresh.")

(defun my/windows-vc-defer-p ()
  "Whether initial VC discovery can safely wait in the current buffer."
  (and buffer-file-name
       (equal vc-handled-backends '(Git))
       (not (file-remote-p buffer-file-name))
       (not (file-symlink-p buffer-file-name))))

(defun my/windows-vc-cancel-refresh-h ()
  "Cancel this buffer's pending initial VC refresh."
  (when (timerp my/windows-vc-refresh-timer)
    (cancel-timer my/windows-vc-refresh-timer))
  (setq my/windows-vc-refresh-timer nil))

(defun my/windows-vc-refresh-on-idle (buffer file)
  "Refresh BUFFER's VC state if it still displays FILE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq my/windows-vc-refresh-timer nil)
      (when (and (not my/windows-vc-refreshed)
                 (equal buffer-file-name file)
                 (get-buffer-window buffer t)
                 (my/windows-vc-defer-p))
        (vc-refresh-state)
        (setq my/windows-vc-refreshed t)))))

(defun my/windows-vc-refresh-soon-h ()
  "Queue one initial VC refresh, without blocking file display."
  (when (and (not my/windows-vc-refreshed)
             (not (timerp my/windows-vc-refresh-timer))
             (my/windows-vc-defer-p))
    (setq my/windows-vc-refresh-timer
          (run-with-idle-timer 0.5 nil #'my/windows-vc-refresh-on-idle
                               (current-buffer) buffer-file-name))
    (add-hook 'kill-buffer-hook #'my/windows-vc-cancel-refresh-h nil t)))

(defun my/windows-vc-find-file-h ()
  "Defer local Git discovery, retaining other backends' visit semantics."
  (my/windows-vc-cancel-refresh-h)
  (setq my/windows-vc-refreshed nil)
  (if (my/windows-vc-defer-p)
      (my/windows-vc-refresh-soon-h)
    (vc-refresh-state)))

(after! vc-hooks
  (setq-default find-file-hook
                (cl-substitute #'my/windows-vc-find-file-h #'vc-refresh-state
                               (default-value 'find-file-hook)))
  (add-hook 'doom-switch-buffer-hook #'my/windows-vc-refresh-soon-h))

(after! super-save
  (setq super-save-when-buffer-switched nil)
  (remove-hook 'window-buffer-change-functions #'super-save-window-change-handler)
  (remove-hook 'window-selection-change-functions #'super-save-window-change-handler))

;; Emacs 31 otherwise probes a missing man while loading Eshell help.
(unless (executable-find (if (boundp 'manual-program) manual-program "man"))
  (defvar Man-man-k-flags '("-k")))

(defun my/windows-ghostel-compilation-start (original stock &rest args)
  "Use standard compilation locally, retaining Ghostel for remote builds."
  ;; The pinned Ghostel compiler hardcodes /bin/sh and a Unix PTY.
  (if (file-remote-p default-directory)
      (apply original stock args)
    (apply stock args)))

(after! ghostel-compile
  (unless (advice-member-p #'my/windows-ghostel-compilation-start
                          'ghostel-compile--compilation-start-advice)
    (advice-add 'ghostel-compile--compilation-start-advice :around
                #'my/windows-ghostel-compilation-start)))

;; Windows variable-pitch font (overrides Cantarell from shared config)
(setq doom-variable-pitch-font (font-spec :family "Segoe UI" :size 15))

;; Dired: Windows lacks ls, use Emacs ls-lisp with directories first
(setq dired-use-ls-dired nil
      dired-listing-switches "-alh"
      ls-lisp-dirs-first t)

;; Fix Windows drive letter encoding in eglot URIs
(after! eglot
  (aset eglot--uri-path-allowed-chars ?: t))

;;; DotNet LSP diagnostics (Windows-only: uses PowerShell)
(defun my/test-dotnet-lsp ()
  "Run DotNet LSP diagnostics for the current project."
  (interactive)
  (let* ((script (expand-file-name "../../scripts/Test-DotNetLsp.ps1" (file-truename doom-user-dir)))
         (dir (if buffer-file-name
                  (file-name-directory (file-truename buffer-file-name))
                default-directory))
         (default-directory dir))
    (compile (format "powershell -NoProfile -ExecutionPolicy Bypass -File \"%s\" -Path \"%s\""
                     script dir))))

(map! :leader
      (:prefix "c"
       :desc "Test DotNet LSP" "R" #'my/test-dotnet-lsp))

;;; Magit — performance tuning for Windows
;; Windows is ~56x slower than Unix for magit due to process spawning cost.
(after! magit
  ;; Don't auto-refresh status buffer on every magit action — only refresh
  ;; when the status buffer is the current buffer. Biggest single win.
  (setq magit-refresh-status-buffer nil)

  ;; Disable expensive diff decorations
  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil)

  ;; Remove slow status sections (each spawns a git subprocess)
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (setq magit-repolist-columns
        '(("Name" 25 magit-repolist-column-ident ())
          ("Path" 99 magit-repolist-column-path ()))))

(after! flycheck
  (let ((python (executable-find "python"))
        (real-python (or (my/windows-real-executable "python")
                         (my/windows-real-executable "python3")))
        (jq (my/windows-real-executable "jq")))
    (when (and python
               (let ((case-fold-search t))
                 (string-match-p "[/\\\\]WindowsApps[/\\\\]" python)))
      (cond
       (jq
        (setq flycheck-json-jq-executable jq)
        (setq-default flycheck-disabled-checkers
                      (cons 'json-python-json
                            (remq 'json-python-json
                                  (default-value 'flycheck-disabled-checkers)))))
       (real-python
        (setq flycheck-json-python-json-executable real-python))
       (t (display-warning 'dotfiles
                           "JSON checker resolves to WindowsApps; install jq or Python."
                           :warning))))))
