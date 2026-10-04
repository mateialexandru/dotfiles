;;; config-python.el --- uv-first Python -*- lexical-binding: t; -*-

;; uv installs the interpreter, resolves dependencies, and owns the per-project
;; `.venv'. The `+uv' module flag already does the hard part: `uv-mode-set' puts
;; the venv on `exec-path'/`PATH'/`VIRTUAL_ENV' and `+python-uv-mode-set-auto-h'
;; re-runs it on every buffer switch, so venvs activate with no prompt.
;;
;; This file only closes the gaps the module leaves: point the rest of the
;; toolchain at that venv, teach projectile what a uv project is, and add the
;; bootstrap commands uv itself can't infer. See docs/decisions/015-python-uv.md.

;; ruff formats and sorts imports in one binary, so it replaces black + isort.
;; Doom's `:editor format' defaults python to black; override both major modes
;; since `+tree-sitter' means `python-ts-mode' is what actually runs.
(after! apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;; Covers buffers with no LSP workspace (a loose file outside any project);
;; inside one, lsp-mode's own checker supersedes these anyway.
(after! flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(python-flake8 python-pylint))))

(after! python
  (setq python-check-command "ruff"))

;; pyright is installed as a `uv tool', which puts it in its own isolated
;; environment. Point it at the nearest `.venv' instead, or every third-party
;; import in a project resolves to nothing.
(after! lsp-pyright
  (setq lsp-pyright-venv-directory ".venv"))

;; `uv run' resolves the project venv itself, so tests work from any buffer
;; without the venv being active in the Emacs process.
(after! python-pytest
  (setq python-pytest-executable "uv run pytest"))

;; Without this a uv project matches projectile's `python-toml' type and
;; `SPC p t' runs `python -m unittest discover'. Registration prepends, so this
;; wins; `uv.lock' rather than `pyproject.toml' keeps it off poetry/pdm roots.
(after! projectile
  (projectile-register-project-type
   'python-uv '("uv.lock")
   :project-file "pyproject.toml"
   :compile "uv build"
   :install "uv sync"
   :test "uv run pytest"
   :run "uv run"
   :test-prefix "test_"
   :test-suffix "_test"))

;; `SPC c c' on a python buffer should just run the thing. uv picks the project
;; venv or the file's own PEP 723 header, whichever applies.
(defun +python-uv-set-compile-command-h ()
  (when (and buffer-file-name (executable-find "uv"))
    (setq-local compile-command
                (format "uv run %s"
                        (shell-quote-argument
                         (file-relative-name buffer-file-name))))))
(add-hook 'python-base-mode-hook #'+python-uv-set-compile-command-h)


;;
;;; Bootstrapping

;; The one thing uv can't do for you: turn a file you already have into
;; something isolated and runnable. Everything else (`uv add', `uv sync', ...)
;; is a shell command and stays one.

(defun +python--uv (dir command &optional then)
  "Run COMMAND in DIR; call THEN once it finishes successfully.
`compilation-start' rather than `compile', which would leave the
bootstrap command behind as the buffer's `compile-command'."
  (let ((default-directory (file-name-as-directory dir)))
    (when then
      (letrec ((watch (lambda (buf status)
                        (when (equal (buffer-name buf) "*uv*")
                          (remove-hook 'compilation-finish-functions watch)
                          (when (string-prefix-p "finished" status)
                            (funcall then))))))
        (add-hook 'compilation-finish-functions watch)))
    (compilation-start command nil (lambda (&rest _) "*uv*"))))

(defun +python--uv-norm (name)
  "Normalize distribution NAME per PEP 503."
  (downcase (replace-regexp-in-string "[-_.]+" "-" name)))

(defun +python--uv-local-modules (dir)
  "Module names importable from DIR — the .py files and packages beside it."
  (let (names)
    (dolist (f (ignore-errors (directory-files dir nil "\\`[^.]" t)) names)
      (cond ((string-suffix-p ".py" f)
             (push (+python--uv-norm (file-name-sans-extension f)) names))
            ((file-exists-p (expand-file-name (concat f "/__init__.py") dir))
             (push (+python--uv-norm f) names))))))

(defun +python--uv-deps (file)
  "Ask which dependencies FILE needs, seeded from its imports.
pipreqs runs through `uvx' (fetched on first use, nothing to install) and
maps import names onto distribution names — PIL to Pillow, bs4 to
beautifulsoup4 — skipping stdlib. FILE is copied somewhere empty first so
neighbouring scripts don't contribute imports.

That isolation hides FILE's own siblings from pipreqs too, which matters
more than it sounds: an import of a local `helpers.py' gets looked up on
PyPI, and if some unrelated `helpers' package exists there it lands in the
install list. So modules found beside FILE are subtracted from both the
seed and the warning.

What pipreqs genuinely cannot map is named in the prompt rather than
dropped, since that's where a hand edit is needed — cv2 has no package of
that name and wants opencv-python. The seed stays editable either way."
  (let* ((scratch (make-temp-file "uv-scan-" t))
         (errfile (make-temp-file "uv-pipreqs-"))
         status stdout stderr)
    (unwind-protect
        (progn
          (copy-file file (expand-file-name (file-name-nondirectory file) scratch) t)
          (with-temp-buffer
            (setq status (call-process "uvx" nil (list t errfile) nil
                                       "pipreqs" "--print" "--mode" "no-pin" scratch)
                  stdout (buffer-string)))
          (with-temp-buffer
            (insert-file-contents errfile)
            (setq stderr (buffer-string))))
      (delete-file errfile)
      (delete-directory scratch t))
    (unless (eq status 0)
      (user-error "pipreqs failed:\n%s" stderr))
    (let* ((locals (+python--uv-local-modules (file-name-directory file)))
           (localp (lambda (name) (member (+python--uv-norm name) locals)))
           (packages (seq-remove localp (split-string stdout "[ \t\n\r]+" t)))
           unresolved)
      (dolist (line (split-string stderr "\n" t))
        (when (string-match "Package \"\\([^\"]+\\)\" does not exist" line)
          (let ((name (match-string 1 line)))
            (unless (funcall localp name)
              (push name unresolved)))))
      (split-string
       (read-string (if unresolved
                        ;; Keep "uv add:" adjacent to the editable text; without
                        ;; a boundary the seed reads as part of the warning.
                        (format "[not on PyPI: %s]  uv add: "
                                (string-join (nreverse unresolved) ", "))
                      "uv add: ")
                    (string-join packages " "))
       "[ \t\n]+" t))))

(defun +python--uv-reactivate (root)
  "Notice a project and `.venv' that appeared after buffers were opened.
Both `+uv' and projectile cache their answers, so without this the new
venv stays unactivated and projectile keeps reporting no project — which
is what decides whether `SPC p t' runs `uv run pytest'."
  (when (fboundp 'projectile-invalidate-cache)
    (projectile-invalidate-cache nil))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (derived-mode-p 'python-base-mode)
                 (file-in-directory-p buffer-file-name root))
        (kill-local-variable '+python--uv-project)
        (+python-uv-mode-set-auto-h)
        (+python-uv-set-compile-command-h)))))

(defun +python/uv-new-project (dir kind)
  "Scaffold a uv app, CLI package, or library in DIR and open its source file.
The project gets a local environment and pytest and Ruff as development
dependencies.  Apps open `main.py'; CLI packages have an installable entry point."
  (interactive
   (let* ((kind (completing-read "New project: "
                                 '("Python app" "Python CLI package" "Python library")
                                 nil t nil nil "Python app"))
          (parent (or (when-let* ((root (ignore-errors (projectile-project-root))))
                        (file-name-directory (directory-file-name root)))
                      default-directory)))
     (list (read-directory-name "Project directory: " parent)
           (pcase kind
             ("Python CLI package" 'cli)
             ("Python library" 'library)
             (_ 'app)))))
  (let ((dir (expand-file-name dir)))
    (when (and (file-exists-p dir) (not (file-directory-p dir)))
      (user-error "%s is not a directory" dir))
    (when (and (file-directory-p dir)
               (seq-remove (lambda (name) (member name '("." "..")))
                           (directory-files dir)))
      (user-error "Choose a new or empty directory: %s" dir))
    (+python--uv (file-name-directory (directory-file-name dir))
                 (format "uv init %s --no-workspace %s && uv add --dev --directory %s pytest ruff"
                         (pcase kind
                           ('library "--lib")
                           ('cli "--app --package")
                           (_ "--app --no-package"))
                         (shell-quote-argument dir)
                         (shell-quote-argument dir))
                 (lambda ()
                   (let* ((src (expand-file-name "src" dir))
                          (entry (if (eq kind 'app)
                                     (expand-file-name "main.py" dir)
                                   (and (file-directory-p src)
                                        (car (directory-files-recursively
                                              src "__init__\\.py\\'"))))))
                     (projectile-add-known-project dir)
                     (when (fboundp '+workspaces-switch-to-project-h)
                       (let ((current-prefix-arg t))
                         (+workspaces-switch-to-project-h dir)))
                     (find-file (or entry (expand-file-name "pyproject.toml" dir)))
                     (+python--uv-reactivate dir))))))

(defun +python/uv-adopt-file (dir)
  "Move this file into a new isolated uv project at DIR, with its imports.
Leaves it in a project with its own `.venv', after which LSP, formatting
and tests all resolve against that."
  (interactive
   (list (read-directory-name
          "Adopt into new uv project: "
          (file-name-as-directory
           (file-name-sans-extension
            (or buffer-file-name (user-error "This buffer is not visiting a file")))))))
  (let* ((file (or buffer-file-name (user-error "This buffer is not visiting a file")))
         (dir (expand-file-name dir))
         (dest (expand-file-name (file-name-nondirectory file) dir))
         (deps (+python--uv-deps file)))
    (when (file-exists-p dest)
      (user-error "%s already exists" dest))
    (when (buffer-modified-p) (save-buffer))
    (make-directory dir t)
    (rename-file file dest)
    (set-visited-file-name dest t)
    (set-buffer-modified-p nil)
    (+python--uv dir
                 (string-join
                  (delq nil (list "uv init --bare ."
                                  (when deps
                                    (concat "uv add "
                                            (mapconcat #'shell-quote-argument deps " ")))
                                  "uv sync"))
                  " && ")
                 (lambda () (+python--uv-reactivate dir)))))

(defun +python/uv-adopt-script ()
  "Give this file a PEP 723 header carrying its imports.
`uv run' then works on it from anywhere with no project and no venv to
manage — the light alternative to `+python/uv-adopt-file' for one-offs."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "This buffer is not visiting a file")))
         (headerp (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "^# /// script" 4096 t)))
         (deps (+python--uv-deps file))
         (steps (delq nil
                      (list (unless headerp
                              (format "uv init --script %s" (shell-quote-argument file)))
                            (when deps
                              (format "uv add --script %s %s"
                                      (shell-quote-argument file)
                                      (mapconcat #'shell-quote-argument deps " ")))))))
    (if (null steps)
        (message "uv: already a script, nothing to add")
      (when (buffer-modified-p) (save-buffer))
      (+python--uv (file-name-directory file) (string-join steps " && ")
                   (lambda () (revert-buffer t t t))))))

;; Evaluate short snippets in the same Python process used by Doom's REPL.
;; On the first eval, start the process and seed it from the unsaved buffer.
;; `python-shell-send-buffer' skips __main__ blocks unless explicitly asked to
;; run them. The focused line is not sent twice on that first press.
(defun +python--start-repl-and-load-buffer ()
  "Start this buffer's Python REPL and send the whole buffer to it."
  (let ((handler (assq major-mode +eval-repl-handler-alist)))
    (unless handler
      (user-error "No Python REPL handler for %s" major-mode))
    (save-current-buffer
      (save-selected-window
        (+eval/open-repl-other-window handler))))
  (let ((process (or (python-shell-get-process)
                     (user-error "Python REPL did not start"))))
    ;; python.el installs its eval helpers only after the first prompt.
    (with-current-buffer (process-buffer process)
      (with-timeout (10 (user-error "Python REPL did not become ready"))
        (while (not python-shell--first-prompt-received)
          (accept-process-output process 0.1)))))
  (save-restriction
    (widen)
    (python-shell-send-buffer nil t))
  ;; Let the initial buffer finish before the next focused evaluation captures
  ;; output, so its result cannot be mistaken for a later line's result.
  (with-current-buffer (process-buffer (python-shell-get-process))
    (with-timeout (2 nil)
      (python-shell-accept-process-output (get-buffer-process (current-buffer)) 0.2)))
  (+eval-display-results
   "Started Python REPL and sent buffer. Press SPC e again to eval at point."
   (current-buffer)))

;; `python-shell-send-string-no-output' captures stdout for an inline result;
;; the process keeps definitions and variables for later interactive use.
(defun +python/eval-in-repl (beg end)
  "Evaluate Python between BEG and END in the project REPL and show its output."
  (if (not (python-shell-get-process))
      (+python--start-repl-and-load-buffer)
    (let* ((process (python-shell-get-process-or-error t))
           (source (python-shell-buffer-substring beg end))
           (output (string-trim (python-shell-send-string-no-output source process))))
      (+eval-display-results (if (string-empty-p output) "Sent to REPL" output)
                             (current-buffer)))))

(defun +python--eval-defun-bounds ()
  "Return the enclosing definition, including its decorators."
  (when-let* ((bounds (bounds-of-thing-at-point 'defun)))
    (cons (save-excursion
            (goto-char (car bounds))
            (while (and (not (bobp))
                        (save-excursion
                          (forward-line -1)
                          (python-nav-beginning-of-statement)
                          (looking-at-p "[[:space:]]*@")))
              (forward-line -1)
              (python-nav-beginning-of-statement))
            (point))
          (cdr bounds))))

(defun +python--eval-block-bounds ()
  "Return the complete enclosing top-level compound statement."
  (save-excursion
    (when (python-nav-beginning-of-block)
      ;; Walk out of nested blocks until the REPL can parse the block alone.
      (while (> (current-indentation) 0)
        (forward-line -1)
        (unless (python-nav-beginning-of-block)
          (user-error "Cannot find the enclosing Python block")))
      ;; An elif/else/except/finally belongs with the preceding block.
      (while (and (looking-at-p "\\(?:elif\\|else\\|except\\|finally\\)\\b")
                  (not (bobp)))
        (forward-line -1)
        (while (and (not (bobp))
                    (or (> (current-indentation) 0)
                        (python-info-current-line-empty-p)
                        (python-info-current-line-comment-p)))
          (forward-line -1))
        (python-nav-beginning-of-block))
      (let ((beg (point))
            end)
        (python-nav-end-of-block)
        (setq end (point))
        (while (progn
                 (goto-char end)
                 (forward-line 1)
                 (while (and (not (eobp))
                             (or (python-info-current-line-empty-p)
                                 (python-info-current-line-comment-p)))
                   (forward-line 1))
                 (and (not (eobp))
                      (= (current-indentation) 0)
                      (looking-at-p "\\(?:elif\\|else\\|except\\|finally\\)\\b")
                      (progn (python-nav-end-of-block)
                             (setq end (point))))))
        (cons beg end)))))

(defun +python--eval-bounds ()
  "Return bounds of the Python unit at point.
Prefer an enclosing function or class, then a top-level block, then
the current statement.  The statement case includes multiline calls."
  (or (+python--eval-defun-bounds)
      (+python--eval-block-bounds)
      (save-excursion
        (python-nav-beginning-of-statement)
        (let ((beg (point)))
          (python-nav-end-of-statement)
          (cons beg (point))))))

(defun +python/eval-dwim (&optional line-only)
  "Evaluate the Python unit at point, or the selection, in the REPL.
With prefix argument LINE-ONLY, evaluate just the current line."
  (interactive "P")
  (pcase-let ((`(,beg . ,end)
               (cond ((doom-region-active-p)
                      (cons (region-beginning) (region-end)))
                     (line-only
                      (cons (line-beginning-position) (line-end-position)))
                     (t (+python--eval-bounds)))))
    (+python/eval-in-repl beg end)))

(defun +python/eval-defun ()
  "Evaluate the Python function or class at point in the REPL."
  (interactive)
  (if-let* ((bounds (bounds-of-thing-at-point 'defun)))
      (+python/eval-in-repl (car bounds) (cdr bounds))
    (user-error "No Python function or class at point")))

(defun +python/eval-buffer ()
  "Evaluate the whole Python buffer in the REPL."
  (interactive)
  (save-restriction
    (widen)
    (+python/eval-in-repl (point-min) (point-max))))

(map! :after python
      :map python-base-mode-map
      :localleader
      :desc "Eval at point or selection in REPL" "e" #'+python/eval-dwim
      :desc "Eval function or class in REPL" "E" #'+python/eval-defun
      :desc "Eval buffer in REPL" "b" #'+python/eval-buffer
      :desc "Open REPL" "r" #'+eval/open-repl-other-window)

(map! :after python
      :map python-base-mode-map
      :n "SPC e" #'+python/eval-dwim
      :v "SPC e" #'+python/eval-dwim
      :n "SPC E" #'+python/eval-buffer)

(map! :after python
      :map python-base-mode-map
      :i "C-c e" #'+python/eval-dwim)

(map! :leader
      (:prefix "p"
       :desc "New Python project" "n" #'+python/uv-new-project))
