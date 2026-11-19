;;; config-powershell.el --- PowerShell editing and explicit execution -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'imenu)
(require 'subr-x)

(declare-function powershell-mode "powershell")
(declare-function lsp "lsp-mode")
(declare-function lsp-mode "lsp-mode")
(declare-function lsp-workspace-folders-add "lsp-mode")
(declare-function apheleia-mode "apheleia")
(declare-function lsp--workspace-client "lsp-mode")
(declare-function lsp--client-server-id "lsp-mode")
(declare-function lsp--workspace-root "lsp-mode")
(declare-function lsp-session "lsp-mode")
(declare-function lsp-session-folder->servers "lsp-mode")
(declare-function lsp-f-canonical "lsp-mode")
(declare-function lsp-find-definition "lsp-mode")
(declare-function lsp--path-to-uri "lsp-mode")
(declare-function lsp--open-in-workspace "lsp-mode")

(defvar lsp-pwsh-dir)
(defvar lsp-pwsh-exe)
(defvar lsp-pwsh-pses-script)
(defvar lsp-pwsh-log-path)
(defvar lsp-pwsh-developer-editor-services-log-level)
(defvar lsp-server-install-dir)
(defvar lsp-auto-guess-root)
(defvar lsp-guess-root-without-session)
(defvar lsp-format-buffer-on-save)
(defvar lsp-fix-all-on-save)
(defvar lsp-enable-file-watchers)
(defvar lsp--cur-workspace)
(defvar lsp--buffer-workspaces)
(defvar +format-on-save-disabled-modes)
(defvar +format-with-lsp)
(defvar +lookup-definition-functions)

(defgroup my/powershell nil
  "PowerShell navigation and on-demand development tools."
  :group 'languages)

(defcustom my/powershell-editor-services-directory nil
  "Optional modules directory containing PowerShellEditorServices.
Otherwise reuse an installed lsp-mode or VS Code copy.  Nothing is downloaded."
  :type '(choice (const nil) directory))

(defcustom my/powershell-test-executable nil
  "Optional PowerShell executable for Pester; otherwise prefer pwsh."
  :type '(choice (const nil) file))

(defcustom my/powershell-navigation-modules '("Pester")
  "Installed module names whose source PSES should index for navigation.
Only these module directories are inspected, in PSModulePath order.
The newest installed version in the first matching module path is used.
Modules are parsed by PSES, never imported or executed."
  :type '(repeat string))

(defcustom my/powershell-library-directories nil
  "Additional local library directories for PowerShell navigation.
These are added only to PowerShell servers, not to the project picker."
  :type '(repeat directory))

(defconst my/powershell--test-runner
  (expand-file-name "../../scripts/invoke-pester.ps1"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar my/powershell--test-buffers (make-hash-table :test #'equal))
(defvar-local my/powershell--base-imenu-function nil)
(defvar-local my/powershell--test-file nil)
(defvar-local my/powershell--navigation-root nil)

(defun my/powershell--root ()
  "Return the nearest Scripts tree, Git checkout, or current directory."
  (file-name-as-directory
   (expand-file-name
    (or (locate-dominating-file
         default-directory
         (lambda (directory)
           (string-equal-ignore-case
            (file-name-nondirectory (directory-file-name directory)) "scripts")))
        (locate-dominating-file default-directory ".git")
        default-directory))))

(defun my/powershell--retain-root (original session file)
  "Retain the originating script root when PSES reconnects a source or library."
  (if (and (derived-mode-p 'powershell-mode) my/powershell--navigation-root)
      my/powershell--navigation-root
    (funcall original session file)))

(defun my/powershell--module-directory (name paths)
  "Find the installed module NAME under PATHS without loading its manifest."
  (unless (string-match-p "\\`[[:alnum:]_][[:alnum:]_.-]*\\'" name)
    (user-error "Invalid PowerShell navigation module name: %s" name))
  (cl-loop
   for path in paths
   for directory = (expand-file-name name path)
   when (and (not (file-remote-p directory))
             (not (string-prefix-p "//" (subst-char-in-string ?\\ ?/ directory)))
             (file-directory-p directory))
   thereis
   (let* ((versions
           (cl-remove-if-not
            (lambda (entry)
              (and (file-directory-p entry)
                   (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)*\\'"
                                   (file-name-nondirectory entry))
                   (or (file-regular-p (expand-file-name (concat name ".psd1") entry))
                       (file-regular-p (expand-file-name (concat name ".psm1") entry)))))
            (directory-files directory t directory-files-no-dot-files-regexp)))
          (newest (car (sort versions
                             (lambda (a b)
                               (version< (file-name-nondirectory b)
                                         (file-name-nondirectory a)))))))
     (or newest
         (when (or (file-regular-p (expand-file-name (concat name ".psd1") directory))
                   (file-regular-p (expand-file-name (concat name ".psm1") directory)))
           directory)))))

(defun my/powershell--library-directories ()
  "Return explicit local libraries and selected installed module directories."
  (let ((paths (split-string (or (getenv "PSModulePath") "") path-separator t)))
    (delete-dups
     (append
      (mapcar
       (lambda (directory)
         (when (or (file-remote-p directory)
                   (string-prefix-p "//" (subst-char-in-string ?\\ ?/ directory))
                   (not (file-directory-p directory)))
           (user-error "PowerShell library is not a local directory: %s" directory))
         (expand-file-name directory))
       my/powershell-library-directories)
      (delq nil (mapcar (lambda (name) (my/powershell--module-directory name paths))
                        my/powershell-navigation-modules))))))

(defun my/powershell--initialize-libraries (original method parameters &rest arguments)
  "Include narrow library roots in PSES's initialize request only."
  ;; The single-root client snapshots workspaceFolders before startup hooks run.
  (when (and (equal method "initialize") lsp--cur-workspace
             (eq (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace))
                 'pwsh-ls))
    (let ((folders (append (plist-get parameters :workspaceFolders) nil))
          (servers (lsp-session-folder->servers (lsp-session))))
      (dolist (directory (my/powershell--library-directories))
        (let* ((root (lsp-f-canonical directory))
               (uri (lsp--path-to-uri root)))
          (unless (cl-find uri folders :key (lambda (folder) (plist-get folder :uri))
                           :test #'equal)
            (setq folders (append folders
                                  (list (list :uri uri
                                              :name (file-name-nondirectory root))))))
          (cl-pushnew lsp--cur-workspace (gethash root servers) :test #'eq)))
      (setq parameters (plist-put (copy-sequence parameters)
                                  :workspaceFolders (vconcat folders)))))
  (apply original method parameters arguments))

(defun my/powershell--definition ()
  "Find a definition through PSES without whole-repository text fallbacks."
  (interactive)
  (unless (bound-and-true-p lsp-managed-mode)
    (user-error "Start PowerShell LSP with SPC m l first"))
  (let ((workspace (car lsp--buffer-workspaces)))
    (lsp-find-definition)
    (with-current-buffer (window-buffer (selected-window))
      (when (and workspace buffer-file-name
                 (derived-mode-p 'powershell-mode)
                 (not (bound-and-true-p lsp-managed-mode)))
        (setq-local my/powershell--navigation-root (lsp--workspace-root workspace))
        (setq-local lsp--buffer-workspaces (list workspace))
        (lsp-mode 1)
        (lsp--open-in-workspace workspace)
        (when (and (not (file-in-directory-p buffer-file-name
                                            (lsp--workspace-root workspace)))
                   (cl-some (lambda (directory)
                              (file-in-directory-p buffer-file-name directory))
                            (my/powershell--library-directories)))
          (view-mode 1)))))
  'deferred)

(defun my/powershell--navigation-setup ()
  "Retain the Pester outline and keep definition lookup on the PowerShell server."
  (when (derived-mode-p 'powershell-mode)
    (my/powershell--imenu-setup)
    (setq-local +lookup-definition-functions '(my/powershell--definition))))

(defun my/powershell--pester-index ()
  "Index literal Pester suite and test names, excluding comments and strings."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          entries)
      (while (re-search-forward
              "^[ \t]*\\(Describe\\|Context\\|It\\)\\_>[ \t\n\r]+\\(?:-Name[ \t\n\r]+\\)?"
              nil t)
        (let ((position (match-beginning 1))
              (kind (match-string-no-properties 1)))
          (unless (nth 8 (save-excursion (syntax-ppss position)))
            (when (memq (char-after) '(?\" ?\'))
              (let ((start (point))
                    (end (condition-case nil (scan-sexps (point) 1)
                           (scan-error nil))))
                (when end
                  (push (cons (format "%s %s" kind
                                      (buffer-substring-no-properties
                                       (1+ start) (1- end)))
                              (copy-marker position))
                        entries)
                  (goto-char end)))))))
      (nreverse entries))))

(defun my/powershell--imenu-index ()
  "Combine Pester cases with the current mode or LSP symbol index."
  (let ((tests (my/powershell--pester-index))
        (symbols (funcall my/powershell--base-imenu-function)))
    (append (when tests (list (cons "Pester" tests))) symbols)))

(defun my/powershell--imenu-setup ()
  "Retain the Pester outline when LSP changes the symbol-index provider."
  (when (and (derived-mode-p 'powershell-mode)
             (not (eq imenu-create-index-function #'my/powershell--imenu-index)))
    (setq-local my/powershell--base-imenu-function
                (or imenu-create-index-function #'imenu-default-create-index-function))
    (setq-local imenu-create-index-function #'my/powershell--imenu-index)
    (setq imenu--index-alist nil)))

(defun my/powershell--server-directory ()
  "Find existing Editor Services without downloading or installing anything."
  (let* ((installed (expand-file-name "pwsh" lsp-server-install-dir))
         (extensions
          (sort (file-expand-wildcards
                 "~/.vscode/extensions/ms-vscode.powershell-*/modules")
                (lambda (a b)
                  (version<
                   (string-remove-prefix
                    "ms-vscode.powershell-"
                    (file-name-nondirectory
                     (directory-file-name (file-name-directory a))))
                   (string-remove-prefix
                    "ms-vscode.powershell-"
                    (file-name-nondirectory
                     (directory-file-name (file-name-directory b))))))))
         (candidates (if my/powershell-editor-services-directory
                         (list my/powershell-editor-services-directory)
                       (cons installed (reverse extensions)))))
    (or (cl-find-if
         (lambda (directory)
           (file-regular-p
            (expand-file-name "PowerShellEditorServices/Start-EditorServices.ps1"
                              directory)))
         candidates)
        (user-error
         "Install PowerShell Editor Services or set my/powershell-editor-services-directory"))))

(defun my/powershell-lsp ()
  "Start PowerShell LSP explicitly, scoped to this script tree."
  (interactive)
  (unless (derived-mode-p 'powershell-mode)
    (user-error "Open a PowerShell script first"))
  (require 'lsp-pwsh)
  (unless (and lsp-pwsh-exe (file-executable-p lsp-pwsh-exe))
    (user-error "Install PowerShell before starting its language server"))
  (let ((directory (my/powershell--server-directory))
        (root (or my/powershell--navigation-root (my/powershell--root)))
        (lsp-auto-guess-root nil)
        (lsp-guess-root-without-session nil)
        (lsp-enable-file-watchers nil)
        (lsp--cur-workspace nil))
    (setq lsp-pwsh-dir directory
          lsp-pwsh-pses-script
          (expand-file-name "PowerShellEditorServices/Start-EditorServices.ps1" directory)
          lsp-pwsh-log-path (expand-file-name "pwsh/logs" lsp-server-install-dir)
          lsp-pwsh-developer-editor-services-log-level "Error")
    (make-directory lsp-pwsh-log-path t)
    (lsp-workspace-folders-add root)
    (setq-local my/powershell--navigation-root root)
    (lsp t)))

(defun my/powershell--test-command (file)
  "Build an explicitly quoted Pester command for FILE."
  (let ((executable (or my/powershell-test-executable
                        (executable-find "pwsh") (executable-find "powershell"))))
    (unless (and executable (file-executable-p executable))
      (user-error "PowerShell is missing; configure my/powershell-test-executable"))
    (unless (and (file-regular-p file)
                 (string-match-p "\\.Tests\\.ps1\\'" file))
      (user-error "Select an existing .Tests.ps1 file"))
    (unless (file-regular-p my/powershell--test-runner)
      (user-error "Pester runner is missing: %s" my/powershell--test-runner))
    (mapconcat #'shell-quote-argument
               (list executable "-NoLogo" "-NoProfile" "-File"
                     my/powershell--test-runner "-Path" (expand-file-name file))
               " ")))

(defun my/powershell-test-file (&optional file)
  "Run the saved Pester FILE asynchronously in a retained compilation buffer."
  (interactive)
  (let* ((file (or file
                   (and buffer-file-name
                        (string-match-p "\\.Tests\\.ps1\\'" buffer-file-name)
                        buffer-file-name)
                   (read-file-name "Pester test file: " (my/powershell--root)
                                   nil t)))
         (visiting (find-buffer-visiting file))
         (root (let ((default-directory (file-name-directory (expand-file-name file))))
                 (my/powershell--root)))
         (name (format "*Pester: %s*" (abbreviate-file-name root)))
         (existing (get-buffer name))
         (default-directory root)
         (compilation-scroll-output nil)
         (compilation-auto-jump-to-first-error nil))
    (when (and visiting (buffer-modified-p visiting))
      (user-error "Save %s before running its tests" (buffer-name visiting)))
    (when (and existing (buffer-file-name existing))
      (user-error "Test output name belongs to a file buffer"))
    (when (and existing (process-live-p (get-buffer-process existing)))
      (user-error "Pester is already running; see %s" name))
    (let ((buffer
           (compilation-start
            (my/powershell--test-command file) 'compilation-mode
            (lambda (_) name))))
      (with-current-buffer buffer
        (setq-local my/powershell--test-file (expand-file-name file))
        (setq-local my/compilation-auto-close nil)
        (setq-local compilation-auto-jump-to-first-error nil)
        (setq-local compilation-error-regexp-alist
                    (cons '("^\\(.+\\.ps[dm]?1\\):\\([0-9]+\\): error:" 1 2)
                          compilation-error-regexp-alist)))
      (puthash root buffer my/powershell--test-buffers)
      buffer)))

(defun my/powershell-test-results ()
  "Show this script tree's retained Pester output."
  (interactive)
  (let ((buffer (gethash (my/powershell--root) my/powershell--test-buffers)))
    (unless (buffer-live-p buffer)
      (user-error "No Pester results for this script tree"))
    (pop-to-buffer buffer)))

(defun my/powershell-test-rerun ()
  "Repeat this script tree's last saved Pester file without saving other buffers."
  (interactive)
  (let* ((buffer (gethash (my/powershell--root) my/powershell--test-buffers))
         (file (and (buffer-live-p buffer)
                    (buffer-local-value 'my/powershell--test-file buffer))))
    (unless file
      (user-error "No Pester command to repeat"))
    (my/powershell-test-file file)))

(defun my/powershell--setup ()
  "Enable navigation and explicit execution without automatic formatting."
  (my/powershell--navigation-setup)
  (setq-local lsp-format-buffer-on-save nil
              lsp-fix-all-on-save nil
              lsp-enable-file-watchers nil
              +format-with-lsp t)
  (font-lock-add-keywords
   nil '(("^[ \t]*\\(Describe\\|Context\\|It\\|BeforeAll\\|BeforeEach\\|AfterAll\\|AfterEach\\)\\_>"
          1 font-lock-keyword-face)))
  (when (fboundp 'apheleia-mode) (apheleia-mode -1))
  (when (and buffer-file-name
             (string-match-p "\\.Tests\\.ps1\\'" buffer-file-name)
             (or my/powershell-test-executable
                 (executable-find "pwsh") (executable-find "powershell")))
    (setq-local compile-command (my/powershell--test-command buffer-file-name))))

(add-hook 'powershell-mode-hook #'my/powershell--setup)
(remove-hook 'lsp-managed-mode-hook #'my/powershell--imenu-setup)
(add-hook 'lsp-managed-mode-hook #'my/powershell--navigation-setup)

(defun my/powershell--register-capability (original &rest arguments)
  "Handle server capabilities without recursive file watches for PSES."
  ;; Registration runs in process buffers, not the originating script buffer.
  (let ((lsp-enable-file-watchers
         (and lsp-enable-file-watchers
              (not (and lsp--cur-workspace
                        (eq (lsp--client-server-id
                             (lsp--workspace-client lsp--cur-workspace))
                            'pwsh-ls))))))
    (apply original arguments)))

(with-eval-after-load 'lsp-mode
  (unless (advice-member-p #'my/powershell--retain-root 'lsp--calculate-root)
    (advice-add 'lsp--calculate-root :around #'my/powershell--retain-root))
  (remove-hook 'lsp-before-initialize-hook #'my/powershell--initialize-libraries)
  (unless (advice-member-p #'my/powershell--initialize-libraries 'lsp-request-async)
    (advice-add 'lsp-request-async :around #'my/powershell--initialize-libraries))
  (unless (advice-member-p #'my/powershell--register-capability
                          'lsp--server-register-capability)
    (advice-add 'lsp--server-register-capability :around
                #'my/powershell--register-capability)))

(with-eval-after-load '+format
  (add-to-list '+format-on-save-disabled-modes 'powershell-mode))

(when (fboundp 'map!)
  (eval
   '(map! :after powershell :map powershell-mode-map :localleader
          :desc "Start PowerShell LSP" "l" #'my/powershell-lsp
          (:prefix ("t" . "Pester")
           :desc "Run test file" "t" #'my/powershell-test-file
           :desc "Repeat last tests" "r" #'my/powershell-test-rerun
           :desc "Show test results" "o" #'my/powershell-test-results))))

(provide 'config-powershell)
;;; config-powershell.el ends here
