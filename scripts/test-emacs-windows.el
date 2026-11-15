;;; test-emacs-windows.el --- Installed Windows build checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'comp)
(require 'comp-run)
(require 'dom)
(require 'gnutls)
(require 'nsm)

(defvar doom-env-deny nil)
(defconst dotfiles/windows-cli-file
  (expand-file-name "../config/doom/cli.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest dotfiles/windows-doom-environment ()
  (dolist (platform '(windows-nt gnu/linux))
    (let ((system-type platform)
          (doom-env-deny '("^EXISTING$")))
      (cl-letf (((symbol-function 'defcli!)
                 (cons 'macro (lambda (_command _args &rest body) (cons 'progn body)))))
        (load dotfiles/windows-cli-file nil t))
      (should (member "^EXISTING$" doom-env-deny))
      (dolist (name '("PATH" "EMACS_NATIVE_COMP_ROOT"))
        (should (eq (not (null (cl-some (lambda (regexp) (string-match-p regexp name)) doom-env-deny)))
                    (eq platform 'windows-nt)))))))

(ert-deftest dotfiles/windows-required-features ()
  (should (eq system-type 'windows-nt))
  (should (native-comp-available-p))
  (should (fboundp 'module-load))
  (should (treesit-available-p))
  (should (gnutls-available-p))
  (should (libxml-available-p))
  (should (equal (json-parse-string "{\"ok\":true}" :object-type 'alist) '((ok . t))))
  (dolist (type '(svg png jpeg gif tiff webp))
    (should (image-type-available-p type)))
  (when (display-images-p)
    (should (image-size (create-image "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2\" height=\"3\"/>"
                                     'svg t) t)))
  (should (equal (dom-tag (car (with-temp-buffer
                               (insert "<root/>")
                               (list (libxml-parse-xml-region (point-min) (point-max))))))
                 'root))
  (let ((db (sqlite-open)))
    (unwind-protect
        (progn
          (sqlite-execute db "create table test (value text)")
          (sqlite-execute db "insert into test values ('ok')")
          (should (equal (sqlite-select db "select value from test") '(("ok")))))
      (sqlite-close db))))

(ert-deftest dotfiles/windows-verified-tls ()
  (require 'url)
  (let* ((gnutls-verify-error t)
         (network-security-level 'high)
         (buffer (url-retrieve-synchronously "https://www.gnu.org/" t nil 60)))
    (should (buffer-live-p buffer))
    (unwind-protect
        (with-current-buffer buffer
          (should (and (boundp 'url-http-response-status)
                       (<= 200 url-http-response-status)
                       (< url-http-response-status 400))))
      (kill-buffer buffer))))

(ert-deftest dotfiles/windows-installed-aot ()
  (let* ((installation (expand-file-name ".." invocation-directory))
         (native-dir (expand-file-name "lib/emacs" installation))
         (native-comp-jit-compilation nil))
    (should (directory-files-recursively native-dir "\\.eln\\'"))
    (require 'rot13)
    (should (subr-native-elisp-p (symbol-function 'rot13-string)))
    (should (equal (rot13-string "uryyb") "hello"))))

(ert-deftest dotfiles/windows-native-compile-load-call ()
  (let* ((dir (make-temp-file
               (expand-file-name "native-smoke-" (or (getenv "DOTFILES_EMACS_BUILD")
                                                    temporary-file-directory)) t))
         (file (expand-file-name "fixture.el" dir))
         (output (expand-file-name "fixture.eln" dir))
         (native-comp-jit-compilation nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; fixture.el -*- lexical-binding: t; -*-\n"
                    "(defun dotfiles-native-fixture (x) (+ x 7))\n"))
          (native-compile file output)
          (should (file-exists-p output))
          (load output nil t)
          (should (subr-native-elisp-p (symbol-function 'dotfiles-native-fixture)))
          (should (= (dotfiles-native-fixture 35) 42)))
      (when (fboundp 'dotfiles-native-fixture) (fmakunbound 'dotfiles-native-fixture))
      ;; Windows keeps the loaded .eln DLL open until this test process exits.
      (delete-file file))))

(ert-deftest dotfiles/windows-async-native-fresh-process ()
  (let* ((dir (make-temp-file "dotfiles-native-async-" t))
         (file (expand-file-name "async-fixture.el" dir))
         (native-comp-eln-load-path (cons dir native-comp-eln-load-path))
         (native-comp-async-jobs-number 1)
         (deadline (+ (float-time) 300)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; async-fixture.el -*- lexical-binding: t; -*-\n"
                    "(defun dotfiles-async-fixture () 42)\n"))
          (native-compile-async file)
          (while (and (< (float-time) deadline)
                      (or comp-files-queue (> (comp--async-runnings) 0)))
            (accept-process-output nil 0.2))
          (should-not comp-files-queue)
          (should (= (comp--async-runnings) 0))
          (let ((eln (car (directory-files-recursively dir "\\.eln\\'"))))
            (should eln)
            (with-temp-buffer
              (should
               (zerop
                (call-process
                 (expand-file-name invocation-name invocation-directory) nil t nil
                 "-Q" "--batch" "--eval"
                 (prin1-to-string
                  `(progn
                     (load ,eln nil t)
                     (unless (and (subr-native-elisp-p (symbol-function 'dotfiles-async-fixture))
                                  (= (dotfiles-async-fixture) 42))
                       (kill-emacs 1))))))))))
      (delete-directory dir t))))
