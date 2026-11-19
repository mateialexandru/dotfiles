;;; test-profile.el --- Private layer precedence regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)

(let ((directory-files-function (symbol-function 'directory-files)))
  (cl-letf (((symbol-function 'directory-files)
             (lambda (directory &rest arguments)
               (unless (string-suffix-p "/layers.d/" directory)
                 (apply directory-files-function directory arguments)))))
    (load (expand-file-name "../config/doom/config-profile.el"
                            (file-name-directory (or load-file-name buffer-file-name)))
          nil t)))

(defvar profile-test-order nil)

(ert-deftest profile-layers-use-lexical-precedence-not-filesystem-order ()
  (let* ((my/dotfiles-layers-directory (make-temp-file "profile-layers-" t))
         (earlier (expand-file-name "10-personal" my/dotfiles-layers-directory))
         (later (expand-file-name "20-work" my/dotfiles-layers-directory))
         (directory-files-function (symbol-function 'directory-files))
         (profile-test-order nil))
    (unwind-protect
        (progn
          (dolist (entry `((,later . work) (,earlier . personal)))
            (make-directory (expand-file-name "doom/" (car entry)) t)
            (with-temp-file (expand-file-name "doom/pre.el" (car entry))
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      (format "(push '%s profile-test-order)" (cdr entry)))))
          (cl-letf (((symbol-function 'directory-files)
                     (lambda (directory &optional full match nosort count)
                       (if (equal directory my/dotfiles-layers-directory)
                           (if nosort (list later earlier) (list earlier later))
                         (funcall directory-files-function
                                  directory full match nosort count)))))
            (should (equal (my/dotfiles-layer-directories) (list earlier later)))
            (my/dotfiles-load-layers "doom/pre.el")
            (should (equal profile-test-order '(work personal)))))
      (delete-directory my/dotfiles-layers-directory t))))

(ert-run-tests-batch-and-exit)
