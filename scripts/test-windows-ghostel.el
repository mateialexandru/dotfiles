;;; test-windows-ghostel.el --- Windows compilation regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defmacro after! (feature &rest body)
  (declare (indent 1))
  `(with-eval-after-load ',feature ,@body))

(defmacro map! (&rest _)
  nil)

(defconst my/windows-ghostel-test-config
  (expand-file-name "../config/doom/config-windows.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(load my/windows-ghostel-test-config nil t)

(ert-deftest my/windows-compilation-uses-stock-locally-and-retains-remote-routing ()
  (let* ((args (list "build command" nil #'ignore nil t))
         (stock (lambda (&rest forwarded) (cons 'stock forwarded)))
         (ghostel (lambda (next &rest forwarded)
                    (should (eq next stock))
                    (cons 'ghostel forwarded))))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (_) nil)))
      (should (equal (apply #'my/windows-ghostel-compilation-start ghostel stock args)
                     (cons 'stock args))))
    (cl-letf (((symbol-function 'file-remote-p) (lambda (_) t)))
      (should (equal (apply #'my/windows-ghostel-compilation-start ghostel stock args)
                     (cons 'ghostel args))))))

(ert-deftest my/windows-compilation-routing-covers-first-ghostel-autoload ()
  (let ((library (make-temp-file "windows-ghostel-compile-" nil ".el"))
        (features features))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-compile--compilation-start-advice) nil)
                  ((symbol-function 'compilation-start)
                   (lambda (&rest _) 'stock)))
          (write-region
           (concat ";;; -*- lexical-binding: t; -*-\n"
                   "(defun ghostel-compile--compilation-start-advice (&rest _)\n"
                   "  (error \"Unix compilation backend was called\"))\n"
                   "(provide 'ghostel-compile)\n")
           nil library nil 'silent)
          (autoload 'ghostel-compile--compilation-start-advice library)
          (advice-add 'compilation-start :around
                      #'ghostel-compile--compilation-start-advice)
          (unwind-protect
              (progn
                (should (eq (compilation-start "build command") 'stock))
                (should (advice-member-p #'my/windows-ghostel-compilation-start
                                        'ghostel-compile--compilation-start-advice)))
            (advice-remove 'compilation-start
                           #'ghostel-compile--compilation-start-advice)
            (advice-remove 'ghostel-compile--compilation-start-advice
                           #'my/windows-ghostel-compilation-start)))
      (delete-file library))))

(ert-deftest my/windows-compilation-routing-is-idempotent ()
  (let ((features (cons 'ghostel-compile features))
        (count 0))
    (cl-letf (((symbol-function 'ghostel-compile--compilation-start-advice) #'ignore))
      (unwind-protect
          (progn
            (load my/windows-ghostel-test-config nil t)
            (load my/windows-ghostel-test-config nil t)
            (advice-mapc
             (lambda (function _properties)
               (when (eq function #'my/windows-ghostel-compilation-start)
                 (cl-incf count)))
             'ghostel-compile--compilation-start-advice)
            (should (= count 1)))
        (advice-remove 'ghostel-compile--compilation-start-advice
                       #'my/windows-ghostel-compilation-start)))))
