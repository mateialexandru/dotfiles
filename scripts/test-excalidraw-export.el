;;; test-excalidraw-export.el --- Drawing pipeline regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'ox-md)

(defvar excalidraw-test-processes nil)
(defvar excalidraw-test-commands nil)
(defvar excalidraw-test-refreshed nil)
(defvar excalidraw-test-warnings nil)

(defun my/org-excalidraw--refresh-previews (svg)
  (push svg excalidraw-test-refreshed))

(load (expand-file-name "../config/doom/config-excalidraw-export.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;; Exercise the actual Org link exporter without loading unrelated Doom services.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../config/doom/config.el"
                    (file-name-directory (or load-file-name buffer-file-name))))
  (goto-char (point-min))
  (let (form found)
    (while (not found)
      (setq form (read (current-buffer)))
      (when (and (eq (car-safe form) 'defun)
                 (eq (cadr form) 'my/org-excalidraw-export))
        (eval form t)
        (setq found t)))))

(defun excalidraw-test-make-process (&rest arguments)
  (push (plist-get arguments :command) excalidraw-test-commands)
  (let ((process (make-pipe-process :name "excalidraw-test"
                                    :buffer (plist-get arguments :buffer)
                                    :noquery t)))
    (push process excalidraw-test-processes)
    process))

(defmacro excalidraw-test-with-fixture (&rest body)
  (declare (indent 0))
  `(let* ((root (make-temp-file "excalidraw export-test-" t))
          (alpha (expand-file-name "alpha.excalidraw" root))
          (beta (expand-file-name "beta.excalidraw" root))
          (my/org-excalidraw--export-jobs (make-hash-table :test #'equal))
          (my/org-excalidraw--export-timers (make-hash-table :test #'equal))
          (excalidraw-test-processes nil)
          (excalidraw-test-commands nil)
          (excalidraw-test-refreshed nil)
          (excalidraw-test-warnings nil))
     (unwind-protect
         (progn
           (dolist (file (list alpha beta))
             (with-temp-file file
               (insert "{\"elements\":[{\"id\":\"live\",\"isDeleted\":false},"
                       "{\"id\":\"old\",\"isDeleted\":true}]}")))
           (cl-letf (((symbol-function 'make-process) #'excalidraw-test-make-process)
                     ((symbol-function 'executable-find) (lambda (_) "excalidraw-cli"))
                     ((symbol-function 'display-warning)
                      (lambda (_type message &rest _)
                        (push message excalidraw-test-warnings))))
             ,@body))
       (dolist (process excalidraw-test-processes)
         (delete-process process)
         (when-let* ((job (process-get process 'excalidraw-job)))
           (my/org-excalidraw--dispose-job job)))
       (delete-directory root t))))

(defun excalidraw-test-finish (process exit &optional no-svg)
  (let* ((job (process-get process 'excalidraw-job))
         (output (if (process-get process 'excalidraw-transparent)
                     (my/org-excalidraw--job-transparent job)
                   (my/org-excalidraw--job-opaque job))))
    (unless no-svg
      (with-temp-file output
        (insert "<svg><symbol id=\"image-test\"><image width=\"100%\"/></symbol></svg>")))
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) exit)))
      (my/org-excalidraw--export-finished process "finished\n"))))

(defmacro excalidraw-test-with-assets (&rest body)
  (declare (indent 0))
  `(let* ((note (generate-new-buffer " *Excalidraw note*"))
          (document (expand-file-name "notes.md" root))
          (md (my/org-excalidraw--asset-target alpha document nil))
          (html (my/org-excalidraw--asset-target alpha document t)))
     (unwind-protect
         (progn
           (with-current-buffer note
             (setq major-mode 'org-mode buffer-file-name
                   (expand-file-name "notes.org" root))
             (insert (concat "[[excalidraw:" alpha ".svg]]")))
           (dolist (target (list md html))
             (make-directory (file-name-directory target) t)
             (with-temp-file target (insert "old asset")))
           ,@body)
       (with-current-buffer note (set-buffer-modified-p nil))
       (kill-buffer note))))

(ert-deftest excalidraw-independent-jobs-and-selective-previews ()
  (excalidraw-test-with-fixture
    (puthash alpha 'pending my/org-excalidraw--export-timers)
    (let ((a (my/org-excalidraw--export alpha))
          (b (my/org-excalidraw--export beta)))
      (should-not (gethash alpha my/org-excalidraw--export-timers))
      (should (= (hash-table-count my/org-excalidraw--export-jobs) 2))
      (should-not (eq a b))
      (should (equal (caar excalidraw-test-commands) "excalidraw-cli"))
      (excalidraw-test-finish a 0)
      (should (equal excalidraw-test-refreshed (list (concat alpha ".svg"))))
      (should (eq (my/org-excalidraw--job-process
                   (gethash beta my/org-excalidraw--export-jobs)) b))
      (excalidraw-test-finish b 0)
      (should (equal excalidraw-test-refreshed
                     (list (concat beta ".svg") (concat alpha ".svg"))))
      (should (= (hash-table-count my/org-excalidraw--export-jobs) 0))
      (should-not excalidraw-test-warnings))))

(ert-deftest excalidraw-snapshot-filters-deleted-elements-and-repairs-images ()
  (excalidraw-test-with-fixture
    (let* ((original (with-temp-buffer (insert-file-contents alpha) (buffer-string)))
           (process (my/org-excalidraw--export alpha))
           (job (process-get process 'excalidraw-job))
           (input (my/org-excalidraw--job-input job)))
      (with-temp-buffer
        (insert-file-contents input)
        (should (= (length (gethash "elements" (json-parse-buffer))) 1)))
      (excalidraw-test-finish process 0)
      (should-not (file-exists-p input))
      (with-temp-buffer
        (insert-file-contents (concat alpha ".svg"))
        (should (search-forward "viewBox=\"0 0 100 100\"" nil t)))
      (with-temp-buffer
        (insert-file-contents alpha)
        (should (equal original (buffer-string)))))))

(ert-deftest excalidraw-repeated-saves-queue-one-rerun-without-publishing-stale-output ()
  (excalidraw-test-with-fixture
    (let ((first (my/org-excalidraw--export alpha)))
      (dotimes (_ 3) (should (eq first (my/org-excalidraw--export alpha))))
      (should (= (length excalidraw-test-commands) 1))
      (excalidraw-test-finish first 0)
      (should-not excalidraw-test-refreshed)
      (should-not (file-exists-p (concat alpha ".svg")))
      (should (= (length excalidraw-test-commands) 2))
      (excalidraw-test-finish
       (my/org-excalidraw--job-process (gethash alpha my/org-excalidraw--export-jobs)) 0)
      (should (equal excalidraw-test-refreshed (list (concat alpha ".svg"))))
      (should (= (hash-table-count my/org-excalidraw--export-jobs) 0)))))

(ert-deftest excalidraw-failure-retains-output-and-last-good-svg ()
  (excalidraw-test-with-fixture
    (with-temp-file (concat alpha ".svg") (insert "last good"))
    (let* ((process (my/org-excalidraw--export alpha))
           (buffer (process-buffer process)))
      (excalidraw-test-finish process 1)
      (should (buffer-live-p buffer))
      (should-not excalidraw-test-refreshed)
      (should (string-match-p (regexp-quote alpha) (car excalidraw-test-warnings)))
      (should-not (gethash alpha my/org-excalidraw--export-jobs))
      (with-temp-buffer
        (insert-file-contents (concat alpha ".svg"))
        (should (equal (buffer-string) "last good")))
      (should (processp (my/org-excalidraw--export alpha))))))

(ert-deftest excalidraw-missing-output-is-not-success-even-with-an-old-svg ()
  (excalidraw-test-with-fixture
    (with-temp-file (concat alpha ".svg") (insert "last good"))
    (excalidraw-test-finish (my/org-excalidraw--export alpha) 0 t)
    (should-not excalidraw-test-refreshed)
    (should (string-match-p "SVG missing or empty" (car excalidraw-test-warnings)))))

(ert-deftest excalidraw-invalid-source-or-missing-exporter-signals ()
  (excalidraw-test-with-fixture
    (should-error (my/org-excalidraw--export (expand-file-name "missing.excalidraw" root))
                  :type 'user-error)
    (should-error (my/org-excalidraw--export "/ssh:example:/remote.excalidraw")
                  :type 'user-error)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (my/org-excalidraw--export alpha) :type 'user-error))
    (should-not excalidraw-test-commands)))

(ert-deftest excalidraw-launch-failure-cleans-owned-files-and-buffer ()
  (excalidraw-test-with-fixture
    (let (output-buffer input output)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest arguments)
                   (setq output-buffer (plist-get arguments :buffer)
                         input (nth 2 (plist-get arguments :command))
                         output (car (last (plist-get arguments :command))))
                   (error "Test launch failure"))))
        (should-error (my/org-excalidraw--export alpha) :type 'error))
      (should-not (buffer-live-p output-buffer))
      (should-not (file-exists-p input))
      (should-not (file-exists-p output))
      (should (= (hash-table-count my/org-excalidraw--export-jobs) 0)))))

(ert-deftest excalidraw-stale-sentinel-does-not-remove-new-job ()
  (excalidraw-test-with-fixture
    (let ((old (my/org-excalidraw--export alpha)))
      (excalidraw-test-finish old 0)
      (let ((current (my/org-excalidraw--export alpha)))
        (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit)))
          (my/org-excalidraw--export-finished old "finished again"))
        (should (eq (my/org-excalidraw--job-process
                     (gethash alpha my/org-excalidraw--export-jobs)) current))
        (should (= (length excalidraw-test-refreshed) 1))))))

(ert-deftest excalidraw-html-and-markdown-assets-refresh-after-background-variants ()
  (excalidraw-test-with-fixture
    (excalidraw-test-with-assets
      (let* ((process (my/org-excalidraw--export alpha))
             (job (process-get process 'excalidraw-job)))
        (excalidraw-test-finish process 0)
        (should-not excalidraw-test-refreshed)
        (should-not (file-exists-p (concat alpha ".svg")))
        (let ((transparent (my/org-excalidraw--job-process job)))
          (should-not (eq transparent process))
          (should (member "--no-export-background" (car excalidraw-test-commands)))
          (excalidraw-test-finish transparent 0))
        (dolist (target (list md html))
          (with-temp-buffer
            (insert-file-contents target)
            (should (search-forward "viewBox=\"0 0 100 100\"" nil t))))
        (should (equal excalidraw-test-refreshed (list (concat alpha ".svg"))))
        (should-not excalidraw-test-warnings)))))

(ert-deftest excalidraw-transparent-failure-does-not-publish-either-variant ()
  (excalidraw-test-with-fixture
    (excalidraw-test-with-assets
      (with-temp-file (concat alpha ".svg") (insert "last good"))
      (let* ((process (my/org-excalidraw--export alpha))
             (job (process-get process 'excalidraw-job)))
        (excalidraw-test-finish process 0)
        (excalidraw-test-finish (my/org-excalidraw--job-process job) 1)
        (should-not excalidraw-test-refreshed)
        (should excalidraw-test-warnings)
        (should (buffer-live-p (my/org-excalidraw--job-buffer job)))
        (should-not (gethash alpha my/org-excalidraw--export-jobs))
        (should-not (file-exists-p (my/org-excalidraw--job-input job)))
        (dolist (target (list md html))
          (with-temp-buffer
            (insert-file-contents target)
            (should (equal (buffer-string) "old asset"))))
        (with-temp-buffer
          (insert-file-contents (concat alpha ".svg"))
          (should (equal (buffer-string) "last good")))))))

(ert-deftest excalidraw-save-during-transparent-conversion-reruns-the-whole-job ()
  (excalidraw-test-with-fixture
    (excalidraw-test-with-assets
      (let* ((process (my/org-excalidraw--export alpha))
             (job (process-get process 'excalidraw-job)))
        (excalidraw-test-finish process 0)
        (my/org-excalidraw--export alpha)
        (excalidraw-test-finish (my/org-excalidraw--job-process job) 0)
        (should-not excalidraw-test-refreshed)
        (should-not (file-exists-p (concat alpha ".svg")))
        (let* ((current (gethash alpha my/org-excalidraw--export-jobs))
               (opaque (my/org-excalidraw--job-process current)))
          (should-not (eq current job))
          (excalidraw-test-finish opaque 0)
          (excalidraw-test-finish (my/org-excalidraw--job-process current) 0))
        (should (equal excalidraw-test-refreshed (list (concat alpha ".svg"))))
        (should-not excalidraw-test-warnings)))))

(ert-deftest excalidraw-markdown-export-waits-for-fresh-conversion ()
  (excalidraw-test-with-fixture
    (let ((output (expand-file-name "notes.md" root))
          (conversions 0))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_program _input _dest _display &rest arguments)
                   (cl-incf conversions)
                   (with-temp-file (car (last arguments)) (insert "<svg/>"))
                   0)))
        (let ((link (my/org-excalidraw-export
                     (concat alpha ".svg") "Drawing" 'md (list :output-file output))))
          (should (= conversions 1))
          (should (string-match-p "!\\[Drawing\\](notes-assets/" link))
          (should (file-exists-p (my/org-excalidraw--asset-target alpha output nil)))
          (should-not excalidraw-test-commands))))))

(ert-deftest excalidraw-background-snapshot-cannot-overwrite-an-explicit-export ()
  (excalidraw-test-with-fixture
    (let ((background (my/org-excalidraw--export alpha)))
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_program _input _dest _display &rest arguments)
                   (with-temp-file (car (last arguments)) (insert "<svg>fresh</svg>"))
                   0)))
        (my/org-excalidraw--ensure-svg alpha))
      (excalidraw-test-finish background 0)
      (should-not excalidraw-test-refreshed)
      (with-temp-buffer
        (insert-file-contents (concat alpha ".svg"))
        (should (equal (buffer-string) "<svg>fresh</svg>")))
      (should (= (length excalidraw-test-commands) 2)))))

(ert-run-tests-batch-and-exit)
