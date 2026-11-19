;;; config-excalidraw-export.el --- Shared drawing export pipeline -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(declare-function my/org-excalidraw--refresh-previews "config")

(defvar my/org-excalidraw--export-timers (make-hash-table :test #'equal))
(defvar my/org-excalidraw--export-jobs (make-hash-table :test #'equal)
  "Background exports keyed by source file.")

(cl-defstruct (my/org-excalidraw--job
               (:constructor my/org-excalidraw--make-job))
  source input opaque transparent buffer process rerun)

(defun my/org-excalidraw--asset-target (source output transparent)
  "Return the generated SVG target for SOURCE beside OUTPUT.
TRANSPARENT selects the web variant; the default has a canvas background."
  (let* ((output (expand-file-name output))
         (asset-dir (expand-file-name
                     (concat (downcase (file-name-base output)) "-assets")
                     (file-name-directory output)))
         (svg (concat source (if transparent ".transparent.svg" ".svg")))
         (asset-name (format "%s-%s.svg"
                             (file-name-sans-extension
                              (file-name-nondirectory svg))
                             (substring (secure-hash 'sha1 source) 0 8))))
    (expand-file-name asset-name asset-dir)))

(defun my/org-excalidraw--copy-asset (svg target)
  "Copy SVG to TARGET, creating the asset directory if needed."
  (make-directory (file-name-directory target) t)
  (copy-file svg target t))

(defun my/org-excalidraw--document-assets (source)
  "Return existing (TARGET . TRANSPARENT) assets for open notes using SOURCE."
  (let ((opaque (concat source ".svg"))
        assets)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'org-mode) buffer-file-name
                   (save-excursion
                     (goto-char (point-min))
                     (search-forward opaque nil t)))
          (let ((output (concat (file-name-sans-extension buffer-file-name) ".md")))
            (dolist (transparent '(nil t))
              (let ((target (my/org-excalidraw--asset-target source output transparent)))
                (when (file-exists-p target)
                  (cl-pushnew (cons target transparent) assets :test #'equal))))))))
    assets))

(defun my/org-excalidraw--sync-open-document-assets (source)
  "Copy completed SVG variants to existing assets for open notes using SOURCE."
  (dolist (asset (my/org-excalidraw--document-assets source))
    (my/org-excalidraw--copy-asset
     (concat source (if (cdr asset) ".transparent.svg" ".svg"))
     (car asset))))

(defun my/org-excalidraw--fix-image-symbols (svg)
  "Give pasted-image symbols in SVG a viewport for Emacs's SVG renderer."
  (with-temp-buffer
    (insert-file-contents svg)
    (goto-char (point-min))
    (let ((changed nil))
      (while (re-search-forward "<symbol\\( id=\"image-[^\"]+\"\\)>" nil t)
        (replace-match
         "<symbol\\1 viewBox=\"0 0 100 100\" preserveAspectRatio=\"none\">"
         nil nil)
        (setq changed t))
      (when changed
        (write-region (point-min) (point-max) svg nil 'silent)))))

(defun my/org-excalidraw--clean-source (path)
  "Snapshot active elements in PATH, leaving the editable source unchanged."
  (let ((clean-source (make-temp-file "excalidraw-export-" nil ".excalidraw")))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents path)
          (let* ((scene (json-parse-buffer :object-type 'hash-table :array-type 'array))
                 (elements (gethash "elements" scene)))
            (unless (vectorp elements)
              (error "Invalid Excalidraw elements in %s" path))
            (puthash "elements"
                     (vconcat (seq-remove
                               (lambda (element)
                                 (eq (gethash "isDeleted" element) t))
                               (append elements nil)))
                     scene)
            (erase-buffer)
            (insert (json-serialize scene)))
          (write-region (point-min) (point-max) clean-source nil 'silent)
          clean-source)
      (error
       (delete-file clean-source)
       (signal (car err) (cdr err))))))

(defun my/org-excalidraw--conversion-command (input output transparent)
  "Build direct exporter arguments for INPUT, OUTPUT and TRANSPARENT."
  (append (list "excalidraw-cli" "convert" input "--format" "svg")
          (when transparent '("--no-export-background"))
          (list "--output" output)))

(defun my/org-excalidraw--check-output (status svg)
  "Signal unless STATUS and SVG describe a successful, nonempty conversion."
  (unless (and (equal status 0)
               (file-readable-p svg)
               (> (file-attribute-size (file-attributes svg)) 0))
    (error "excalidraw-cli exited %s; SVG missing or empty: %s" status svg)))

(defun my/org-excalidraw--convert (path svg &optional transparent)
  "Synchronously export active elements in PATH to SVG for explicit exports."
  (when-let* ((job (gethash (expand-file-name path) my/org-excalidraw--export-jobs)))
    (setf (my/org-excalidraw--job-rerun job) t))
  (let ((input (my/org-excalidraw--clean-source path))
        output)
    (unwind-protect
        (progn
          (setq output (make-temp-file "excalidraw-svg-" nil ".svg"))
          (let* ((command (my/org-excalidraw--conversion-command input output transparent))
                 (status (apply #'call-process (car command) nil nil nil (cdr command))))
            (my/org-excalidraw--check-output status output)
            (my/org-excalidraw--fix-image-symbols output)
            (rename-file output svg t)))
      (delete-file input)
      (when (and output (file-exists-p output)) (delete-file output)))))

(defun my/org-excalidraw--ensure-svg (path &optional transparent force)
  "Ensure the SVG for PATH and TRANSPARENT is current, or regenerate with FORCE."
  (let ((svg (concat path (if transparent ".transparent.svg" ".svg"))))
    (when (or force (not (file-exists-p svg)) (file-newer-than-file-p path svg))
      (unless (executable-find "excalidraw-cli")
        (error "excalidraw-cli is required to export %s" path))
      (my/org-excalidraw--convert path svg transparent))
    svg))

(defun my/org-excalidraw--ensure-transparent-svg (path &optional force)
  "Ensure transparent SVG for PATH exists, regenerating with FORCE."
  (my/org-excalidraw--ensure-svg path t force))

(defun my/org-excalidraw--dispose-job (job &optional keep-output)
  "Remove JOB's temporary files, retaining its diagnostic buffer with KEEP-OUTPUT."
  (when (eq job (gethash (my/org-excalidraw--job-source job)
                         my/org-excalidraw--export-jobs))
    (remhash (my/org-excalidraw--job-source job) my/org-excalidraw--export-jobs))
  (dolist (file (list (my/org-excalidraw--job-input job)
                      (my/org-excalidraw--job-opaque job)
                      (my/org-excalidraw--job-transparent job)))
    (when (and file (file-exists-p file)) (delete-file file)))
  (unless keep-output
    (when (buffer-live-p (my/org-excalidraw--job-buffer job))
      (kill-buffer (my/org-excalidraw--job-buffer job)))))

(defun my/org-excalidraw--start-export (job &optional transparent)
  "Start JOB's opaque or TRANSPARENT conversion without invoking a shell."
  (when transparent
    (setf (my/org-excalidraw--job-transparent job)
          (make-temp-file "excalidraw-transparent-" nil ".svg")))
  (let ((process
         (make-process
          :name "excalidraw-export"
          :buffer (my/org-excalidraw--job-buffer job)
          :command (my/org-excalidraw--conversion-command
                    (my/org-excalidraw--job-input job)
                    (if transparent
                        (my/org-excalidraw--job-transparent job)
                      (my/org-excalidraw--job-opaque job))
                    transparent)
          :connection-type 'pipe :coding 'utf-8-unix :noquery t
          :sentinel #'my/org-excalidraw--export-finished)))
    (setf (my/org-excalidraw--job-process job) process)
    (process-put process 'excalidraw-job job)
    (process-put process 'excalidraw-transparent transparent)
    process))

(defun my/org-excalidraw--export-finished (process _event)
  "Publish only completed, current PROCESS output and refresh dependent assets."
  (when-let* ((job (process-get process 'excalidraw-job))
              (source (my/org-excalidraw--job-source job)))
    (when (and (memq (process-status process) '(exit signal))
               (eq job (gethash source my/org-excalidraw--export-jobs))
               (eq process (my/org-excalidraw--job-process job)))
      (let* ((transparent (process-get process 'excalidraw-transparent))
             (output (if transparent (my/org-excalidraw--job-transparent job)
                       (my/org-excalidraw--job-opaque job)))
             (rerun (my/org-excalidraw--job-rerun job)))
        (condition-case err
            (progn
              (my/org-excalidraw--check-output
               (if (eq (process-status process) 'exit)
                   (process-exit-status process) 'signal)
               output)
              (cond
               (rerun (my/org-excalidraw--dispose-job job))
               ((and (not transparent)
                     (cl-some #'cdr (my/org-excalidraw--document-assets source)))
                (my/org-excalidraw--fix-image-symbols output)
                (my/org-excalidraw--start-export job t))
               (t
                (my/org-excalidraw--fix-image-symbols output)
                (rename-file (my/org-excalidraw--job-opaque job) (concat source ".svg") t)
                (when transparent
                  (rename-file output (concat source ".transparent.svg") t))
                (my/org-excalidraw--refresh-previews (concat source ".svg"))
                (my/org-excalidraw--sync-open-document-assets source)
                (my/org-excalidraw--dispose-job job))))
          (error
           (my/org-excalidraw--dispose-job job t)
           (display-warning
            'org-excalidraw
            (format "Excalidraw export failed for %s: %s. See %s."
                    source (error-message-string err)
                    (buffer-name (my/org-excalidraw--job-buffer job)))
            :error)))
        (when rerun (my/org-excalidraw--export source))))))

(defun my/org-excalidraw--export (source)
  "Export SOURCE in the background, coalescing repeated saves into one rerun."
  (setq source (expand-file-name source))
  (remhash source my/org-excalidraw--export-timers)
  (when (file-remote-p source)
    (user-error "Excalidraw export requires a local source: %s" source))
  (unless (file-readable-p source)
    (user-error "Excalidraw source is not readable: %s" source))
  (unless (executable-find "excalidraw-cli")
    (user-error "Install excalidraw-cli to enable drawing exports"))
  (if-let* ((job (gethash source my/org-excalidraw--export-jobs)))
      (progn
        (setf (my/org-excalidraw--job-rerun job) t)
        (my/org-excalidraw--job-process job))
    (let ((job (my/org-excalidraw--make-job :source source)))
      (condition-case err
          (progn
            (setf (my/org-excalidraw--job-input job)
                  (my/org-excalidraw--clean-source source)
                  (my/org-excalidraw--job-opaque job)
                  (make-temp-file "excalidraw-svg-" nil ".svg")
                  (my/org-excalidraw--job-buffer job)
                  (generate-new-buffer
                   (format " *Excalidraw export: %s*" (file-name-nondirectory source))))
            (puthash source job my/org-excalidraw--export-jobs)
            (my/org-excalidraw--start-export job))
        (error
         (my/org-excalidraw--dispose-job job)
         (signal (car err) (cdr err)))))))

(provide 'config-excalidraw-export)
;;; config-excalidraw-export.el ends here
