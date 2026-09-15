;;; config-jsonviz.el --- Visualize JSON with PlantUML -*- lexical-binding: t; -*-

;; Revives the useful part of the pre-Doom jsonviz-plantuml helper: validate and
;; pretty-print a region (or buffer), wrap it in PlantUML's @startjson syntax,
;; then preview or export it. PlantUML remains local; no JSON leaves the machine.

(require 'json)

(defgroup my/jsonviz nil
  "Render JSON data through PlantUML."
  :group 'tools)

(defcustom my/jsonviz-theme "hacker"
  "PlantUML theme used for JSON diagrams, or nil for no explicit theme."
  :type '(choice (const :tag "No theme" nil) string)
  :group 'my/jsonviz)

;; Doom's enabled `:lang rest +jq' module owns the jq-mode package. Expose its
;; most useful command in ordinary JSON buffers: iteratively filter the active
;; region, or the entire buffer when no region is selected.
(autoload 'jq-interactively "jq-mode" nil t)

(defun my/jsonviz--source ()
  "Return the active region, or the entire current buffer, as plain text."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun my/jsonviz--pretty (source)
  "Validate and pretty-print JSON SOURCE, signaling a friendly user error."
  (with-temp-buffer
    (insert source)
    (condition-case err
        (progn
          (goto-char (point-min))
          (json-parse-buffer)
          (json-pretty-print-buffer)
          (buffer-substring-no-properties (point-min) (point-max)))
      (json-parse-error
       (user-error "Invalid JSON: %s" (error-message-string err)))
      (error
       (user-error "Could not prepare JSON: %s" (error-message-string err))))))

(defun my/jsonviz--plantuml-source ()
  "Build a PlantUML @startjson document from the current region or buffer."
  (concat "@startjson\n"
          (when my/jsonviz-theme (format "!theme %s\n" my/jsonviz-theme))
          (my/jsonviz--pretty (my/jsonviz--source))
          "\n@endjson\n"))

;;;###autoload
(defun my/jsonviz-preview (&optional prefix)
  "Preview JSON at region or buffer through PlantUML.
With PREFIX, use PlantUML's alternate display target."
  (interactive "p")
  (require 'plantuml-mode)
  (plantuml-preview-string (or prefix 1) (my/jsonviz--plantuml-source)))

(defun my/jsonviz--export-with-jar (source output type)
  "Render PlantUML SOURCE with its configured jar into OUTPUT of TYPE."
  (unless (and (boundp 'plantuml-jar-path)
               (file-readable-p plantuml-jar-path))
    (user-error "PlantUML jar not found; run M-x plantuml-download-jar"))
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'utf-8-unix)
        (output-buffer (generate-new-buffer " *jsonviz-export*")))
    (unwind-protect
        (progn
          ;; PlantUML writes binary PNG/SVG bytes to stdout in -pipe mode.
          (with-current-buffer output-buffer
            (set-buffer-multibyte nil))
          (with-temp-buffer
            (insert source)
            (let ((status (call-process-region
                           (point-min) (point-max)
                           (or (and (boundp 'plantuml-java-command)
                                    plantuml-java-command)
                               "java")
                           nil output-buffer nil
                           "-jar" (expand-file-name plantuml-jar-path)
                           (format "-t%s" type) "-pipe")))
              (unless (zerop status)
                (user-error "PlantUML export failed (exit %s): %s"
                            status
                            (with-current-buffer output-buffer
                              (buffer-string))))
              (with-current-buffer output-buffer
                (let ((coding-system-for-write 'no-conversion))
                  (write-region (point-min) (point-max) output nil 'silent))))))
      (kill-buffer output-buffer))))

;;;###autoload
(defun my/jsonviz-export (output)
  "Export JSON at region or buffer to PNG or SVG file OUTPUT."
  (interactive
   (list (read-file-name "Export JSON diagram: " nil nil nil "json.svg")))
  (require 'plantuml-mode)
  (let* ((extension (downcase (or (file-name-extension output) "svg")))
         (output (if (file-name-extension output)
                     (expand-file-name output)
                   (expand-file-name (concat output ".svg")))))
    (unless (member extension '("png" "svg"))
      (user-error "Output must end in .png or .svg"))
    (make-directory (file-name-directory output) t)
    (my/jsonviz--export-with-jar
     (my/jsonviz--plantuml-source) output extension)
    (find-file-other-window output)
    (message "Exported JSON diagram to %s" output)))

(after! json-mode
  (map! :map json-mode-map
        :localleader
        :desc "Filter with jq"     "q" #'jq-interactively
        :desc "Preview structure" "v" #'my/jsonviz-preview
        :desc "Export structure"  "V" #'my/jsonviz-export))

(after! json-ts-mode
  (map! :map json-ts-mode-map
        :localleader
        :desc "Filter with jq"     "q" #'jq-interactively
        :desc "Preview structure" "v" #'my/jsonviz-preview
        :desc "Export structure"  "V" #'my/jsonviz-export))

(provide 'config-jsonviz)
;;; config-jsonviz.el ends here
