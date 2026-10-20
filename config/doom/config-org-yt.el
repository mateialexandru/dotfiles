;;; config-org-yt.el -*- lexical-binding: t; -*-

;; Keep YouTube links on Org's current preview API.  The older org-yt
;; package advises and redefines Org's former inline-image functions.
(defun my/org-yt-valid-id-p (id)
  "Return non-nil when ID has the shape of a YouTube video ID."
  (and (stringp id)
       (string-match-p "\\`[-_[:alnum:]]\\{11\\}\\'" id)))

(defun my/org-yt-follow (id _arg)
  "Open the YouTube video with ID in a browser."
  (unless (my/org-yt-valid-id-p id)
    (user-error "Invalid YouTube video ID: %s" id))
  (browse-url (concat "https://www.youtube.com/watch?v=" id)))

(defun my/org-yt-open-at-mouse (event)
  "Open the YouTube link beneath mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (org-open-at-point))

(defun my/org-yt-thumbnail-file (id)
  "Return the cached thumbnail for video ID, downloading it if needed."
  (let ((file (expand-file-name (format "youtube.%s.mq.jpg" id) +org-preview-dir))
        (fallback (expand-file-name (format "youtube.%s.jpg" id) +org-preview-dir)))
    (unless (file-exists-p file)
      (make-directory +org-preview-dir t)
      (condition-case err
          (url-copy-file (format "https://img.youtube.com/vi/%s/mqdefault.jpg" id)
                         file t)
        (error (message "YouTube thumbnail unavailable: %s"
                        (error-message-string err)))))
    (cond ((and (file-regular-p file)
                (> (file-attribute-size (file-attributes file)) 0)) file)
          ((and (file-regular-p fallback)
                (> (file-attribute-size (file-attributes fallback)) 0)) fallback))))

(defun my/org-yt-preview (ov id link)
  "Show a cached thumbnail for video ID in Org overlay OV for LINK."
  (when (and (display-graphic-p) (my/org-yt-valid-id-p id))
    (when-let* ((file (my/org-yt-thumbnail-file id)))
      (let ((org-image-actual-width 320)
            (org-image-align nil))
        (when (org-link-preview-file ov file link)
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map image-map)
            (define-key map [mouse-1] #'my/org-yt-open-at-mouse)
            (overlay-put ov 'keymap map)
            (overlay-put ov 'mouse-face 'highlight)
            (overlay-put ov 'help-echo "mouse-1: watch on YouTube"))
          t)))))

(defun my/org-yt-export (id desc backend info)
  "Export YouTube ID as a title link or a thumbnail in HTML or Markdown.
DESC selects a text link; a bare link becomes a thumbnail."
  (when (or (org-export-derived-backend-p backend 'html)
            (org-export-derived-backend-p backend 'md))
    (unless (my/org-yt-valid-id-p id)
      (user-error "Invalid YouTube video ID: %s" id))
    (require 'ox-html)
    (let* ((url (concat "https://www.youtube.com/watch?v=" id))
           (file (my/org-yt-thumbnail-file id))
           (output (plist-get info :output-file))
           (src (if (and file output)
                    (let* ((output (expand-file-name output))
                           (asset-dir
                            (expand-file-name
                             (concat (downcase (file-name-base output)) "-assets")
                             (file-name-directory output)))
                           (target (expand-file-name
                                    (format "youtube-%s.jpg" id) asset-dir)))
                      (make-directory asset-dir t)
                      (copy-file file target t)
                      (url-encode-url
                       (file-relative-name
                        target (file-name-directory (expand-file-name output)))))
                  (format "https://img.youtube.com/vi/%s/0.jpg" id))))
      (if (org-export-derived-backend-p backend 'md)
          (if desc
              (format "[%s](%s)" desc url)
            (format "[![YouTube video thumbnail](%s)](%s)" src url))
        (if desc
            (format "<a href=\"%s\">%s</a>" url desc)
          (let ((image (org-html--format-image
                        src
                        '(:alt "YouTube video thumbnail" :loading "lazy"
                          :style "display: block; width: 480px; max-width: 100%; aspect-ratio: 16 / 9; object-fit: cover; border-radius: 8px;")
                        info)))
            (format "<a href=\"%s\" aria-label=\"Watch on YouTube\">%s</a>"
                    url image)))))))

(defun my/org-yt-refresh-after-revert ()
  "Refresh YouTube thumbnails after an Org buffer is reverted."
  (when (and (derived-mode-p 'org-mode)
             (display-graphic-p)
             (save-excursion
               (goto-char (point-min))
               (search-forward "[[yt:" nil t)))
    (org-link-preview-refresh)))

(after! org
  (org-link-set-parameters "yt"
                           :follow #'my/org-yt-follow
                           :preview #'my/org-yt-preview
                           :export #'my/org-yt-export)
  (add-hook 'after-revert-hook #'my/org-yt-refresh-after-revert))
