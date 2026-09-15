;;; config-notify.el --- alert.el + ntfy.sh notifications -*- lexical-binding: t; -*-

;; Toggle state — top-level so other files (config-toggles.el) can reference them
(defvar my/notify-on-compilation nil
  "When non-nil, send alerts on compilation finish.")
(defvar my/notify-use-ntfy nil
  "When non-nil, push to ntfy.sh in addition to local notifications.")

(use-package! alert
  :config
  ;; Custom ntfy.sh style using Emacs-native url-retrieve (async, no curl)
  (defvar my/ntfy-topic (getenv "NTFY_TOPIC")
    "ntfy.sh topic for Emacs notifications, or nil when unconfigured.")

  (alert-define-style 'ntfy
                      :title "ntfy.sh push notification"
                      :notifier (lambda (info)
                                  (unless my/ntfy-topic
                                    (user-error "Set NTFY_TOPIC before enabling ntfy notifications"))
                                  (let* ((title (or (plist-get info :title) "Emacs"))
                                         (body (plist-get info :message))
                                         (priority (pcase (plist-get info :severity)
                                                     ('urgent "5") ('high "4") ('moderate "3")
                                                     (_ "2")))
                                         (url-request-method "POST")
                                         (url-request-extra-headers
                                          `(("Title" . ,title)
                                            ("Priority" . ,priority)
                                            ("Tags" . "emacs")))
                                         (url-request-data (encode-coding-string body 'utf-8)))
                                    (message "[ntfy] Sending: %s — %s" title body)
                                    (url-retrieve
                                     (format "https://ntfy.sh/%s" my/ntfy-topic)
                                     (lambda (status)
                                       (if (plist-get status :error)
                                           (message "[ntfy] FAILED: %S" (plist-get status :error))
                                         (message "[ntfy] Sent OK to topic '%s'" my/ntfy-topic))
                                       (when (buffer-live-p (current-buffer))
                                         (kill-buffer (current-buffer))))
                                     nil t t))))  ; silent, inhibit-cookies

  ;; Compilation hook
  (defun my/alert-compilation-finish (buf status)
    "Send alert when compilation finishes, if enabled."
    (when my/notify-on-compilation
      (let* ((clean-status (string-trim status))
             (success (string-match-p "finished" clean-status))
             (project (or (when-let ((proj (project-current nil)))
                            (project-name proj))
                          (file-name-nondirectory
                           (directory-file-name default-directory))))
             (cmd (with-current-buffer buf
                    (bound-and-true-p compile-command)))
             (title (format "[%s] Compilation %s" project
                            (if success "succeeded" "FAILED")))
             (body (or cmd clean-status))
             (severity (if success 'normal 'high)))
        (message "[notify] %s: %s" title body)
        (alert body :title title :severity severity)
        (when my/notify-use-ntfy
          (alert body :title title :severity severity :style 'ntfy)))))

  (add-hook 'compilation-finish-functions #'my/alert-compilation-finish))
