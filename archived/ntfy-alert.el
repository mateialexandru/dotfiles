;;; archived/ntfy-alert.el --- alert.el + ntfy.sh notification system (shelved)
;;
;; Shelved: 2026-04-22. The compile notification hook and ntfy.sh push
;; were rarely used in practice. More complexity than value.
;;
;; To restore: add packages back to packages.el, paste config blocks back
;; into config.el (shared), config-linux.el, and config-windows.el.
;;
;; See decisions/007-archived-packages.md

;;; packages.el entries:
;; (package! alert)
;; (package! alert-toast)

;;; config.el entry (shared):

;; (use-package! alert
;;   :config
;;   (defvar my/ntfy-topic (getenv "NTFY_TOPIC")
;;     "ntfy.sh topic for Emacs notifications.")
;;
;;   (alert-define-style 'ntfy
;;                       :title "ntfy.sh push notification"
;;                       :notifier (lambda (info)
;;                                   (let* ((title (or (plist-get info :title) "Emacs"))
;;                                          (body (plist-get info :message))
;;                                          (priority (pcase (plist-get info :severity)
;;                                                      ('urgent "5") ('high "4") ('moderate "3")
;;                                                      (_ "2")))
;;                                          (url-request-method "POST")
;;                                          (url-request-extra-headers
;;                                           `(("Title" . ,title)
;;                                             ("Priority" . ,priority)
;;                                             ("Tags" . "emacs")))
;;                                          (url-request-data (encode-coding-string body 'utf-8)))
;;                                     (message "[ntfy] Sending: %s — %s" title body)
;;                                     (url-retrieve
;;                                      (format "https://ntfy.sh/%s" my/ntfy-topic)
;;                                      (lambda (status)
;;                                        (if (plist-get status :error)
;;                                            (message "[ntfy] FAILED: %S" (plist-get status :error))
;;                                          (message "[ntfy] Sent OK to topic '%s'" my/ntfy-topic))
;;                                        (when (buffer-live-p (current-buffer))
;;                                          (kill-buffer (current-buffer))))
;;                                      nil t t))))
;;
;;   (defvar my/notify-on-compilation nil)
;;   (defvar my/notify-use-ntfy nil)
;;
;;   (defun my/alert-compilation-finish (buf status)
;;     (when my/notify-on-compilation
;;       (let* ((clean-status (string-trim status))
;;              (success (string-match-p "finished" clean-status))
;;              (project (or (when-let ((proj (project-current nil)))
;;                             (project-name proj))
;;                           (file-name-nondirectory
;;                            (directory-file-name default-directory))))
;;              (cmd (with-current-buffer buf
;;                     (bound-and-true-p compile-command)))
;;              (title (format "[%s] Compilation %s" project
;;                             (if success "succeeded" "FAILED")))
;;              (body (or cmd clean-status))
;;              (severity (if success 'normal 'high)))
;;         (message "[notify] %s: %s" title body)
;;         (alert body :title title :severity severity)
;;         (when my/notify-use-ntfy
;;           (alert body :title title :severity severity :style 'ntfy)))))
;;
;;   (add-hook 'compilation-finish-functions #'my/alert-compilation-finish)
;;
;;   (require 'transient)
;;   (transient-define-prefix my/notify-transient ()
;;     "Notification settings."
;;     [:description
;;      (lambda ()
;;        (format "Notifications  ntfy:%s  compile:%s"
;;                (if my/notify-use-ntfy "on" "off")
;;                (if my/notify-on-compilation "on" "off")))
;;      ("c" "Toggle compile notifications"
;;       (lambda () (interactive)
;;         (setq my/notify-on-compilation (not my/notify-on-compilation))
;;         (message "Compile notifications: %s" (if my/notify-on-compilation "ON" "OFF"))))
;;      ("n" "Toggle ntfy push"
;;       (lambda () (interactive)
;;         (setq my/notify-use-ntfy (not my/notify-use-ntfy))
;;         (message "ntfy push: %s" (if my/notify-use-ntfy "ON" "OFF"))))
;;      ("t" "Send test notification"
;;       (lambda () (interactive)
;;         (alert "Happy hacking!" :title "Emacs" :severity 'normal)
;;         (when my/notify-use-ntfy
;;           (alert "Happy hacking!" :title "Emacs" :severity 'normal :style 'ntfy))))
;;      ("q" "Quit" transient-quit-one)])
;;
;;   (map! :leader
;;         (:prefix ("t" . "toggle")
;;          :desc "Notifications" "n" #'my/notify-transient)))

;;; config-linux.el entry:
;; (after! alert
;;   (setq alert-default-style 'libnotify))

;;; config-windows.el entry:
;; (use-package! alert-toast
;;   :after alert
;;   :config
;;   (setq alert-default-style 'toast))
