;;; config-toggles.el --- Unified transient toggle menu -*- lexical-binding: t; -*-

(require 'transient)

;;; Theme cycling
(defvar my/cycle-themes '(modus-operandi modus-vivendi doom-one)
  "Themes to cycle through with `my/cycle-theme'.")

(defun my/cycle-theme ()
  "Cycle to the next theme in `my/cycle-themes'."
  (interactive)
  (let* ((current (car custom-enabled-themes))
         (pos (cl-position current my/cycle-themes))
         (next (nth (mod (1+ (or pos 0)) (length my/cycle-themes)) my/cycle-themes)))
    (load-theme next t)
    (message "Theme: %s" next)))

(transient-define-prefix my/toggles-transient ()
  "Toggle settings for notifications, compilation, and auto-save."
  [:description
   (lambda ()
     (format "Toggles  super-save:%s  ntfy:%s  compile:%s  auto-close:%s  theme:%s  gptel:%s"
             (if my/super-save-enabled "on" "off")
             (if my/notify-use-ntfy "on" "off")
             (if my/notify-on-compilation "on" "off")
             (if my/compilation-auto-close "on" "off")
             (symbol-name (or (car custom-enabled-themes) doom-theme))
             (my/gptel-backend-label)))
   ("s" "Toggle super-save"
    (lambda () (interactive)
      (setq my/super-save-enabled (not my/super-save-enabled))
      (message "Super-save: %s" (if my/super-save-enabled "ON" "OFF")))
    :transient t)
   ("c" "Toggle compile notifications"
    (lambda () (interactive)
      (setq my/notify-on-compilation (not my/notify-on-compilation))
      (message "Compile notifications: %s" (if my/notify-on-compilation "ON" "OFF")))
    :transient t)
   ("n" "Toggle ntfy push"
    (lambda () (interactive)
      (setq my/notify-use-ntfy (not my/notify-use-ntfy))
      (message "ntfy push: %s" (if my/notify-use-ntfy "ON" "OFF")))
    :transient t)
   ("t" "Send test notification"
    (lambda () (interactive)
      (alert "Happy hacking!" :title "Emacs" :severity 'normal)
      (when my/notify-use-ntfy
        (alert "Happy hacking!" :title "Emacs" :severity 'normal :style 'ntfy)))
    :transient t)
   ("a" "Toggle compilation auto-close"
    (lambda () (interactive)
      (setq my/compilation-auto-close (not my/compilation-auto-close))
      (message "Compilation auto-close: %s" (if my/compilation-auto-close "ON" "OFF")))
    :transient t)
   ("T" "Cycle theme (modus-operandi → modus-vivendi → doom-one)"
    (lambda () (interactive) (my/cycle-theme))
    :transient t)
   ("g" "Toggle gptel backend (primary ↔ Ollama)"
    (lambda () (interactive) (my/gptel-toggle-backend))
    :transient t)
   ("q" "Quit" transient-quit-one)])

(map! :leader
      (:prefix "t"
       :desc "Toggles" "n" #'my/toggles-transient))
