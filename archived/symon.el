;;; archived/symon.el --- System monitor sparklines (shelved)
;;
;; Shelved: 2026-04-22. Too much maintenance surface for the value.
;; symon is disabled on macOS (no useful monitors). Linux had no
;; custom config. Windows required a bespoke PowerShell backend
;; (scripts/symon-perf.ps1) to work around broken built-in monitors.
;;
;; To restore: add packages back to packages.el, paste config blocks
;; back into config.el (shared) and config-windows.el (Windows monitors).
;;
;; See decisions/007-archived-packages.md

;;; packages.el entry:
;; (package! symon :disable (featurep :system 'macos))

;;; config.el entry (shared):
;; ;;; symon — system monitor (monitors configured per-platform)
;; (use-package! symon
;;   :demand t)

;;; config-windows.el entry (Windows-only — full custom backend):

;; (after! symon
;;   (defvar my/symon--perf-proc nil)
;;   (defvar my/symon--cpu-value nil)
;;   (defvar my/symon--mem-value nil)
;;   (defvar my/symon--rx-value nil)
;;   (defvar my/symon--tx-value nil)
;;   (defvar my/symon--cpu-history nil)
;;   (defvar my/symon--mem-history nil)
;;   (defvar my/symon--rx-history nil)
;;   (defvar my/symon--tx-history nil)
;;   (defconst my/symon--sparkline-chars [?▁ ?▂ ?▃ ?▄ ?▅ ?▆ ?▇ ?█])
;;   (defconst my/symon--sparkline-len 20)

;;   (defun my/symon--text-sparkline (history &optional max-val)
;;     "Render HISTORY (newest-first list) as a Unicode sparkline.
;; MAX-VAL is the ceiling (default 100); network monitors pass the peak value."
;;     (if (null history) ""
;;       (let ((ceil (or max-val 100)))
;;         (apply #'string
;;                (mapcar (lambda (v)
;;                          (aref my/symon--sparkline-chars
;;                                (min 7 (max 0 (floor (* 8 v) (max 1 (1+ ceil)))))))
;;                        (reverse history))))))

;;   (defun my/symon--start-perf ()
;;     "Start the PowerShell perf helper (direct make-process, no shell)."
;;     (unless (and my/symon--perf-proc (process-live-p my/symon--perf-proc))
;;       (setq my/symon--cpu-value nil
;;             my/symon--mem-value nil
;;             my/symon--rx-value nil
;;             my/symon--tx-value nil
;;             my/symon--cpu-history nil
;;             my/symon--mem-history nil
;;             my/symon--rx-history nil
;;             my/symon--tx-history nil
;;             my/symon--perf-proc
;;             (make-process
;;              :name "symon-perf"
;;              :command (list "powershell.exe" "-NoProfile" "-ExecutionPolicy" "Bypass"
;;                             "-File"
;;                             (expand-file-name "../scripts/symon-perf.ps1"
;;                                               (file-truename doom-user-dir))
;;                             "-Interval" (number-to-string symon-refresh-rate))
;;              :noquery t
;;              :connection-type 'pipe
;;              :filter (lambda (_proc output)
;;                        (when (string-match
;;                               "cpu:\\([0-9]+\\) mem:\\([0-9]+\\) rx:\\([0-9]+\\) tx:\\([0-9]+\\)"
;;                               output)
;;                          (let ((cpu (string-to-number (match-string 1 output)))
;;                                (mem (string-to-number (match-string 2 output)))
;;                                (rx  (string-to-number (match-string 3 output)))
;;                                (tx  (string-to-number (match-string 4 output))))
;;                            (setq my/symon--cpu-value cpu
;;                                  my/symon--mem-value mem
;;                                  my/symon--rx-value rx
;;                                  my/symon--tx-value tx)
;;                            (push cpu my/symon--cpu-history)
;;                            (push mem my/symon--mem-history)
;;                            (push rx my/symon--rx-history)
;;                            (push tx my/symon--tx-history)
;;                            (dolist (sym '(my/symon--cpu-history my/symon--mem-history
;;                                           my/symon--rx-history my/symon--tx-history))
;;                              (when (> (length (symbol-value sym)) my/symon--sparkline-len)
;;                                (set sym (seq-take (symbol-value sym) my/symon--sparkline-len))))))))))

;;   (defun my/symon--stop-perf ()
;;     "Kill the PowerShell perf helper."
;;     (when (and my/symon--perf-proc (process-live-p my/symon--perf-proc))
;;       (kill-process my/symon--perf-proc)
;;       (setq my/symon--perf-proc nil)))

;;   (define-symon-monitor my/symon-w32-cpu-monitor
;;                         :index "CPU:" :unit "%"
;;                         :setup   (my/symon--start-perf)
;;                         :cleanup (my/symon--stop-perf)
;;                         :fetch my/symon--cpu-value
;;                         :display (format "CPU:%s %s"
;;                                          (if my/symon--cpu-value
;;                                              (format "%d%%" my/symon--cpu-value) "N/A")
;;                                          (my/symon--text-sparkline my/symon--cpu-history)))

;;   (define-symon-monitor my/symon-w32-mem-monitor
;;                         :index "MEM:" :unit "%"
;;                         :setup   (my/symon--start-perf)
;;                         :cleanup (my/symon--stop-perf)
;;                         :fetch my/symon--mem-value
;;                         :display (format "MEM:%s %s"
;;                                          (if my/symon--mem-value
;;                                              (format "%d%%" my/symon--mem-value) "N/A")
;;                                          (my/symon--text-sparkline my/symon--mem-history)))

;;   (define-symon-monitor my/symon-w32-bat-monitor
;;                         :index "BAT:" :unit "%"
;;                         :fetch (when battery-status-function
;;                                  (let ((pct (cdr (assq ?p (funcall battery-status-function)))))
;;                                    (when (and pct (not (string= pct "N/A")))
;;                                      (string-to-number pct))))
;;                         :display (let ((pct (when battery-status-function
;;                                               (cdr (assq ?p (funcall battery-status-function))))))
;;                                    (if (and pct (not (string= pct "N/A")))
;;                                        (format "BAT:%s%%" pct)
;;                                      "")))

;;   (define-symon-monitor my/symon-w32-rx-monitor
;;                         :index "RX:" :unit "KB/s"
;;                         :setup   (my/symon--start-perf)
;;                         :cleanup (my/symon--stop-perf)
;;                         :fetch my/symon--rx-value
;;                         :display (let ((peak (if my/symon--rx-history
;;                                                  (apply #'max my/symon--rx-history) 1)))
;;                                    (format "RX:%s %s"
;;                                            (if my/symon--rx-value
;;                                                (format "%dKB/s" my/symon--rx-value) "N/A")
;;                                            (my/symon--text-sparkline my/symon--rx-history peak))))

;;   (define-symon-monitor my/symon-w32-tx-monitor
;;                         :index "TX:" :unit "KB/s"
;;                         :setup   (my/symon--start-perf)
;;                         :cleanup (my/symon--stop-perf)
;;                         :fetch my/symon--tx-value
;;                         :display (let ((peak (if my/symon--tx-history
;;                                                  (apply #'max my/symon--tx-history) 1)))
;;                                    (format "TX:%s %s"
;;                                            (if my/symon--tx-value
;;                                                (format "%dKB/s" my/symon--tx-value) "N/A")
;;                                            (my/symon--text-sparkline my/symon--tx-history peak))))

;;   (setq symon-monitors '(my/symon-w32-cpu-monitor
;;                          my/symon-w32-mem-monitor
;;                          my/symon-w32-bat-monitor
;;                          my/symon-w32-rx-monitor
;;                          my/symon-w32-tx-monitor))

;;   (defun symon--display-update ()
;;     "Update symon display with horizontal centering."
;;     (unless (or cursor-in-echo-area (active-minibuffer-window))
;;       (let ((message-log-max nil)
;;             (page 0))
;;         (dolist (lst symon--display-fns)
;;           (if (= page symon--active-page)
;;               (let* ((text (apply #'concat (mapcar #'funcall lst)))
;;                      (pad (max 0 (/ (- (frame-width) (string-width text)) 2))))
;;                 (message "%s" (concat (make-string pad ?\s) text)))
;;             (mapc #'funcall lst))
;;           (setq page (1+ page))))
;;       (setq symon--display-active t))))
