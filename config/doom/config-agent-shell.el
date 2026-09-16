;;; config-agent-shell.el --- Pi project agents through ACP -*- lexical-binding: t; -*-

;; agent-shell is the full project-agent surface.  It talks to the existing Pi
;; installation through pi-acp, so models, OAuth, sessions, private layers, and
;; AGENTS.md all remain owned by ~/.pi rather than being duplicated in Emacs.

;; This command lives in agent-shell-pi.el rather than agent-shell.el, so make
;; the binding usable without eagerly loading the package at startup.
(autoload 'agent-shell-pi-start-agent "agent-shell-pi" nil t)

(use-package! agent-shell
  :commands agent-shell
  :init
  (setq agent-shell-preferred-agent-config 'pi
        agent-shell-pi-environment
        (list (concat "PI_CODING_AGENT_DIR="
                      (expand-file-name
                       (or (getenv "PI_CODING_AGENT_DIR") "~/.pi/agent"))))))

(provide 'config-agent-shell)
;;; config-agent-shell.el ends here
