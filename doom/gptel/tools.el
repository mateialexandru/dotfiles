;;; gptel/tools.el --- Machine-specific gptel tools -*- lexical-binding: t; -*-

;; Loaded from config-gptel.el after gptel-agent.
;;
;; gptel-agent already ships the general set — Read, Grep, Glob, Bash, Edit,
;; Write, WebSearch, WebFetch, Eval, plus ~16 Emacs introspection tools. Don't
;; duplicate those here. This file is for tools about *this machine*: things
;; only the dotfiles know how to answer.
;;
;; To add one, copy the shape below. `:confirm t' makes gptel ask before running
;; it — set it on anything that mutates state; the tools here only read.
;;
;;   (gptel-make-tool
;;    :name "snake_case_name"
;;    :function (lambda (arg) ...)     ; returns a string
;;    :description "What it does, for the model."
;;    :args (list '(:name "arg" :type string :description "..."))
;;    :category "dotfiles")
;;
;; Then add the name to `my/gptel-extra-tools' in config-gptel.el so the agent
;; preset picks it up.

(defun my/gptel--sh (&rest args)
  "Run ARGS, returning combined output as a string."
  (with-temp-buffer
    (let ((exit (apply #'call-process (car args) nil t nil (cdr args))))
      (format "exit %s\n%s" exit (buffer-string)))))

(gptel-make-tool
 :name "keeper_health"
 :function (lambda ()
             (my/gptel--sh "just"
                           "-f" (expand-file-name "~/Source/dotfiles/justfile")
                           "-d" (expand-file-name "~/Source/dotfiles")
                           "health"))
 :description "Run the dotfiles confidence pass (`keeper health'): doom symlink, \
Emacs daemon and its environment, native-comp queue, doom doctor, core tools, \
LSP servers, formatters, org-protocol handler, excalidraw toolchain, Ollama and \
ssh ControlMaster. Use this when asked why the Emacs or dotfiles setup is \
misbehaving. Takes a while; it is read-only."
 :args nil
 :category "dotfiles")

(gptel-make-tool
 :name "ollama_models"
 :function (lambda ()
             (if (eq 0 (call-process "curl" nil nil nil "-fsS" "-m" "2"
                                     "http://localhost:11434/api/tags"))
                 (my/gptel--sh "ollama" "list")
               "Ollama is not running. Start it with `keeper llm start'."))
 :description "List the local LLM models pulled into Ollama on this machine, or \
report that the on-demand daemon is stopped. The manifest of models that \
*should* be present is scripts/ollama-models.txt in the dotfiles repo."
 :args nil
 :category "dotfiles")

(provide 'gptel-tools)
;;; gptel/tools.el ends here
