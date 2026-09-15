;;; config-gptel.el --- gptel: ChatGPT subscription, local Ollama, agent sessions -*- lexical-binding: t; -*-

;; Layers on top of Doom's `:tools llm' module, which already supplies gptel
;; itself, gptel-quick (explain at point), gptel-magit (commit messages),
;; ob-gptel (org-babel blocks), a popup rule and the `SPC o l' keymap. Nothing
;; here re-declares what that module owns — we add the parts it leaves open:
;;
;; - Auth. `gptel-make-openai-oauth' talks to the Codex endpoint on chatgpt.com
;;   using a ChatGPT Plus/Pro login, so the subscription is the credential and
;;   no OpenAI platform API key (billed separately) is involved. The token lands
;;   in ~/.config/emacs/.cache/gptel-openai/, not in auth-source.
;;
;; - A second backend on the managed Ollama endpoint from ADR-013. That daemon
;;   is on-demand, so switching to it checks the endpoint and points at
;;   `keeper llm start' rather than failing mid-request.
;;
;; - gptel-agent, which turns a buffer into a project-scoped agent: ~32 tools
;;   (read/grep/glob free, bash/edit/write confirmed), sub-agents read from
;;   md/org files, and skills picked up from ~/.claude/skills (an upstream
;;   compatibility path; the Claude Code CLI is not required). Sub-agent calls
;;   are routed to the local box so delegated grunt work costs no quota.
;;
;; - Persistence. `gptel-agent' builds a throwaway buffer; `my/gptel-project'
;;   puts the same session in <project>/.gptel/chat.org so it survives restarts.
;;   Those files are kept out of commits by the global gitignore that install.sh
;;   links to ~/.config/git/ignore.
;;
;; See ADR-014.

;;; --- Backends --------------------------------------------------------------

(defvar my/gptel-ollama-endpoint "http://localhost:11434"
  "Base URL of the managed Ollama endpoint (ADR-013).")

(defvar my/gptel-backend-openai nil
  "The ChatGPT Plus/Pro OAuth backend.")

(defvar my/gptel-backend-ollama nil
  "The local Ollama backend.")

(defvar my/gptel-openai-model 'gpt-5.6-sol
  "Model last used with `my/gptel-backend-openai'.
gptel advertises this one as the best for coding and agentic tasks;
`SPC o l m' switches, and this is only the starting point.")

(defvar my/gptel-ollama-model 'gpt-oss:20b
  "Model last used with `my/gptel-backend-ollama'.")

(defun my/gptel--ollama-up-p ()
  "Return non-nil if the Ollama endpoint answers."
  (eq 0 (call-process "curl" nil nil nil "-fsS" "-m" "2"
                      (concat my/gptel-ollama-endpoint "/api/tags"))))

(after! gptel
  (setq gptel-expert-commands t
        gptel-use-tools t)

  ;; Codex models reject a temperature parameter outright, so leave it unset.
  (setq gptel-temperature nil)

  (setq my/gptel-backend-openai (gptel-make-openai-oauth "ChatGPT"))
  ;; Models mirror scripts/ollama-models.txt; `keeper llm status' lists what is
  ;; actually pulled.
  (setq my/gptel-backend-ollama
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '(gpt-oss:20b qwen3-coder:30b qwen3.6:latest)))

  (setq gptel-backend my/gptel-backend-openai
        gptel-model   my/gptel-openai-model)

  ;; gptel writes these into a Local Variables block when a chat is saved;
  ;; marking them safe keeps reopening a transcript prompt-free.
  (dolist (v '(gptel-model gptel--backend-name gptel--bounds
               gptel-max-tokens gptel-temperature))
    (put v 'safe-local-variable #'always)))

(defun my/gptel--codex-p (&optional backend)
  "Return non-nil if BACKEND (default `gptel-backend') is the Codex endpoint.
Codex rejects `temperature' and `max_output_tokens', so callers that would
otherwise set them have to check first."
  (and (fboundp 'gptel-openai-oauth-p)
       (gptel-openai-oauth-p (or backend (bound-and-true-p gptel-backend)))))

(defun my/gptel-backend-label ()
  "Short name of the active gptel backend, or \"-\" before gptel loads."
  (if (bound-and-true-p gptel-backend)
      (gptel-backend-name gptel-backend)
    "-"))

;;;###autoload
(defun my/gptel-toggle-backend ()
  "Toggle `gptel-backend' between the ChatGPT subscription and local Ollama.
Remembers the model last used with each side."
  (interactive)
  (require 'gptel)
  (let ((to-ollama (not (eq gptel-backend my/gptel-backend-ollama))))
    (when (and to-ollama (not (my/gptel--ollama-up-p)))
      (user-error "Ollama is not answering at %s — run `keeper llm start'"
                  my/gptel-ollama-endpoint))
    (if to-ollama
        (setq my/gptel-openai-model gptel-model)
      (setq my/gptel-ollama-model gptel-model))
    (setq gptel-backend (if to-ollama my/gptel-backend-ollama my/gptel-backend-openai)
          gptel-model   (if to-ollama my/gptel-ollama-model my/gptel-openai-model))
    ;; An agent buffer carries a buffer-local 8192 that Codex refuses; drop it
    ;; on the way in, restore it on the way back out.
    (when (local-variable-p 'gptel-max-tokens)
      (setq-local gptel-max-tokens (unless (my/gptel--codex-p) 8192)))
    (message "gptel: %s / %s" (gptel-backend-name gptel-backend) gptel-model)))

;;; --- Quick lookups --------------------------------------------------------

;; `gptel-quick' (SPC o l e) is the zero-ceremony surface: sexp or region at
;; point, a sentence back in a posframe, `+' for more. Doom's `:tools llm'
;; installs and binds it but leaves the model unset, and that choice is the
;; difference between the command working and not.
;;
;; Upstream is explicit that reasoning models break it — they emit their
;; thinking ahead of the answer and the popup fills with that. Every Codex model
;; is a reasoning model, so the default backend here is exactly the case that
;; fails; it also warns on each request, since gptel-quick sizes the reply with
;; `gptel-max-tokens' and that endpoint rejects it. A local instruct model
;; answers a twelve-word lookup with none of it, off quota. The daemon is
;; on-demand, so when it is down we fall through to the session's own backend.
;;
;; The probe shells out to curl. Running it per invocation would put a
;; round-trip in front of a command whose appeal is that it costs nothing to
;; reach for, so one answer a minute stands in for all of them.

(defvar my/gptel-quick-model 'qwen3-coder:30b
  "Non-reasoning Ollama model to answer `gptel-quick' lookups with.")

(defvar my/gptel--ollama-probe nil
  "Cons of (TIME . RESULT) from the last `my/gptel--ollama-up-p' call.")

(defun my/gptel--ollama-up-cached-p (&optional ttl)
  "Return non-nil if Ollama answers, reusing a probe younger than TTL seconds."
  (let ((ttl (or ttl 60)))
    (unless (and my/gptel--ollama-probe
                 (time-less-p (time-since (car my/gptel--ollama-probe)) ttl))
      (setq my/gptel--ollama-probe
            (cons (current-time) (my/gptel--ollama-up-p))))
    (cdr my/gptel--ollama-probe)))

(defun my/gptel-quick--prefer-local (fn &rest args)
  "Run FN (`gptel-quick') against local Ollama when the daemon is up.
`gptel-quick' reads both variables synchronously when it builds the
request, so binding them around the call is enough."
  (if (my/gptel--ollama-up-cached-p)
      (let ((gptel-quick-backend my/gptel-backend-ollama)
            (gptel-quick-model   my/gptel-quick-model))
        (apply fn args))
    (apply fn args)))

;; The popup already has the escalation ladder: `+' for a longer answer, `M-w'
;; to copy, `M-RET' to carry the query and the answer into a gptel chat buffer
;; (org, `gptel-mode' on — so `C-x C-w' keeps it, bounds and backend included).
;; What it doesn't have is reachable keys. A GUI frame sends `M-<return>', and
;; Emacs only translates that to `M-RET' when nothing else claims it — org-mode
;; claims it for `org-ctrl-c-ret'. So upstream's follow-up key is dead in exactly
;; the buffers where a lookup most wants one, and works everywhere else, which
;; makes it read as flaky rather than broken.
;;
;; So: aliases onto upstream's own closures, no reimplementation. `r' and `w'
;; need no meta and no key translation at all, and a single char in an
;; `overriding-terminal-local-map' beats evil's own `r'/`w' while the popup is
;; up. Upstream builds the map inside its callback out of `cl-flet' closures, so
;; there is nothing to reach from outside — but it hands the finished map to
;; `set-transient-map' synchronously, and intercepting that is enough.

(defvar my/gptel-quick-key-aliases
  '(("r"          . "M-RET")                     ;continue in a chat buffer
    ("M-<return>" . "M-RET")                     ;what a GUI frame actually sends
    ("w"          . "<remap> <kill-ring-save>")) ;copy
  "Extra keys for the `gptel-quick' popup, as (KEY . EXISTING-KEY).
Each is bound to whatever upstream already bound EXISTING-KEY to.")

(defun my/gptel-quick--alias-keys (fn response info)
  "Alias `my/gptel-quick-key-aliases' in the `gptel-quick' popup's keymap.
FN is `gptel-quick--callback-posframe', called with RESPONSE and INFO."
  (cl-letf* ((install (symbol-function 'set-transient-map))
             ((symbol-function 'set-transient-map)
              (lambda (map &rest args)
                (when (keymapp map)
                  (pcase-dolist (`(,key . ,source) my/gptel-quick-key-aliases)
                    (when-let* ((cmd (keymap-lookup map source)))
                      (keymap-set map key cmd))))
                (apply install map args))))
    (funcall fn response info)))

(after! gptel-quick
  ;; Ten seconds suits a glance-and-forget popup, but the keymap dies with the
  ;; popup, and deciding to escalate takes longer than reading.
  (setq gptel-quick-timeout 20)
  (advice-add 'gptel-quick :around #'my/gptel-quick--prefer-local)
  (advice-add 'gptel-quick--callback-posframe :around #'my/gptel-quick--alias-keys))

(after! embark
  ;; Lookup as an action: `?' on a candidate, an identifier or a region.
  (keymap-set embark-general-map "?" #'gptel-quick))

;;; --- Agent ----------------------------------------------------------------

(defvar my/gptel-extra-tools '("keeper_health" "ollama_models")
  "Names of tools from gptel/tools.el to hand the agent alongside its own.")

(defun my/gptel--extend-agent-preset (&rest _)
  "Append `my/gptel-extra-tools' to the gptel-agent preset's tool list.
The preset's tools come from the bundled agents/gptel-agent.md, and
`gptel-agent-update' rebuilds it, so this runs after every update."
  (when-let* ((preset (gptel-get-preset 'gptel-agent))
              (tools  (plist-get preset :tools)))
    (plist-put preset :tools
               (append tools (cl-set-difference my/gptel-extra-tools tools
                                                :test #'equal)))))

(defun my/gptel--agent-without-max-tokens (fn &rest args)
  "Run FN (`gptel-agent') without letting it set `gptel-max-tokens' on Codex.
Upstream does `(unless gptel-max-tokens (setq-local gptel-max-tokens 8192))';
binding it non-nil for the duration makes that a no-op, so the fresh buffer
inherits the global nil instead of a value the endpoint refuses."
  (if (my/gptel--codex-p)
      (let ((gptel-max-tokens 'unset)) (apply fn args))
    (apply fn args)))

(use-package! gptel-preset-collection
  :after gptel)

(use-package! gptel-agent
  :after gptel
  :config
  ;; Our own sub-agents sit alongside the bundled executor/researcher/introspector.
  (add-to-list 'gptel-agent-dirs (expand-file-name "gptel/agents/" doom-user-dir))
  ;; Delegated sub-agent work runs locally instead of spending subscription quota.
  (setq gptel-agent-preset '(:backend "Ollama" :model qwen3-coder:30b))
  (load! "gptel/tools")
  (advice-add 'gptel-agent-update :after #'my/gptel--extend-agent-preset)
  (advice-add 'gptel-agent :around #'my/gptel--agent-without-max-tokens)
  (gptel-agent-update))

;;; --- Project sessions -----------------------------------------------------

(defvar my/gptel-project-file ".gptel/chat.org"
  "Transcript path, relative to the project root.")

(defun my/gptel--apply-agent-preset ()
  "Apply the `gptel-agent' preset to the current buffer.
Mirrors what `gptel-agent' does to its own buffer, so a file-visiting
chat gets the same tools and system prompt."
  (gptel-agent-update)
  (gptel--apply-preset 'gptel-agent
                       (lambda (sym val) (set (make-local-variable sym) val)))
  ;; gptel-agent raises this to 8192 because most backends default low. The
  ;; Codex endpoint rejects max_output_tokens outright and warns on every
  ;; request, so leave it unset there.
  (unless (or gptel-max-tokens (my/gptel--codex-p))
    (setq-local gptel-max-tokens 8192)))

;;;###autoload
(defun my/gptel-project ()
  "Open this project's persistent gptel chat, with the agent preset applied."
  (interactive)
  (require 'gptel-agent)
  (let* ((root (or (doom-project-root) default-directory))
         (file (expand-file-name my/gptel-project-file root))
         (new  (not (file-exists-p file))))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (setq-local default-directory root)
    (when new
      (insert (format "#+title: gptel — %s\n\n"
                      (file-name-nondirectory (directory-file-name root))))
      ;; Write it out straight away so the transcript exists on disk even if the
      ;; first question is never asked.
      (save-buffer))
    ;; `gptel-mode' restores state from org properties, and complains about a nil
    ;; backend when there are none yet. On a file we just created there is
    ;; nothing to restore, so that noise is meaningless — but keep it for
    ;; existing transcripts, where a failed restore is worth hearing about.
    (let ((inhibit-message new))
      (unless gptel-mode (gptel-mode 1)))
    (my/gptel--apply-agent-preset)
    (goto-char (point-max))))

;;;###autoload
(defun my/gptel-add-project-files (files)
  "Add FILES from the current project to gptel's context.
Interactively, select any number of them with completion."
  (interactive
   (let* ((project (project-current t))
          (root    (project-root project))
          (names   (mapcar (lambda (f) (file-relative-name f root))
                           (project-files project))))
     (list (mapcar (lambda (f) (expand-file-name f root))
                   (completing-read-multiple "Add to gptel context: " names nil t)))))
  (mapc #'gptel-add-file files)
  (message "gptel context: added %d file(s)" (length files)))

;;; --- Streaming-safe org buffers -------------------------------------------

;; A transcript is an org buffer that a machine appends to token by token, and
;; three ambient features assume a human typing into a settled file:
;;
;;   super-save fires on a 5s idle, which lands mid-stream; its before-save-hook
;;   runs ws-butler, which rewrites the trailing whitespace under the half-
;;   written heading; that edit fires `after-change-functions', and
;;   `org-element--cache-after-change' re-parses a buffer whose last element is
;;   still incomplete. `org-element--list-struct' can spin there forever, and
;;   because it is all on the main thread the daemon stops answering entirely —
;;   no timers, no input, `emacsclient' hangs. It reads as "gptel is hung".
;;
;; The cache buys nothing on a file only gptel writes, and neither does trimming
;; whitespace out from under it.

(defun my/gptel--tame-org-buffer-h ()
  "Keep whitespace and element-cache machinery off a live transcript."
  (when (derived-mode-p 'org-mode)
    (setq-local org-element-use-cache nil)
    (when (bound-and-true-p ws-butler-mode)
      (ws-butler-mode -1))))

(add-hook 'gptel-mode-hook #'my/gptel--tame-org-buffer-h)

(after! super-save
  ;; Streaming buffers save themselves when the response completes.
  (add-to-list 'super-save-predicates
               (lambda () (not (bound-and-true-p gptel-mode)))))

;;; --- Inline rewrites ------------------------------------------------------

(defun my/gptel-rewrite-auto-accept (overlay)
  "Accept a gptel rewrite as one isolated undo step.
OVERLAY is the completed rewrite supplied by `gptel-rewrite'."
  (with-current-buffer (overlay-buffer overlay)
    ;; The asynchronous callback is not an interactive command, so give its
    ;; delete+insert pair explicit boundaries and amalgamate it into one `u'.
    (undo-boundary)
    (with-undo-amalgamate
      (gptel--rewrite-accept overlay))
    (undo-boundary)
    (message "gptel rewrite applied — press `u' to undo")))

(defun my/gptel-rewrite-auto ()
  "Rewrite the selected region and apply the result automatically.
The replacement is one undo step; use `u' in Evil normal state to revert it."
  (interactive)
  (require 'gptel-rewrite)
  ;; The response arrives asynchronously, so this must outlive the command's
  ;; dynamic scope. Buffer-local state also keeps unrelated buffers independent.
  (setq-local gptel-rewrite-default-action #'my/gptel-rewrite-auto-accept)
  (call-interactively #'gptel-rewrite))

(defun my/gptel-rewrite-review ()
  "Rewrite the selected region using gptel's regular review-first flow."
  (interactive)
  (require 'gptel-rewrite)
  (setq-local gptel-rewrite-default-action nil)
  (call-interactively #'gptel-rewrite))

;;; --- Bindings -------------------------------------------------------------

;; Upstream ships no autoload cookie for this one, so the symbol is void until
;; something else pulls in gptel-context.el — the binding below fails with
;; `commandp' until you happen to have added context first. Same fix the module
;; uses for the gptel-org commands.
(autoload 'gptel-context-remove-all "gptel-context" nil t)

;; `:tools llm' already owns a e f l L s m r o O under this prefix. Override
;; lowercase r with optimistic auto-apply; uppercase R preserves its stock,
;; review-first rewrite flow.
(map! :desc "Rewrite (auto-apply)"
      :v "SPC r" #'my/gptel-rewrite-auto)

(map! :leader
      (:prefix "o"
               (:prefix "l"
                :desc "Rewrite (auto-apply)"   "r" #'my/gptel-rewrite-auto
                :desc "Rewrite (review)"       "R" #'my/gptel-rewrite-review
                :desc "Project chat (agent)"  "p" #'my/gptel-project
                :desc "Agent session"         "A" #'gptel-agent
                :desc "Add project files"     "F" #'my/gptel-add-project-files
                :desc "Clear context"         "c" #'gptel-context-remove-all
                :desc "Compact conversation"  "k" #'gptel-agent-compact
                :desc "Toggle backend"        "b" #'my/gptel-toggle-backend)))

;; Same surface without the leader. Free here:
;; `doom-localleader-alt-key' is only bound to C-c l in non-evil setups.
(defvar-keymap my/gptel-map
  :doc "gptel commands, mirroring the `SPC o l' leader map."
  "l" #'gptel
  "s" #'gptel-send
  "m" #'gptel-menu
  "e" #'gptel-quick
  "r" #'my/gptel-rewrite-auto
  "R" #'my/gptel-rewrite-review
  "a" #'gptel-add
  "f" #'gptel-add-file
  "p" #'my/gptel-project
  "A" #'gptel-agent
  "F" #'my/gptel-add-project-files
  "c" #'gptel-context-remove-all
  "k" #'gptel-agent-compact
  "b" #'my/gptel-toggle-backend)

(keymap-global-set "C-c l" my/gptel-map)

(provide 'config-gptel)
;;; config-gptel.el ends here
