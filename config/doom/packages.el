;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;; No packages needed for tags - using built-in xref + Universal Ctags

;; Devcontainer support (build, start, stop containers; compile inside them)
(package! devcontainer)

;; C# community snippet collection
(package! yasnippet-snippets)

(package! git-link)

;; Notifications (config-notify.el)
(package! alert)
(package! alert-toast)

;; Move lines/regions up and down
(package! drag-stuff)

;; Auto-save buffers on switch and idle
(package! super-save)

;; Flash window background on focus to show active window
(package! winpulse
  :recipe (:host github :repo "xenodium/winpulse"))

(package! exec-path-from-shell)


;; Mermaid diagrams — syntax highlighting + org-babel rendering
(package! mermaid-mode)
(package! ob-mermaid)
(package! md-mermaid
  :recipe (:host github :repo "ahmetus/md-mermaid"
           :files ("*.el" "*.json" "scripts"))
  :pin "e9c8b6a4393dcc28f5796d9cf6d69d7ff05198f2")

;; Excalidraw — freeform/hand-drawn canvas via `excalidraw:' org links.
;; Chrome PWA edits the JSON; fswatch → excalidraw_export → SVG inline. See ADR-008.
(package! org-excalidraw
  :recipe (:host github :repo "wdavew/org-excalidraw"))

;; Gnuplot — major mode + org-plot integration
;; :pre-build patches gnuplot-context.el: bare incf → cl-incf (Emacs 30 cl-lib compat)
(package! gnuplot
  :recipe (:pre-build (("perl" "-i" "-pe" "s/\\(incf /(cl-incf /g" "gnuplot-context.el"))))
(package! gnuplot-mode)
(package! spacious-padding)

;; Consult-powered org-roam search, backlinks, forward links, and previews.
(package! consult-org-roam)

;; Editable table browser on top of Emacs 29+'s built-in sqlite-mode.
(package! sqlite-mode-extras)

;; Named font presets (family + size bundles) with runtime switching and
;; persistence across sessions.
(package! fontaine)

;; Variable-pitch body text in prose buffers (org/markdown), with code blocks
;; staying monospace via the `fixed-pitch' face. `(org +pretty)' brings
;; org-modern but not this — declare it explicitly.
(package! mixed-pitch)

;; Centered, measure-limited body text in the same prose buffers. mixed-pitch
;; chooses the face; olivetti chooses the column width.
(package! olivetti)

;; `:tools llm' pins gptel to a release commit; gptel-agent tracks gptel master.
(unpin! gptel)

;; Agent preset for gptel: project-scoped sessions, ~32 tools, sub-agents as
;; md/org files. Complements what `:tools llm' already gives us (chat, rewrite,
;; gptel-magit commit messages, ob-gptel blocks). See ADR-014.
;; `:files' mirrors the upstream MELPA recipe — without "agents" the bundled
;; sub-agent definitions are not installed.
(package! gptel-agent
  :recipe (:host github :repo "karthink/gptel-agent"
           :files (:defaults "agents")))

;; TLA+ — tree-sitter editing + compile-mode error patterns for TLC/PlusCal/SANY
;; tla-tools needs polymode for its mixed TLA+/PlusCal buffer mode.
(package! polymode)
(package! tla-ts-mode
  :recipe (:host github :repo "Davidbrcz/tla-ts-mode"))
(package! tla-tools
  :recipe (:host github :repo "mrc/tla-tools"))
(package! gptel-preset-collection
  :recipe (:host github :repo "karthink/gptel-preset-collection"))
