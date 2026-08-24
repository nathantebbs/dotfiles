;;; rc-programming.el --- Language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes and per-language settings. Formatting is apheleia's job in
;; rc-editing, so only what a language needs beyond its own package is here.
;;
;; Two languages take a file of their own. Odin is in rc-odin, since its major
;; mode is written rather than installed, and C and C++ are in rc-cc, along
;; with the CMake that builds them, since they are the only ones that run a
;; language server.
;;
;; Bindings that live in a mode's own keymap go behind with-eval-after-load,
;; since the map does not exist until the mode's package loads.

;;; Code:

(require 'treesit)

;;; Python

;; Homebrew installs python3 and no bare `python'. Emacs 30 already defaults
;; both of these to python3; stating them keeps an older one from going looking
;; for a Python 2 that is not there either.
(setopt python-shell-interpreter "python3")
(setopt python-interpreter "python3")

;; python.el registers its own commit-pinned grammar source when it loads, so
;; requiring it is what makes the grammar installable. Stating a source here
;; would only add a second, unpinned entry racing that one.
(defun rc-programming-install-python-grammar ()
  "Compile and install the Python tree-sitter grammar."
  (interactive)
  (require 'python)
  (treesit-install-language-grammar 'python))

;; python.el's autoloads have already put python-mode in
;; `treesit-major-mode-remap-alist', so enabling the mode is all that is left.
;; Appended rather than assigned, so another module can enable its own modes
;; through the same option.
;;
;; Guarded on the grammar, so a fresh clone falls back to `python-mode' rather
;; than sitting in a python-ts-mode with no font lock.
(when (treesit-ready-p 'python t)
  (setopt treesit-enabled-modes
          (append treesit-enabled-modes '(python-ts-mode))))

(add-hook 'python-base-mode-hook #'pyvenv-mode)

;; uv puts the environment at .venv in the project root and does not export
;; anything, so nothing in a GUI Emacs would otherwise know it exists.
;; Activating it is what puts the project's own interpreter and its ruff on
;; `exec-path', which is what the interpreter and Flymake below then find.
;; Deactivating when there is no .venv is the half that is easy to forget:
;; pyvenv edits `exec-path' and `process-environment' globally, so a project
;; without one would otherwise keep running against whichever interpreter the
;; last project activated.
(defun rc-programming-activate-venv ()
  "Activate the project's .venv, or deactivate if it has none."
  (let* ((root (locate-dominating-file default-directory ".venv"))
         (venv (and root (expand-file-name ".venv" root))))
    (cond
     ((null venv)
      (when (bound-and-true-p pyvenv-virtual-env)
        (pyvenv-deactivate)))
     ((not (equal (bound-and-true-p pyvenv-virtual-env)
                  (file-name-as-directory venv)))
      (pyvenv-activate venv)))))

;; Depth, so pyvenv-mode above is on before this reaches for pyvenv-activate.
(add-hook 'python-base-mode-hook #'rc-programming-activate-venv 90)

;; Linting without a language server, which the rest of this configuration also
;; does without. ruff is the same toolchain as uv and subsumes flake8, so it is
;; preferred; flake8 stays as the fallback for a machine where brew bundle has
;; not run. Resolved once at startup, after exec-path-from-shell has fixed PATH.
;;
;; Both print `stdin:line:col: CODE message', which is already what
;; `python-flymake-command-output-pattern' expects, so only severities follow.
(setopt python-flymake-command
        (if (executable-find "ruff")
            '("ruff" "check" "--output-format=concise"
              "--stdin-filename" "stdin" "-")
          '("flake8" "--stdin-display-name" "stdin" "-")))

;; First match wins, and the empty pattern matches everything. Without that
;; last clause an unrecognised code defaults to :error, which would make every
;; rule a project turns on shout. Only a file that will not run is an error.
;;
;; ruff reports a parse failure as an uncoded "invalid-syntax:" rather than the
;; E999 flake8 uses, so both spellings are listed. It has to precede the note
;; rule: python.el matches these with `case-fold-search' left at t, so a bare
;; "^[IC]" also matches the i in "invalid-syntax" and files that do not parse
;; end up as notes. The digit anchors close that off as well.
(setopt python-flymake-msg-alist
        '(("^invalid-syntax" . :error)    ; ruff, a file that will not parse
          ("^E9" . :error)                ; flake8, the same
          ("^F82" . :error)               ; undefined name
          ("^[IC][0-9]" . :note)          ; import order, complexity
          ("" . :warning)))               ; style, and anything else opinionated

(add-hook 'python-base-mode-hook #'flymake-mode)

;; apheleia formats Python with black by default, which uv users do not have.
;; `ruff-isort' sorts imports and `ruff' is `ruff format', so the pair covers
;; what isort and black did.
(with-eval-after-load 'apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff)
        (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;;; Markdown

;; markdown-mode claims .md and .markdown through its own autoloads. This only
;; redirects README.md, and lands ahead of that entry by being added later.
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "C-c C-e" #'markdown-do))

(provide 'rc-programming)
;;; rc-programming.el ends here
