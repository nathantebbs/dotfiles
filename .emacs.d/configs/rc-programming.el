;;; rc-programming.el --- Language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes and per-language settings. No LSP: formatting is handled by
;; apheleia in rc-editing, and cc-mode indents C and C++ while typing.
;;
;; Only languages that need something beyond their package appear here.
;;
;; Odin is the exception the other way and lives in rc-odin, since its major
;; mode is written here rather than installed and takes a file of its own.
;;
;; Bindings that live in a mode's own keymap go behind with-eval-after-load,
;; since the map does not exist until the mode's package loads.

;;; Code:

(require 'treesit)

;;; C and C++

;; K&R with four spaces and no tabs, applied live by cc-mode so newlines land
;; at the right column without waiting for a format. Matches ~/.clang-format.
(defun rc-programming-c-indent-style ()
  "Set the K&R style and a four column offset for the current C buffer."
  (c-set-style "k&r")
  (setq c-basic-offset 4
        indent-tabs-mode nil))

(add-hook 'c-mode-common-hook #'rc-programming-c-indent-style)

;;; Python

;; Homebrew installs python3 and no bare `python'. Emacs 30 already defaults
;; both of these to python3; stating them keeps an older one from going looking
;; for a Python 2 that is not there either.
(setopt python-shell-interpreter "python3")
(setopt python-interpreter "python3")

;; Tree-sitter on the same terms as Odin: the grammar is built per machine and
;; the directory is gitignored, so the remap only takes hold once it exists and
;; a fresh clone falls back to `python-mode' rather than erroring.
(add-to-list 'treesit-language-source-alist
             '(python "https://github.com/tree-sitter/tree-sitter-python"))

(defun rc-programming-install-python-grammar ()
  "Compile and install the Python tree-sitter grammar."
  (interactive)
  (treesit-install-language-grammar 'python))

(when (treesit-ready-p 'python t)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

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
