;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; Install whatever pre-init.el declared and this machine does not have yet.
;; The repository packages go first: package-install-selected-packages would
;; otherwise go looking for them in the archives and fail.
(package-vc-install-selected-packages)
(package-install-selected-packages :no-confirm)

(require 'rc-defaults)
(require 'rc-ui)
(require 'rc-completion)
(require 'rc-evil)

;; The built-in outline-minor-mode provides structured code folding in modes
;; such as Emacs Lisp and Python, allowing users to collapse and expand sections
;; based on headings or indentation levels. This feature enhances navigation and
;; improves the management of large files with hierarchical structures.
(use-package outline
  :ensure nil
  :commands outline-minor-mode
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
   ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
   ;; folds more visually distinctive and readable.
   (outline-minor-mode
    .
    (lambda()
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table))))))

;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
(use-package outline-indent
  :ensure t
  :commands outline-indent-minor-mode

  :custom
  (outline-indent-ellipsis " ▼")

  :init
  ;; The minor mode can also be automatically activated for a certain modes.
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
(use-package stripspace
  :ensure t
  :commands stripspace-local-mode

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))

;; K&R braces with 4-space indentation and no tabs. Applied live by cc-mode as
;; you type, so newlines land at the right column without waiting for a format.
;; Matches the ~/.clang-format apheleia runs on save.
(defun my/c-indent-style ()
  (c-set-style "k&r")
  (setq c-basic-offset 4
        indent-tabs-mode nil))
(add-hook 'c-mode-common-hook #'my/c-indent-style)

;; Haskell Support
(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode))

;; Ormolu auto-formatting (optional)
(use-package ormolu
  :ensure t
  :hook (haskell-mode . ormolu-format-on-save-mode))

;; Python venv support
(use-package pyvenv
  :ensure t
  :commands (pyvenv-mode pyvenv-activate pyvenv-workon)
  :init
  (add-hook 'python-base-mode-hook #'pyvenv-mode))

;; Zig Support
(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode))

;; Odin Support
;; Installed by package-vc from pre-init.el, so nothing to ensure here.
(use-package odin-mode
  :ensure nil
  :bind (:map odin-mode-map
              ("C-c C-r" . 'odin-run-project)
              ("C-c C-c" . 'odin-build-project)
              ("C-c C-t" . 'odin-test-project)))

;; Org mode is a major mode designed for organizing notes, planning, task
;; management, and authoring documents using plain text with a simple and
;; expressive markup syntax. It supports hierarchical outlines, TODO lists,
;; scheduling, deadlines, time tracking, and exporting to multiple formats
;; including HTML, LaTeX, PDF, and Markdown.
;; The Org that ships with Emacs, not a second copy from ELPA.
(use-package org
  :ensure nil
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :bind (("C-c a"   . org-agenda))
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  :config
  (setq org-agenda-files
        (seq-filter #'file-directory-p '("~/source/org")))
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold)) ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
          ("WAITING" . (:foreground "blue" :weight bold)) ("DONE" . (:foreground "green" :weight bold)))))

;; PDF Stuff
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
(use-package markdown-mode
  :ensure t
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;; The flyspell package is a built-in Emacs minor mode that provides
;; on-the-fly spell checking. It highlights misspelled words as you type,
;; offering interactive corrections. In text modes, it checks the entire buffer,
;; while in programming modes, it typically checks only comments and strings. It
;; integrates with external spell checkers like aspell, hunspell, or
;; ispell to provide suggestions and corrections.
;;
;; NOTE: flyspell-mode can become slow when using Aspell, especially with large
;; buffers or aggressive suggestion settings like --sug-mode=ultra. This
;; slowdown occurs because Flyspell checks words dynamically as you type or
;; navigate text, requiring frequent communication between Emacs and the
;; external Aspell process. Each check involves sending words to Aspell and
;; receiving results, which introduces overhead from process invocation and
;; inter-process communication.
(use-package ispell
  :ensure nil
  :commands (ispell ispell-minor-mode)
  :custom
  ;; Set the ispell program name to aspell
  (ispell-program-name "aspell")

  ;; Define the "en_US" spell-check dictionary locally, telling Emacs to use
  ;; UTF-8 encoding, match words using alphabetic characters, allow apostrophes
  ;; inside words, treat non-alphabetic characters as word boundaries, and pass
  ;; -d en_US to the underlying spell-check program.
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

  ;; Configures Aspell's suggestion mode to "ultra", which provides more
  ;; aggressive and detailed suggestions for misspelled words. The language
  ;; is set to "en_US" for US English, which can be replaced with your desired
  ;; language code (e.g., "en_GB" for British English, "de_DE" for German).
  (ispell-extra-args '(; "--sug-mode=ultra"
                       "--lang=en_US")))

;; The flyspell package is a built-in Emacs minor mode that provides
;; on-the-fly spell checking. It highlights misspelled words as you type,
;; offering interactive corrections.
(use-package flyspell
  :ensure nil
  :commands flyspell-mode
  :config
  ;; Remove strings from Flyspell
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                       flyspell-prog-text-faces))

  ;; Remove doc from Flyspell
  (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
                                       flyspell-prog-text-faces)))

;; Apheleia is an Emacs package designed to run code formatters (e.g., Shfmt,
;; Black and Prettier) asynchronously without disrupting the cursor position.
(use-package apheleia
  :ensure t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

;; The official collection of snippets for yasnippet.
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; YASnippet is a template system designed that enhances text editing by
;; enabling users to define and use snippets. When a user types a short
;; abbreviation, YASnippet automatically expands it into a full template, which
;; can include placeholders, fields, and dynamic content.
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode
             yas-global-mode)

  :hook
  (after-init . yas-global-mode)

  :custom
  (yas-also-auto-indent-first-line t)  ; Indent first line of snippet
  (yas-also-indent-empty-lines t)
  (yas-snippet-revival nil)  ; Setting this to t causes issues with undo
  (yas-wrap-around-region nil) ; Do not wrap region when expanding snippets
  ;; (yas-triggers-in-field nil)  ; Disable nested snippet expansion
  ;; (yas-indent-line 'fixed) ; Do not auto-indent snippet content
  ;; (yas-prompt-functions '(yas-no-prompt))  ; No prompt for snippet choices

  :init
  ;; Suppress verbose messages
  (setq yas-verbosity 0))

;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))

;; Enables automatic indentation of code while typing
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

;; Prevent thesis imbalance
(use-package paredit
  :ensure t
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

;; For paredit+Evil mode users: enhances paredit with Evil mode compatibility
;; --------------------------------------------------------------------------
(use-package enhanced-evil-paredit
  :ensure t
  :commands enhanced-evil-paredit-mode
  :hook
  (paredit-mode . enhanced-evil-paredit-mode))

;; Trying to make the modeline look pretty and also cut down on the amount of
;; modes being displayed on the modeline.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Magit is like lazyvim but within emacs so you can do most git/github
;; actions without having to leave emacs. Magit pulls in `transient' itself,
;; so no separate declaration is needed. Deferred until first invoked.
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

;; The easysession Emacs package is a session manager for Emacs that can persist
;; and restore file editing buffers, indirect buffers/clones, Dired buffers,
;; windows/splits, the built-in tab-bar (including tabs, their buffers, and
;; windows), and Emacs frames. It offers a convenient and effortless way to
;; manage Emacs editing sessions and utilizes built-in Emacs functions to
;; persist and restore frames.
(use-package easysession
  :ensure t
  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes

  :init
  (global-set-key (kbd "C-c l") 'easysession-switch-to)
  (global-set-key (kbd "C-c s") 'easysession-save-as)
  (global-set-key (kbd "C-c r") 'easysession-reset)

  ;; Picks `server-after-make-frame-hook' under a daemon and
  ;; `emacs-startup-hook' otherwise, at the depth 102 this used to hardcode.
  (easysession-setup))

;; Ghostel is a terminal emulator powered by libghostty-vt, the VT engine
;; behind the Ghostty terminal. It supports synchronized output, true color,
;; the Kitty keyboard and graphics protocols, hyperlinks, and more. The native
;; module is a prebuilt binary that auto-downloads on first use, so no
;; toolchain or build step is required.
(use-package ghostel
  :ensure t
  :commands ghostel
  ;; `C-c t' opens/reuses the default *ghostel* terminal. `C-c T' always spawns
  ;; a fresh, independent terminal. A numeric prefix also works, e.g. `M-2 C-c
  ;; t' jumps to (or creates) *ghostel*<2>, giving tmux-window-style addressing.
  :bind (("C-c t" . ghostel)
         ("C-c T" . my/ghostel-new))
  :config
  (defun my/ghostel-new ()
    "Spawn a new, independent ghostel terminal buffer."
    (interactive)
    (ghostel '(4))))

;; Evil integration for ghostel. Syncs the terminal cursor with Emacs point
;; across evil state transitions so normal-mode navigation (hjkl, etc.) works
;; correctly inside a ghostel buffer.
(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))
