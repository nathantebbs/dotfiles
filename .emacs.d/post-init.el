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
(require 'rc-editing)
(require 'rc-elisp)
(require 'rc-programming)

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
