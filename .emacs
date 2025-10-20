;; File: .emacs
;; Author: Nathan Tebbs
;; Purpose: Simple Emacs configuration

;; Remove window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode t) ;; Line numbers

(setq inhibit-startup-screen t) ;; Disable startup screen

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Mini-buffer completion mode
(fido-vertical-mode)

;; Misc
(electric-pair-mode t) ;; Autopairs
(which-key-mode) ;; which-key
(setq org-agenda-files '("~/org/todo.org"))

;; Change file backup location
(setq make-backup-file nil) ;; No more
(setq auto-save-default nil) ;; No autosave files
(setq backup-directory-alist '((".*" . "~/.Trash")))

;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; =================
;; Install Packages:
;; =================

(straight-use-package 'use-package)

;; Theme
(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t))

;; Evil mode
(use-package evil
  :straight t

  :init
  (setq evil-want-C-u-scroll t) ;; Fixes C-u scrolling

  :config
  (evil-mode 1)

  ;; Keymaps (*Evil*)
  
  ;; Leader
  (define-prefix-command 'nate/leader-map)
  (define-key evil-normal-state-map (kbd "SPC") 'nate/leader-map)
  (define-key evil-visual-state-map (kbd "SPC") 'nate/leader-map)

  ;; Fzf
  (define-key nate/leader-map (kbd "s n") (lambda () (interactive) (fzf-find-file-in-dir "~/dotfiles/")))
  (define-key nate/leader-map (kbd "s f") (lambda () (interactive) (fzf-find-file)))
  (define-key nate/leader-map (kbd "s p") (lambda () (interactive) (fzf-find-file-in-dir "~/dev/probe/")))
  (define-key nate/leader-map (kbd "f") #'find-file)

  ;; Magit
  (define-key nate/leader-map (kbd "g s") #'magit)

  ;; State
  (define-key evil-insert-state-map (kbd "C-g") 'evil-change-to-previous-state)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-change-to-previous-state)

  ;; Buffers
  (define-key nate/leader-map (kbd "b") #'switch-to-buffer)
  (define-key nate/leader-map (kbd "B") #'ibuffer-other-window)
  (define-key nate/leader-map (kbd "w") #'save-buffer)
  (define-key nate/leader-map (kbd "q") #'save-buffers-kill-terminal)

  ;; Config
  (define-key nate/leader-map (kbd "r r") (lambda () (interactive) (load-file "~/.emacs")))

  )

;; Magit
(use-package magit
  :straight t)

;; Devdocs
;; NOTE: use M-x devdocs-install
(use-package devdocs
  :straight t)

;; Company
(use-package company
  :straight t
  :hook (prog-mode . global-company-mode))

;; Fzf
(use-package fzf
  :straight t)


;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; ox-hugo
(use-package ox-hugo
  :straight t
  :after ox)

;; Terminal
(use-package vterm
  :straight t)

;; Org pomodoro
(use-package org-pomodoro
  :straight t)

;; Popup Mgmt
(use-package popper
  :straight t

  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*compilation\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
