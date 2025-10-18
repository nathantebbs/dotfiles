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

;; Change file backup location
(setq backup-firactory-alist '(("."."~/.emacs.d/backups")))

;; ================
;; Keybindings
;; ================

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-c") '(lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-x e") 'eval-buffer)

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

(straight-use-package 'use-package)

;; =================
;; Install Packages:
;; =================

;; Theme
(use-package gruber-darker-theme
  :straight t
  :config
  (load-theme 'gruber-darker t))

;; Evil mode
(use-package evil
  :straight t)
(require 'evil)
(evil-mode 1)

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
  :bind ("C-c s" . fzf-grep)
  :straight t)


;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "multimarkdown")  ;; or "pandoc"
  :config
  (setq markdown-fontify-code-blocks-natively t))
