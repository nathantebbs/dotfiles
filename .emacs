;; File: .emacs
;; Author: Nathan Tebbs
;; Purpose: Simple Emacs configuration

;; Remove window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'modus-vivendi) ;; Theme

(global-display-line-numbers-mode t) ;; Line numbers

;; Mini-buffer completion mode
(fido-vertical-mode)

;; Misc
(electric-pair-mode t) ;; Autopairs

;; Change file backup location
(setq backup-firactory-alist '(("."."~/.emacs.d/backups")))

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

;; Evil mode
(use-package evil
  :straight t)
(require 'evil)
(evil-mode 1)

;; Devdocs
;; NOTE: use M-x devdocs-install
(use-package devdocs
  :straight t)

;; Company
(use-package company
  :straight t
  :hook (prog-mode . global-company-mode))
