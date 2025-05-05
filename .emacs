;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c i") #'consult-outline); outline-regexp: ";;;"; -*-

;;; startup
(setq gc-cons-threshold (* 100 1000 1000))
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (message "Startup in %s sec with %d garbage collections"
		       (emacs-init-time "%.2f")
		       gcs-done)))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode)
(hl-line-mode)

(add-to-list 'default-frame-alist `(font . "JetBrains Mono"))
(load-theme 'gruber-darker t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package evil
  :ensure t
  :config
  (evil-mode)
  (setq evil-search-module 'evil-search)
  (evil-set-undo-system 'undo-tree))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package auctex
  :ensure t
  :defer t)


(use-package consult
  :ensure t
  :bind (("C-c i" . consult-outline))
  :init
  (setq outline-regexp ";;;"))  ;; Customize to treat lines starting with ;;; as headings



(setq backup-directory-alist            '((".*" . "~/.Trash")))
(setq custom-file "~/.custom.el")
