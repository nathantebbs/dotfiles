;; -*- lexical-binding: t; -*-
;; File: .emacs
;; Author: Nathan Tebbs
;; Purpose: Simple (ish) Emacs Configuration

;; Remove window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Font
;; --- Font setup: Zenbones Mono ---
(defun npt/set-zenbones-font ()
  "Set Zenbones Brainy as the default and fixed-pitch font."
  (when (member "Zenbones Brainy" (font-family-list))
    (set-face-attribute 'default nil :family "Zenbones Brainy" :height 160)
    (set-face-attribute 'fixed-pitch nil :family "Zenbones Brainy" :height 160)
    (add-to-list 'default-frame-alist
                 '(font . "Zenbones Brainy:pixelsize=16:foundry=UKWN:weight=regular:slant=normal:width=normal:spacing=90:scalable=true"))))

;; Apply immediately if in a GUI frame
(when (display-graphic-p)
  (npt/set-zenbones-font))

;; Also apply *after* frame creation (e.g., when using emacsclient or daemon)
(add-hook 'after-make-frame-functions
          (lambda (_frame)
            (with-selected-frame _frame
              (npt/set-zenbones-font))))

;; ===========================
;; macOS Modifier Setup
;; ===========================
(defun npt/mac-mods ()
  "Configure modifier keys for macOS."
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-control-modifier 'control
        ns-function-modifier 'hyper))

(with-eval-after-load 'mac-win
  (npt/mac-mods))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (npt/mac-mods))))
;; Lines
(global-display-line-numbers-mode t)
(setq-default truncate-lines t) ;; No visual wrapping

(setq inhibit-startup-screen t) ;; Disable startup screen

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; Mini-buffer completion mode
(fido-vertical-mode)

;; Misc
(electric-pair-mode t) ;; Autopairs
(which-key-mode) ;; which-key

;; Change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; Create backup and autosave directories if they don't exist
;; Disable lockfiles
(setq create-lockfiles nil)
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (autosave-dir (expand-file-name "autosaves/" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory autosave-dir t)

  ;; Backups (files ending with ~)
  (setq backup-directory-alist `(("." . ,backup-dir))
        make-backup-files t
        version-control t          ; use versioned backups
        kept-new-versions 10
        kept-old-versions 2
        delete-old-versions t)

  ;; Autosave files (#foo#)
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t))
        auto-save-default t
        auto-save-timeout 20        ; save every 20 sec idle
        auto-save-interval 200))    ; or every 200 keystrokes

;; General Keymaps
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "-") #'dired-up-directory))

;; Lectura (Or other commonly used VMs)
(defun connect-lectura ()
  (interactive)
  (dired "/ssh:ntebbs@lec.cs.arizona.edu:/home/ntebbs/"))

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
(use-package ef-themes
  :straight t
  :config
  (load-theme 'ef-dark t))

;; Evil mode
(use-package evil
  :straight t

  :init
  (setq evil-want-C-u-scroll t) ;; Fixes C-u scrolling

  :config
  (evil-mode 1)

  ;; =================
  ;; *Evil* Keymaps 
  ;; =================
  
  ;; Leader
  (define-prefix-command 'nate/leader-map)
  (define-key evil-normal-state-map (kbd "SPC") 'nate/leader-map)
  (define-key evil-visual-state-map (kbd "SPC") 'nate/leader-map)

  ;; Finding Files
  (define-key nate/leader-map (kbd "s n") (lambda () (interactive) (fzf-find-file-in-dir "~/dotfiles/")))
  (define-key nate/leader-map (kbd "s o") (lambda () (interactive) (fzf-find-file-in-dir "~/org/")))
  (define-key nate/leader-map (kbd "s f") (lambda () (interactive) (fzf-find-file)))
  (define-key nate/leader-map (kbd "s p") (lambda () (interactive) (fzf-find-file-in-dir "~/dev/probe/")))
  (define-key nate/leader-map (kbd "s F") (lambda () (interactive) (fzf-grep)))
  (define-key nate/leader-map (kbd "f") #'find-file)
  (define-key nate/leader-map (kbd "e") #'dired-jump)

  ;; Compilation
  (define-key nate/leader-map (kbd "c c") #'compile)

  ;; Magit
  (define-key nate/leader-map (kbd "g s") #'magit-status)

  ;; Org Mode
  (define-key nate/leader-map (kbd "o a") #'org-agenda)
  (define-key nate/leader-map (kbd "o c") #'org-capture)
  (define-key nate/leader-map (kbd "o r") #'org-refile)
  (define-key nate/leader-map (kbd "o T") (lambda () (interactive) (find-file "~/org/todo.org")))
  (define-key nate/leader-map (kbd "o N") (lambda () (interactive) (find-file "~/org/notes.org")))
  (define-key nate/leader-map (kbd "o P") (lambda () (interactive) (find-file "~/org/projects.org")))
  (define-key nate/leader-map (kbd "o A") (lambda () (interactive) (find-file "~/org/assignments.org")))
  (define-key nate/leader-map (kbd "o L") (lambda () (interactive) (find-file "~/org/log.org")))
  
  ;; State
  (define-key evil-insert-state-map (kbd "C-g") 'evil-change-to-previous-state)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-change-to-previous-state)
  (define-key evil-replace-state-map (kbd "C-g") 'evil-change-to-previous-state)

  ;; Buffers
  (define-key nate/leader-map (kbd "b b") #'switch-to-buffer)
  (define-key nate/leader-map (kbd "b i") #'ibuffer-other-window)
  (define-key nate/leader-map (kbd "b k") #'kill-buffer)

  ;; Config
  (define-key nate/leader-map (kbd "r r") (lambda () (interactive) (load-file "~/.emacs"))))

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
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))
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

;; Org mode
(use-package org
  :straight (:type built-in)

  :config
  ;; Indentation
  (setq org-startup-indented t
        org-agenda-window-setup 'reorganize-frame
        org-pretty-entities t
        org-return-follows-link t
        org-cycle-separator-lines 0)

  ;; Base directories
  (setq org-directory "~/org/")
  (setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

  ;; Use OS pdf viewer
  (setq org-file-apps
      '(("\\.pdf\\'" . "xdg-open %s")))

  ;; TODO workflow
  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence
           "ASSIGNED(a)" "WORKING(w)" "|" "SUBMITTED(s)" "GRADED(g)" "CANCELLED(c)")
          (sequence
           "BACKLOG(p)" "DEVELOPMENT(d)" "TESTING(t)" "|" "FINISHED(f)")))

  ;; Faces for visual distinction
  ;; NOTE: See X11 Colors
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "deep pink" :weight bold))
          ("IN-PROGRESS" . (:foreground "deep sky blue" :weight bold))
          ("BLOCKED" . (:foreground "orange red" :weight bold))
          ("REVIEW" . (:foreground "plum" :weight bold))
          ("DONE" . (:foreground "lime green" :weight bold))
          ("CANCELLED" . (:foreground "gray" :weight bold))
          ("ASSIGNED" . (:foreground "hot pink" :weight bold))
          ("WORKING" . (:foreground "red" :weight bold))
          ("SUBMITTED" . (:foreground "dark violet" :weight bold))
          ("GRADED" . (:foreground "x11 green" :weight bold))
          ("BACKLOG" . (:foreground "maroon" :weight bold))
          ("DEVELOPMENT" . (:foreground "slate blue" :weight bold))
          ("TESTING" . (:foreground "orchid" :weight bold))
          ("FINISHED" . (:foreground "green" :weight bold))))

  ;; Tag presets
  (setq org-tag-alist
        '((:startgroup)
          ("@school" . ?s)
          ("@personal" . ?p)
          (:endgroup)
          ("CS352" . ?1)
          ("CS460" . ?2)
          ("CS425" . ?3)
          ("GEOS251" . ?4)
          ("PROJECT" . ?P)
          ("LIFE" . ?l)
          ("LOG" . ?L)
          ("QUIZ" . ?q)
          ("NOTE" . ?n)
          ("ASSIGNMENT" . ?a)
          ("RECIPE" . ?r)))

  ;; Time
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; Refile
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))))


;; Org Superstar
(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("●" "○" "◆" "◇")))

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

;; Dired Single (No More Messy Buffer)
(use-package dired-single
  :straight t
  :bind (:map dired-mode-map
              ("RET" . dired-single-buffer)
              ("^"   . dired-single-up-directory)))

;; LSP
(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (java-mode . lsp))
  :commands lsp)

(use-package lsp-java
  :straight t
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))

;; TODO: does this help at all?? (e.g. LSP failure(s) resulting in restart of service)
(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))
