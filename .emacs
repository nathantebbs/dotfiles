;; -*- lexical-binding: t; -*-
;; File: .emacs
;; Author: Nathan Tebbs
;; Purpose: Simple (ish) Emacs Configuration

;; ===========================
;; Startup Optimizations
;; ===========================
(setq gc-cons-threshold (* 50 1000 1000)
      read-process-output-max (* 4 1024 1024)
      inhibit-compacting-font-caches t
      native-comp-async-report-warnings-errors nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))
            (message "✅ Emacs ready in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; ===========================
;; UI Basics
;; ===========================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

;; ===========================
;; Font
;; ===========================
(defconst npt/font-family "Zenbones Brainy")
(defconst npt/font-height 160) ;; 160 ~= 16pt-ish

(defun npt/apply-font (&optional frame)
  "Apply my preferred font to FRAME (or current frame)."
  (let ((frame (or frame (selected-frame))))
    (when (display-graphic-p frame)
      (when (member npt/font-family (font-family-list))
        (with-selected-frame frame
          (set-face-attribute 'default nil
                              :family npt/font-family
                              :height npt/font-height)
          (set-face-attribute 'fixed-pitch nil
                              :family npt/font-family
                              :height npt/font-height))))))

;; Apply once init completes (theme can override faces)
(add-hook 'after-init-hook #'npt/apply-font)

;; Apply to new GUI frames (daemon/emacsclient)
(add-hook 'after-make-frame-functions #'npt/apply-font)

;; ===========================
;; macOS Modifier Setup
;; (safe on non-mac; variables may be unbound, so guard)
;; ===========================
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-control-modifier 'control
        ns-function-modifier 'hyper))

(setq select-enable-clipboard t
      select-enable-primary t)

;; ===========================
;; Lines, Tabs, and Indentation
;; ===========================
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default truncate-lines t) ;; No visual wrapping

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq c-basic-offset 4)

;; ===========================
;; Completion & Editing
;; ===========================
(fido-vertical-mode 1)
(electric-pair-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(setq recentf-max-saved-items 200)

;; Change custom file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ===========================
;; Backups & Autosaves
;; ===========================
(setq create-lockfiles nil)
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (autosave-dir (expand-file-name "autosaves/" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory autosave-dir t)

  (setq backup-directory-alist `(("." . ,backup-dir))
        make-backup-files t
        version-control t
        kept-new-versions 10
        kept-old-versions 2
        delete-old-versions t)

  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t))
        auto-save-default t
        auto-save-timeout 20
        auto-save-interval 200))

;; ===========================
;; General Keymaps
;; ===========================
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "-") #'dired-up-directory))

(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "r") #'recompile)
  (define-key compilation-mode-map (kbd "g") #'compilation-next-error))

;; Lectura (Or other commonly used VMs)
(defun connect-lectura ()
  (interactive)
  (dired "/ssh:ntebbs@lec.cs.arizona.edu:/home/ntebbs/"))

;; ===========================
;; Straight.el bootstrap
;; ===========================
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

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; =================
;; Theme
;; =================
(use-package ef-themes
  :config
  (load-theme 'ef-dark t)
  ;; Theme may touch faces; re-apply font deterministically
  (npt/apply-font))

;; =================
;; which-key
;; =================
(use-package which-key
  :config
  (which-key-mode 1))

;; =================
;; Evil mode
;; =================
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)

  ;; Leader
  (define-prefix-command 'nate/leader-map)
  (define-key evil-normal-state-map (kbd "SPC") 'nate/leader-map)
  (define-key evil-visual-state-map (kbd "SPC") 'nate/leader-map)

  ;; Finding Files
  (define-key nate/leader-map (kbd "s n")
    (lambda () (interactive) (fzf-find-file-in-dir "~/dotfiles/")))
  (define-key nate/leader-map (kbd "s o")
    (lambda () (interactive) (fzf-find-file-in-dir "~/org/")))
  (define-key nate/leader-map (kbd "s f")
    (lambda () (interactive) (fzf-find-file)))
  (define-key nate/leader-map (kbd "s p")
    (lambda () (interactive) (fzf-find-file-in-dir "~/dev/probe/")))
  (define-key nate/leader-map (kbd "s F")
    (lambda () (interactive) (fzf-grep)))

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
  (define-key nate/leader-map (kbd "o T")
    (lambda () (interactive) (find-file "~/org/todo.org")))
  (define-key nate/leader-map (kbd "o N")
    (lambda () (interactive) (find-file "~/org/notes.org")))
  (define-key nate/leader-map (kbd "o P")
    (lambda () (interactive) (find-file "~/org/projects.org")))
  (define-key nate/leader-map (kbd "o A")
    (lambda () (interactive) (find-file "~/org/assignments.org")))
  (define-key nate/leader-map (kbd "o L")
    (lambda () (interactive) (find-file "~/org/log.org")))

  ;; State
  (define-key evil-insert-state-map (kbd "C-g") #'evil-change-to-previous-state)
  (define-key evil-visual-state-map (kbd "C-g") #'evil-change-to-previous-state)
  (define-key evil-replace-state-map (kbd "C-g") #'evil-change-to-previous-state)

  ;; Buffers
  (define-key nate/leader-map (kbd "b b") #'switch-to-buffer)
  (define-key nate/leader-map (kbd "b i") #'ibuffer-other-window)
  (define-key nate/leader-map (kbd "b k") #'kill-buffer)

  ;; Config reload
  (define-key nate/leader-map (kbd "r r")
    (lambda () (interactive) (load-file "~/.emacs"))))

;; =================
;; Magit
;; =================
(use-package magit
  :defer t
  :commands (magit-status magit-log)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; =================
;; Devdocs
;; =================
(use-package devdocs
  :defer t)

;; =================
;; Company
;; =================
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;; =================
;; Fzf
;; =================
(use-package fzf)

;; =================
;; Markdown
;; =================
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; =================
;; ox-hugo
;; =================
(use-package ox-hugo
  :after ox)

;; =================
;; Org mode
;; =================
(use-package org
  :straight (:type built-in)
  :config
  (setq org-startup-indented t
        org-agenda-window-setup 'reorganize-frame
        org-pretty-entities t
        org-return-follows-link t
        org-cycle-separator-lines 0)

  (setq org-directory "~/org/")
  (setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

  (setq org-file-apps
        '(("\\.pdf\\'" . "xdg-open %s")))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")
          (sequence
           "ASSIGNED(a)" "WORKING(w)" "|" "SUBMITTED(s)" "GRADED(g)" "CANCELLED(c)")
          (sequence
           "BACKLOG(p)" "DEVELOPMENT(d)" "TESTING(t)" "|" "FINISHED(f)")))

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

  (setq org-log-done 'time
        org-log-into-drawer t)

  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))))

;; =================
;; Org Superstar
;; =================
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("●" "○" "◆" "◇")))

;; =================
;; Popup Mgmt
;; =================
(use-package popper
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
  :config
  (popper-mode +1)
  (popper-echo-mode +1))

;; =================
;; Dired Single
;; =================
(use-package dired-single
  :bind (:map dired-mode-map
              ("RET" . dired-single-buffer)
              ("^"   . dired-single-up-directory)))

;; =================
;; LSP
;; =================
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (java-mode . lsp))
  :commands lsp)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))

;; =================
;; Web Dev Stuff
;; =================
(use-package web-mode
  :mode ("\\.html?\\'" . web-mode)
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t))

;; =================
;; Typst
;; =================
(use-package typst-ts-mode
  :mode ("\\.typ\\'" . typst-ts-mode)
  :config
  (setq typst-ts-watch-options "--root ."
        typst-ts-mode-indent-offset 2))

;; =================
;; TypeScript
;; =================
(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-mode)
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))
