;;; rc-org.el --- Org and document viewing -*- lexical-binding: t; -*-

;;; Commentary:

;; Org and PDF viewing. This is the Org that ships with Emacs; pre-init.el
;; deliberately leaves it out of the package manifest so a second copy from
;; ELPA cannot race the built-in for load order.

;;; Code:

;;; Org

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(keymap-global-set "C-c a" #'org-agenda)

(setopt org-hide-leading-stars t)
(setopt org-startup-indented t)
(setopt org-adapt-indentation nil)
(setopt org-edit-src-content-indentation 0)

(with-eval-after-load 'org
  ;; Filtered, so a missing directory does not break the agenda.
  (setq org-agenda-files
        (seq-filter #'file-directory-p '("~/source/org")))
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "red" :weight bold))
          ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
          ("WAITING" . (:foreground "blue" :weight bold))
          ("DONE" . (:foreground "green" :weight bold)))))

;;; PDF

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; Builds epdfinfo on first use if it is missing.
(with-eval-after-load 'pdf-tools
  (pdf-tools-install :no-query))

(provide 'rc-org)
;;; rc-org.el ends here
