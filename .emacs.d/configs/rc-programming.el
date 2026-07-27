;;; rc-programming.el --- Language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes and per-language settings. No LSP: formatting is handled by
;; apheleia in rc-editing, and cc-mode indents C and C++ while typing.
;;
;; Only languages that need something beyond their package appear here. Zig is
;; absent because zig-mode does the whole job on its own.
;;
;; Odin is the exception the other way and lives in rc-odin, since its major
;; mode is written here rather than installed and takes a file of its own.
;;
;; Bindings that live in a mode's own keymap go behind with-eval-after-load,
;; since the map does not exist until the mode's package loads.

;;; Code:

;;; C and C++

;; K&R with four spaces and no tabs, applied live by cc-mode so newlines land
;; at the right column without waiting for a format. Matches ~/.clang-format.
(defun rc-programming-c-indent-style ()
  "Set the K&R style and a four column offset for the current C buffer."
  (c-set-style "k&r")
  (setq c-basic-offset 4
        indent-tabs-mode nil))

(add-hook 'c-mode-common-hook #'rc-programming-c-indent-style)

;;; Haskell

(add-hook 'haskell-mode-hook #'ormolu-format-on-save-mode)

;;; Python

(add-hook 'python-base-mode-hook #'pyvenv-mode)

;;; Markdown

;; markdown-mode claims .md and .markdown through its own autoloads. This only
;; redirects README.md, and lands ahead of that entry by being added later.
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "C-c C-e" #'markdown-do))

(provide 'rc-programming)
;;; rc-programming.el ends here
