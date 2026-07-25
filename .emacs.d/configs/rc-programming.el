;;; rc-programming.el --- Language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes and per-language settings. No LSP: formatting is handled by
;; apheleia in rc-editing, and cc-mode indents C and C++ as you type.
;;
;; Bindings that live in a mode's own keymap go behind with-eval-after-load,
;; since the map does not exist until the mode's package loads.

;;; Code:

;;; C and C++

;; K&R with four spaces and no tabs, applied live by cc-mode so newlines land
;; at the right column without waiting for a format. Matches ~/.clang-format.
(defun my/c-indent-style ()
  (c-set-style "k&r")
  (setq c-basic-offset 4
        indent-tabs-mode nil))

(add-hook 'c-mode-common-hook #'my/c-indent-style)

;;; Haskell

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(add-hook 'haskell-mode-hook #'ormolu-format-on-save-mode)

;;; Python

(add-hook 'python-base-mode-hook #'pyvenv-mode)

;;; Zig

(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

;;; Odin

;; Installed by package-vc from pre-init.el.
(with-eval-after-load 'odin-mode
  (keymap-set odin-mode-map "C-c C-r" #'odin-run-project)
  (keymap-set odin-mode-map "C-c C-c" #'odin-build-project)
  (keymap-set odin-mode-map "C-c C-t" #'odin-test-project))

;;; Markdown

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "C-c C-e" #'markdown-do))

(provide 'rc-programming)
;;; rc-programming.el ends here
