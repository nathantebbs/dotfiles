;;; rc-eglot.el --- Shared language server bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; Bindings shared by every language that uses Eglot. Server commands and
;; startup hooks stay with their language modules.

;;; Code:

(defvar eglot-mode-map)

(declare-function eglot-code-actions "eglot")
(declare-function eglot-find-declaration "eglot")
(declare-function eglot-find-implementation "eglot")
(declare-function eglot-find-typeDefinition "eglot")
(declare-function eglot-format "eglot")
(declare-function eglot-inlay-hints-mode "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-rename "eglot")

(with-eval-after-load 'eglot
  ;; The stock bindings already cover definitions, back navigation, workspace
  ;; symbols and Flymake diagnostics.
  (keymap-set eglot-mode-map "C-c l a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c l d" #'eldoc-doc-buffer)
  (keymap-set eglot-mode-map "C-c l f" #'eglot-format)
  (keymap-set eglot-mode-map "C-c l i" #'eglot-inlay-hints-mode)
  (keymap-set eglot-mode-map "C-c l r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c l D" #'eglot-find-declaration)
  (keymap-set eglot-mode-map "C-c l R" #'eglot-reconnect)
  (keymap-set eglot-mode-map "C-c l m" #'eglot-find-implementation)
  (keymap-set eglot-mode-map "C-c l t" #'eglot-find-typeDefinition))

(provide 'rc-eglot)
;;; rc-eglot.el ends here
