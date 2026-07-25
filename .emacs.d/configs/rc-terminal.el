;;; rc-terminal.el --- Terminal emulator -*- lexical-binding: t; -*-

;;; Commentary:

;; Ghostel, the libghostty-vt terminal, plus its Evil integration. The native
;; module is a prebuilt binary fetched on first use, so there is no build step.

;;; Code:

(defun my/ghostel-new ()
  "Spawn a new, independent ghostel terminal buffer."
  (interactive)
  (ghostel '(4)))

;; `C-c t' reuses the default *ghostel*, `C-c T' always spawns a fresh one. A
;; numeric prefix addresses them like tmux windows, e.g. `M-2 C-c t'.
(keymap-global-set "C-c t" #'ghostel)
(keymap-global-set "C-c T" #'my/ghostel-new)

;; Keeps the terminal cursor in step with point across Evil state changes, so
;; hjkl navigation works inside a ghostel buffer.
(add-hook 'ghostel-mode-hook #'evil-ghostel-mode)

(provide 'rc-terminal)
;;; rc-terminal.el ends here
