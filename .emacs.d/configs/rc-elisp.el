;;; rc-elisp.el --- Emacs Lisp editing and help -*- lexical-binding: t; -*-

;;; Commentary:

;; Tooling for editing Emacs Lisp, plus helpful, which improves the help
;; buffers for every mode but is only ever reached through elisp symbols.

;;; Code:

;;; Emacs Lisp buffers

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;; Paredit's RET reindents rather than opening a line, which fights Evil.
(with-eval-after-load 'paredit
  (keymap-set paredit-mode-map "RET" nil))

(add-hook 'paredit-mode-hook #'enhanced-evil-paredit-mode)

;;; Help

(setopt helpful-max-buffers 7)

(keymap-global-set "<remap> <describe-command>" #'helpful-command)
(keymap-global-set "<remap> <describe-function>" #'helpful-callable)
(keymap-global-set "<remap> <describe-key>" #'helpful-key)
(keymap-global-set "<remap> <describe-symbol>" #'helpful-symbol)
(keymap-global-set "<remap> <describe-variable>" #'helpful-variable)

(provide 'rc-elisp)
;;; rc-elisp.el ends here
