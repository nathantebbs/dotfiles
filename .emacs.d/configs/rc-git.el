;;; rc-git.el --- Git -*- lexical-binding: t; -*-

;;; Commentary:

;; Magit. It pulls in transient itself, so there is nothing else to declare.

;;; Code:

(keymap-global-set "C-x g" #'magit-status)

(provide 'rc-git)
;;; rc-git.el ends here
