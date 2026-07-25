;;; rc-session.el --- Session persistence -*- lexical-binding: t; -*-

;;; Commentary:

;; easysession restores file buffers, Dired buffers, window layouts, tab-bar
;; tabs and frames between runs.

;;; Code:

(setopt easysession-mode-line-misc-info t)
(setopt easysession-save-interval (* 10 60))

(keymap-global-set "C-c l" #'easysession-switch-to)
(keymap-global-set "C-c s" #'easysession-save-as)
(keymap-global-set "C-c r" #'easysession-reset)

;; Picks `server-after-make-frame-hook' under a daemon, where no frame exists
;; at startup, and `emacs-startup-hook' otherwise. Depth 102 either way.
(easysession-setup)

(provide 'rc-session)
;;; rc-session.el ends here
