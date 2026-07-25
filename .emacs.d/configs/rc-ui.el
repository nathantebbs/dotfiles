;;; rc-ui.el --- Appearance and window management -*- lexical-binding: t; -*-

;;; Commentary:

;; Everything that decides how Emacs looks: font, theme, line numbers, the
;; mode line, and the handful of global minor modes that affect presentation
;; rather than behaviour.
;;
;; The font and theme calls here still run at load time, which is correct for
;; a normal startup and wrong under a daemon, where no frame exists yet. That
;; is dealt with separately.

;;; Code:

(set-face-attribute 'default nil
                    :height 170 :weight 'normal :family "Zenbones Brainy")

;;; Theme

(require 'modus-themes)

;; The pair `modus-themes-toggle' flips between.
(setopt modus-themes-to-toggle '(modus-vivendi modus-operandi))

(modus-themes-load-theme 'modus-vivendi)

;;; Mode line

(column-number-mode 1)
(doom-modeline-mode 1)

;;; Buffer presentation

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(electric-pair-mode 1)
(show-paren-mode 1)

;;; Scrolling

(setopt mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setopt mouse-wheel-progressive-speed nil)

;;; Window configuration

;; `C-c left' and `C-c right' undo and redo window layouts.
(winner-mode 1)

(keymap-global-set "C-c c" #'compile)

(provide 'rc-ui)
;;; rc-ui.el ends here
