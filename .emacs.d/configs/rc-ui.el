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

(require 'modus-themes)

;; The pair `modus-themes-toggle' flips between.
(setopt modus-themes-to-toggle '(modus-vivendi modus-operandi))

(defun rc-ui-apply-appearance (&rest _)
  "Apply the default font and theme.  Idempotent, so safe on every frame."
  (set-face-attribute 'default nil
                      :height 170 :weight 'normal :family "Zenbones Brainy")
  (unless (memq 'modus-vivendi custom-enabled-themes)
    (modus-themes-load-theme 'modus-vivendi)))

;; A daemon starts with no frame, so a font set now would have nothing to
;; attach to and client frames would come up with the wrong one.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'rc-ui-apply-appearance)
  (rc-ui-apply-appearance))

;;; Mode line

;; The stock mode line with the dead weight removed: the coding-system block,
;; the "@" every client frame carries, and the frame identification.
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                "  "
                mode-line-position
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-format-right-align
                mode-line-misc-info
                mode-line-end-spaces))

;; Minor mode lighters collapse to one "…", which menus the full list on click.
(setopt mode-line-collapse-minor-modes t)

;; Line and column, without the percentage through the buffer.
(column-number-mode 1)
(setopt mode-line-percent-position nil)

;;; Buffer presentation

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; show-paren-mode has been on by default since Emacs 28, so it is not here.
(electric-pair-mode 1)

;;; Scrolling

(setopt mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setopt mouse-wheel-progressive-speed nil)

;;; Window configuration

;; `C-c left' and `C-c right' undo and redo window layouts.
(winner-mode 1)

;;; Compilation

;; Not `C-c c': rc-org.el takes that for `org-capture', which every piece of
;; Org documentation assumes.
(keymap-global-set "C-c b" #'compile)

(provide 'rc-ui)
;;; rc-ui.el ends here
