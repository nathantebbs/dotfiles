;;; pre-init.el --- Package manifest -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; init.el loads this file immediately before it calls `package-initialize',
;; which is the last point at which `package-selected-packages' still decides
;; what gets activated. The archives and their priorities are already set by
;; early-init.el, upstream, in the order GNU > NonGNU > MELPA > MELPA stable.
;;
;; Org is deliberately absent: Emacs ships a current one, and pulling a second
;; copy from ELPA only invites a version mismatch against the built-in that
;; loads first.

;;; Code:

(setq package-selected-packages
      '(;; Completion and navigation
        cape
        consult
        corfu
        embark
        embark-consult
        marginalia
        orderless
        vertico

        ;; Modal editing
        evil
        evil-collection
        evil-mc
        evil-surround
        move-text
        undo-fu
        undo-fu-session

        ;; Editing
        apheleia
        outline-indent
        stripspace
        yasnippet
        yasnippet-snippets

        ;; Emacs Lisp
        aggressive-indent
        enhanced-evil-paredit
        helpful
        highlight-defined
        paredit

        ;; Languages
        haskell-mode
        markdown-mode
        ormolu
        pyvenv
        zig-mode

        ;; Documents
        pdf-tools

        ;; Interface
        doom-modeline
        modus-themes

        ;; Everything else
        easysession
        evil-ghostel
        exec-path-from-shell
        ghostel
        magit))

;; odin-mode has no ELPA recipe, so it comes straight from its repository.
(setq package-vc-selected-packages
      '((odin-mode :url "https://github.com/mattt-b/odin-mode")))

;;; pre-init.el ends here
