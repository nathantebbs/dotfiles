;;; rc-evil.el --- Modal editing -*- lexical-binding: t; -*-

;;; Commentary:

;; Evil and the packages that extend it, plus undo-fu, which is the undo
;; system Evil is pointed at.
;;
;; Order is load bearing here. evil-want-integration, evil-want-keybinding and
;; evil-undo-system are read by Evil as it loads, so they have to be set before
;; the require rather than after it, and evil-collection has the same
;; requirement for its own minibuffer setting.

;;; Code:

;;; Undo

(add-hook 'after-init-hook #'undo-fu-session-global-mode)

(keymap-global-unset "C-z")
(keymap-global-set "C-z" #'undo-fu-only-undo)
(keymap-global-set "C-S-z" #'undo-fu-only-redo)

;;; Evil

;; evil-core.el declares this with no value and `evil-initializing-p' reads it,
;; but nothing in evil 1.15.0 ever sets it. Without this every command in
;; normal state signals a void-variable from `post-command-hook'.
(defvar evil-mode-buffers nil)

;; All three are consumed while Evil loads, so they precede the require.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-undo-system 'undo-fu)

(require 'evil)

;; Make :s in visual mode operate on the actual selection rather than every
;; line the selection touches.
(setopt evil-ex-visual-char-range t)
;; Vim-style regexps in search and substitute, so \v, \zs and \ze work.
(setopt evil-ex-search-vim-style-regexp t)
(setopt evil-split-window-below t)
(setopt evil-vsplit-window-right t)
;; Echoing the state would clobber eldoc.
(setopt evil-echo-state nil)
(setopt evil-move-cursor-back nil)
(setopt evil-v$-excludes-newline t)
(setopt evil-want-C-h-delete t)
(setopt evil-want-C-u-delete t)
(setopt evil-want-fine-undo t)
(setopt evil-move-beyond-eol t)
(setopt evil-search-wrap nil)
(setopt evil-want-Y-yank-to-eol t)

(evil-mode 1)

;; C-u scrolls a half page rather than starting a prefix argument.
(keymap-set evil-normal-state-map "C-u" #'evil-scroll-up)
(keymap-set evil-visual-state-map "C-u" #'evil-scroll-up)
(keymap-set evil-motion-state-map "C-u" #'evil-scroll-up)

;; C-g always escapes to normal state.
(keymap-set evil-insert-state-map "C-g" #'evil-normal-state)
(keymap-set evil-visual-state-map "C-g" #'evil-normal-state)
(keymap-set evil-replace-state-map "C-g" #'evil-normal-state)
(keymap-set evil-operator-state-map "C-g" #'evil-normal-state)

;;; Evil integration for the rest of Emacs

;; Read by evil-collection-init, so it precedes the require.
(setq evil-collection-setup-minibuffer t)

(require 'evil-collection)
(evil-collection-init)

;;; Multiple cursors

;; Deferred, as it was under use-package: the mode comes on when one of
;; evil-mc's own commands first loads the package.
(with-eval-after-load 'evil-mc
  (global-evil-mc-mode 1))

;;; Surround

(setopt evil-surround-pairs-alist
        '((?\( . ("(" . ")"))
          (?\[ . ("[" . "]"))
          (?\{ . ("{" . "}"))

          (?\) . ("(" . ")"))
          (?\] . ("[" . "]"))
          (?\} . ("{" . "}"))

          (?< . ("<" . ">"))
          (?> . ("<" . ">"))))

(add-hook 'after-init-hook #'global-evil-surround-mode)

;;; Commenting

(evil-define-operator my-evil-comment-or-uncomment (beg end)
  "Toggle comment for the region between BEG and END."
  (interactive "<r>")
  (comment-or-uncomment-region beg end))

(evil-define-key 'normal 'global (kbd "gc") #'my-evil-comment-or-uncomment)

;;; Moving lines

(keymap-global-set "M-p" #'move-text-up)
(keymap-global-set "M-n" #'move-text-down)

(provide 'rc-evil)
;;; rc-evil.el ends here
