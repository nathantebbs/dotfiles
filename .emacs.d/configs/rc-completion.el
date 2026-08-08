;;; rc-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; The Minad/Oantolin stack: Vertico for the minibuffer, Orderless for
;; matching, Marginalia for annotations, Consult for the commands, Embark for
;; acting on candidates, and Corfu plus Cape for completion in the buffer.
;;
;; Keys bound to autoloaded commands need no with-eval-after-load; keys bound
;; inside another package's keymap do, because the map does not exist until
;; that package loads.

;;; Code:

;;; Minibuffer UI

(vertico-mode)

(setopt completion-styles '(orderless basic))
(setopt completion-category-defaults nil)
(setopt completion-category-overrides '((file (styles partial-completion))))

(add-hook 'after-init-hook #'marginalia-mode)

;;; In-buffer completion

;; Hide commands in M-x that do not apply to the current mode.
(setopt read-extended-command-predicate #'command-completion-default-include-p)
;; Ispell's completion is a poor fit here; cape-dict is the alternative.
(setopt text-mode-ispell-word-completion nil)
(setopt tab-always-indent 'complete)

(setopt corfu-auto t)
(setopt corfu-auto-delay 0.15)
(setopt corfu-auto-prefix 1)
(setopt corfu-cycle t)

;; Corfu ends up on everywhere, but lazily: the mode hooks are only the trigger
;; that loads the package, and `global-corfu-mode' takes over from the first
;; prog, shell or eshell buffer onward. Calling it at top level instead would
;; load Corfu during startup.
(add-hook 'prog-mode-hook #'corfu-mode)
(add-hook 'shell-mode-hook #'corfu-mode)
(add-hook 'eshell-mode-hook #'corfu-mode)

(with-eval-after-load 'corfu
  (global-corfu-mode))

;;; Completion at point extensions

(keymap-global-set "C-c p" #'cape-prefix-map)

;; Buffer-local, not global: the global value is inherited by the minibuffer
;; and every special buffer, where dabbrev and filename completion only get in
;; the way of the completion the buffer already provides.
;;
;; Depth 90 matters. `add-hook' prepends by default, which would put these
;; ahead of the major mode's own capf and let dabbrev answer first with a worse
;; candidate set. In the global value they sat behind it, at the local list's
;; trailing t, and that order is what is being preserved here.
(defun rc-completion-add-capfs ()
  "Add the general-purpose Cape backends to the current buffer."
  (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)
  (add-hook 'completion-at-point-functions #'cape-file 90 t))

(add-hook 'prog-mode-hook #'rc-completion-add-capfs)
(add-hook 'text-mode-hook #'rc-completion-add-capfs)
(add-hook 'conf-mode-hook #'rc-completion-add-capfs)

;; Only useful where an Elisp src block can appear.
(defun rc-completion-add-elisp-block-capf ()
  "Add `cape-elisp-block' to the current buffer."
  (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))

(add-hook 'org-mode-hook #'rc-completion-add-elisp-block-capf)
(add-hook 'markdown-mode-hook #'rc-completion-add-elisp-block-capf)

;;; Embark

(setopt prefix-help-command #'embark-prefix-help-command)

(keymap-global-set "C-." #'embark-act)
(keymap-global-set "C-;" #'embark-dwim)
(keymap-global-set "C-h B" #'embark-bindings)

(with-eval-after-load 'embark
  ;; Hide the mode line of the Embark live and completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;;; Consult

(setopt register-preview-delay 0.5)
(setopt register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)

(setopt xref-show-xrefs-function #'consult-xref)
(setopt xref-show-definitions-function #'consult-xref)

(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

(keymap-global-set "C-c M-x" #'consult-mode-command)
(keymap-global-set "C-c h" #'consult-history)
(keymap-global-set "C-c k" #'consult-kmacro)
(keymap-global-set "C-c m" #'consult-man)
(keymap-global-set "C-c i" #'consult-info)
(keymap-global-set "<remap> <Info-search>" #'consult-info)

(keymap-global-set "C-x M-:" #'consult-complex-command)
(keymap-global-set "C-x b" #'consult-buffer)
(keymap-global-set "C-x 4 b" #'consult-buffer-other-window)
(keymap-global-set "C-x 5 b" #'consult-buffer-other-frame)
(keymap-global-set "C-x t b" #'consult-buffer-other-tab)
(keymap-global-set "C-x r b" #'consult-bookmark)
(keymap-global-set "C-x p b" #'consult-project-buffer)

(keymap-global-set "M-#" #'consult-register-load)
(keymap-global-set "M-'" #'consult-register-store)
(keymap-global-set "C-M-#" #'consult-register)
(keymap-global-set "M-y" #'consult-yank-pop)

(keymap-global-set "M-g e" #'consult-compile-error)
(keymap-global-set "M-g f" #'consult-flymake)
(keymap-global-set "M-g g" #'consult-goto-line)
(keymap-global-set "M-g M-g" #'consult-goto-line)
(keymap-global-set "M-g o" #'consult-outline)
(keymap-global-set "M-g m" #'consult-mark)
(keymap-global-set "M-g k" #'consult-global-mark)
(keymap-global-set "M-g i" #'consult-imenu)
(keymap-global-set "M-g I" #'consult-imenu-multi)

(keymap-global-set "M-s d" #'consult-find)
(keymap-global-set "M-s c" #'consult-locate)
(keymap-global-set "M-s g" #'consult-grep)
(keymap-global-set "M-s G" #'consult-git-grep)
(keymap-global-set "M-s r" #'consult-ripgrep)
(keymap-global-set "M-s l" #'consult-line)
(keymap-global-set "M-s L" #'consult-line-multi)
(keymap-global-set "M-s k" #'consult-keep-lines)
(keymap-global-set "M-s u" #'consult-focus-lines)
(keymap-global-set "M-s e" #'consult-isearch-history)

;; Both maps come from preloaded files, so they can be set directly.
(keymap-set isearch-mode-map "M-e" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-s e" #'consult-isearch-history)
(keymap-set isearch-mode-map "M-s l" #'consult-line)
(keymap-set isearch-mode-map "M-s L" #'consult-line-multi)

(keymap-set minibuffer-local-map "M-s" #'consult-history)
(keymap-set minibuffer-local-map "M-r" #'consult-history)

(with-eval-after-load 'consult
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(provide 'rc-completion)
;;; rc-completion.el ends here
