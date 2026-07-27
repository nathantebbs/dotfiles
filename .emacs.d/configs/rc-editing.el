;;; rc-editing.el --- Folding, whitespace, snippets, formatting -*- lexical-binding: t; -*-

;;; Commentary:

;; Editing behaviour that is not tied to any one language: outline folding,
;; trailing whitespace, snippet expansion, on-save formatting and spelling.

;;; Code:

;;; Outline folding

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

(defun rc-editing-outline-ellipsis ()
  "Show folded text as \" ▼\" rather than the default \"...\"."
  (let* ((display-table (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
    (set-display-table-slot display-table 'selective-display value)
    (setq buffer-display-table display-table)))

(add-hook 'outline-minor-mode-hook #'rc-editing-outline-ellipsis)

;;; Indentation-based folding

(setopt outline-indent-ellipsis " ▼")

(add-hook 'python-mode-hook #'outline-indent-minor-mode)
(add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

;;; Trailing whitespace

;; nil strips always; non-nil would only strip buffers that started clean.
(setopt stripspace-only-if-initially-clean nil)
;; Keep the cursor column after stripping, so saving does not move point.
(setopt stripspace-restore-column t)

(add-hook 'prog-mode-hook #'stripspace-local-mode)
(add-hook 'text-mode-hook #'stripspace-local-mode)
(add-hook 'conf-mode-hook #'stripspace-local-mode)

;;; Formatting on save

(add-hook 'prog-mode-hook #'apheleia-mode)

;;; Snippets

(setq yas-verbosity 0)

(setopt yas-also-auto-indent-first-line t)
(setopt yas-also-indent-empty-lines t)
;; Revival breaks undo.
(setopt yas-snippet-revival nil)
(setopt yas-wrap-around-region nil)

(add-hook 'after-init-hook #'yas-global-mode)

;;; Spelling

(setopt ispell-program-name "aspell")

;; UTF-8, alphabetic word characters, apostrophes inside words, en_US.
(setopt ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

;; --sug-mode=ultra is markedly slower, since Flyspell round-trips every word
;; through the Aspell process while typing.
(setopt ispell-extra-args '("--lang=en_US"))

(with-eval-after-load 'flyspell
  ;; Do not spell-check strings or docstrings in code.
  (setq flyspell-prog-text-faces (delq 'font-lock-string-face
                                       flyspell-prog-text-faces))
  (setq flyspell-prog-text-faces (delq 'font-lock-doc-face
                                       flyspell-prog-text-faces)))

(provide 'rc-editing)
;;; rc-editing.el ends here
