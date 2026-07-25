;;; rc-defaults.el --- Built-in state and the shell environment -*- lexical-binding: t; -*-

;;; Commentary:

;; The built-in minor modes that persist state across sessions, plus Dired and
;; auto-save.
;;
;; minimal-emacs.d sets the variables for most of these but deliberately does
;; not turn the modes on, so the `add-hook' calls below are what actually
;; enables them. Only settings that differ from upstream's are repeated here.

;;; Code:

;;; Shell environment

;; A GUI Emacs inherits none of the shell's PATH, and a launchd daemon gets an
;; even barer environment while reporting `window-system' as nil.
(when (or (daemonp) (memq window-system '(mac ns x)))
  ;; A login shell is enough; the default "-l -i" pays for an interactive rc
  ;; file at every startup.
  (setopt exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;; Auto revert

(setopt auto-revert-interval 3)
(setopt auto-revert-use-notify t)
(setopt auto-revert-avoid-polling nil)
(setopt auto-revert-verbose t)

(add-hook 'after-init-hook #'global-auto-revert-mode)

;;; Recent files

;; A daemon outlives many edits, so let it prune periodically; a foreground
;; Emacs only needs the cleanup that runs on exit.
(setopt recentf-auto-cleanup (if (daemonp) 300 'never))
(setopt recentf-exclude
        (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
              "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
              "\\.7z$" "\\.rar$"
              "COMMIT_EDITMSG\\'"
              "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
              "-autoloads\\.el$" "autoload\\.el$"))

(add-hook 'after-init-hook #'recentf-mode)

;; Depth -90 puts the cleanup ahead of the `recentf-save-list' that
;; `recentf-mode' adds to `kill-emacs-hook', so stale entries never get saved.
;; Registered only once recentf is loaded, or the hook calls a void function.
(with-eval-after-load 'recentf
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;;; Minibuffer history

(setopt savehist-autosave-interval 600)

;; Upstream's list without kill-ring; carrying the clipboard across restarts
;; is the point of enabling this at all.
(setopt savehist-additional-variables
        '(kill-ring                        ; clipboard
          register-alist                   ; macros
          mark-ring global-mark-ring       ; marks
          search-ring regexp-search-ring)) ; searches

(add-hook 'after-init-hook #'savehist-mode)

;;; Cursor position

(setopt save-place-limit 400)

(add-hook 'after-init-hook #'save-place-mode)

;;; Dired

;; macOS ships BSD ls, which has no --dired; without this Dired complains on
;; every listing. GNU ls via coreutils would be the alternative.
(setopt dired-use-ls-dired nil)

(with-eval-after-load 'dired
  (require 'dired-x)
  ;; Append to upstream's pattern rather than replacing it.
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
  (setq dired-listing-switches "-alh"))

;;; Auto save

;; Emacs defaults this to t, but minimal-emacs.d set it to nil as recently as
;; 1.3.1, so state it rather than inherit it.
(setopt auto-save-default t)
(setopt auto-save-interval 300)
(setopt auto-save-timeout 30)

;; Unlike `auto-save-mode', this writes the file itself rather than a #file#
;; alongside it, and only for buffers actually visiting a file.
(setopt auto-save-visited-interval 5)
(auto-save-visited-mode 1)

(provide 'rc-defaults)
;;; rc-defaults.el ends here
