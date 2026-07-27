;;; pre-early-init.el --- Load path and startup report -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; The first file minimal-emacs.d loads. Nothing in configs/ is reachable until
;; that directory is on the load path, so it goes in before anything else runs.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "configs/" user-emacs-directory))

;; *scratch* comes up in `fundamental-mode' (see `initial-major-mode' in
;; early-init.el), which does no font locking, hence the mode switch.
(defun rc-display-startup-time ()
  "Write startup time and package count as comments atop *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (unless (derived-mode-p 'lisp-interaction-mode)
      (lisp-interaction-mode))
    (goto-char (point-min))
    (insert (format ";; Startup Time: %.2fs\n;; Packages: %d\n\n"
                    (float-time (time-subtract after-init-time before-init-time))
                    (length package-activated-list)))))

(add-hook 'emacs-startup-hook #'rc-display-startup-time 100)

;;; pre-early-init.el ends here
