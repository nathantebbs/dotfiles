;;; pre-early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;; The rc-*.el modules that post-init.el requires. This has to happen here,
;; before anything on the load path is looked up.
(add-to-list 'load-path
             (expand-file-name "configs/" user-emacs-directory))

;; DEBUG MODE
;; Uncomment when actively debugging; leaving this on pops a backtrace on every
;; error during normal use.
;; (setq debug-on-error t)

;; Write startup time and package count as comments atop the *scratch* buffer.
;; The scratch buffer defaults to `fundamental-mode' (see `initial-major-mode'
;; in early-init.el), which has no font-locking; switch it to
;; `lisp-interaction-mode' so the comment lines are highlighted.
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
