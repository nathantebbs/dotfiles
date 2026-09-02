;;; rc-zig.el --- Zig support -*- lexical-binding: t; -*-

;;; Commentary:

;; zig-mode supplies syntax and indentation. Apheleia runs zig fmt on save,
;; and compile builds the nearest package.

;;; Code:

(defun rc-zig--project-root ()
  "Return the nearest parent directory containing build.zig."
  (locate-dominating-file default-directory "build.zig"))

(defun rc-zig-set-compile-command ()
  "Build the Zig package that owns the current buffer."
  (when-let* ((root (rc-zig--project-root)))
    (setq-local compile-command
                (format "cd %s && zig build"
                        (shell-quote-argument root)))))

(add-hook 'zig-mode-hook #'rc-zig-set-compile-command)

(provide 'rc-zig)
;;; rc-zig.el ends here
