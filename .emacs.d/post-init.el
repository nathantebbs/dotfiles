;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; The last file minimal-emacs.d loads. It installs whatever pre-init.el
;; declared and then requires the modules in configs/, which is where the
;; actual configuration lives.
;;
;; Order matters only at the top: rc-defaults repairs PATH, which anything
;; shelling out depends on, and rc-ui applies the theme.

;;; Code:

;; Install what this machine is missing. The vc call comes first: with a vc
;; package declared, the archive pass would otherwise look for it and fail.
(package-vc-install-selected-packages)
(package-install-selected-packages :no-confirm)

;; early-init.el points `custom-file' here but never loads it, so anything set
;; through customize was written and ignored. Before the modules, so they win.
(when (file-exists-p custom-file)
  (load custom-file nil :nomessage))

(require 'rc-defaults)
(require 'rc-ui)
(require 'rc-completion)
(require 'rc-evil)
(require 'rc-editing)
(require 'rc-elisp)
(require 'rc-programming)
(require 'rc-odin)
(require 'rc-org)
(require 'rc-git)
(require 'rc-session)
(require 'rc-terminal)

;;; post-init.el ends here
