;;; post-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;; The last file minimal-emacs.d loads. It installs whatever pre-init.el
;; declared and then requires the modules in configs/, which is where the
;; actual configuration lives.
;;
;; Order matters only at the top: rc-defaults repairs PATH, which anything
;; shelling out depends on, and rc-ui applies the theme.

;;; Code:

;; Install what this machine does not have yet. The repository packages go
;; first: package-install-selected-packages would otherwise go looking for
;; them in the archives and fail.
(package-vc-install-selected-packages)
(package-install-selected-packages :no-confirm)

(require 'rc-defaults)
(require 'rc-ui)
(require 'rc-completion)
(require 'rc-evil)
(require 'rc-editing)
(require 'rc-elisp)
(require 'rc-programming)
(require 'rc-org)
(require 'rc-git)
(require 'rc-session)
(require 'rc-terminal)

;;; post-init.el ends here
