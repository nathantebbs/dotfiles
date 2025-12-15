;; -*- lexical-binding: t; -*-
;; early-init.el — runs before init.el/.emacs and before first GUI frame

;; Make startup faster/less janky
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024)
      inhibit-compacting-font-caches t)

;; Don’t let package.el initialize before straight
(setq package-enable-at-startup nil)

;; UI chrome off as early as possible (prevents visible “flash”)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; IMPORTANT: set default font for the *first* frame.
;; (This is the piece your current config is missing.)
(add-to-list 'default-frame-alist '(font . "Zenbones Brainy-16"))

(message "EARLY-INIT LOADED: %s" early-init-file)
