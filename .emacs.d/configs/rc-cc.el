;;; rc-cc.el --- C and C++ support -*- lexical-binding: t; -*-

;;; Commentary:

;; C and C++ get a language server where the rest of this configuration does
;; not. clangd is the reason: it is the only tool here that reads the build
;; system, so cross-file navigation, completion of a struct's own fields and
;; clang-tidy diagnostics all come from one process nothing else can stand in
;; for. Python keeps its ruff and Flymake setup, which covers the same ground
;; for that language without a server.
;;
;; The target is a CMake project that exports a compilation database.
;; `rc-cc-cmake-configure' writes one, clangd finds it in build/ without being
;; told where to look, and `compile' builds the same tree. Nothing crossing a
;; translation unit works before that database exists, so it is the one step
;; a new project needs.
;;
;; CMake lives here rather than in rc-programming because in this setup it
;; exists to build C++, and splitting the build system from the language it
;; builds only means reading two files to answer one question.
;;
;; Three things are deliberately absent. Formatting is apheleia running
;; clang-format from `prog-mode-hook' in rc-editing, which already resolves a
;; project's own .clang-format. Completion is Corfu, with Eglot's capf landing
;; ahead of the Cape backends rc-completion adds at depth 90. Diagnostics are
;; Flymake, which Eglot turns on itself. None of them need anything here.
;;
;; The Eglot bindings live in this file rather than in a module of their own
;; because C and C++ are the only things in this configuration that start a
;; server. A second consumer is when to lift them out.

;;; Code:

(require 'treesit)

;; Loaded by the hooks below rather than at startup: `project-current' is
;; autoloaded and pulls in the rest of project.el on first use.
(declare-function project-current "project")
(declare-function project-root "project")

;; Eglot functions used by `rc-cc-find-other-file', which runs in a buffer
;; where Eglot has already loaded.
(declare-function eglot-current-server "eglot")
(declare-function eglot-path-to-uri "eglot")
(declare-function eglot-uri-to-path "eglot")

(defgroup rc-cc nil
  "C and C++ support."
  :group 'languages)

;;; Tree-sitter

;; The modes register their own pinned grammar sources when they load, so
;; requiring them is what makes these installable and why no source is
;; repeated here.
(defun rc-cc-install-grammars ()
  "Compile and install the C, C++, Doxygen and CMake tree-sitter grammars."
  (interactive)
  (require 'c-ts-mode)
  (require 'cmake-ts-mode)
  (dolist (lang '(c cpp doxygen cmake))
    (treesit-install-language-grammar lang)))

;; c-ts-mode's autoloads have already put the c, c++ and c-or-c++ entries in
;; `treesit-major-mode-remap-alist'. Setting this option is what copies them
;; into `major-mode-remap-alist'.
;;
;; Appended rather than assigned, since rc-programming enables python-ts-mode
;; through the same option.
;;
;; Guarded on both grammars: without them Emacs would still enter c-ts-mode and
;; leave the buffer with no font lock and no indentation, so a fresh clone is
;; better off in cc-mode until `rc-cc-install-grammars' has run.
(when (and (treesit-ready-p 'c t) (treesit-ready-p 'cpp t))
  (setopt treesit-enabled-modes
          (append treesit-enabled-modes
                  '(c-ts-mode c++-ts-mode c-or-c++-ts-mode))))

;; Fontifies @param and @return inside /** */, and is off by default.
(setopt c-ts-mode-enable-doxygen (treesit-ready-p 'doxygen t))

;;; Indentation

;; The fallback, for a file no .clang-format covers. K&R at four columns and no
;; tabs is what ~/.clang-format says. The `linux' style is the closer match to
;; that file's BreakBeforeBraces, but it forces `indent-tabs-mode' on, which
;; the same file turns off.
(setopt c-ts-mode-indent-style 'k&r)
(setopt c-ts-indent-offset 4)

(defun rc-cc-set-cc-mode-style ()
  "Set the K&R style and a four column offset for the current C buffer.
The cc-mode half of the defaults above, which cc-mode does not read."
  (c-set-style "k&r")
  (setq c-basic-offset 4
        indent-tabs-mode nil))

;; cc-mode's own hooks, not `c-mode-common-hook', which also fires for
;; java-mode and awk-mode, where a C style and a CMake build do not belong.
(defconst rc-cc--hooks
  '(c-mode-hook c++-mode-hook objc-mode-hook c-ts-base-mode-hook)
  "Every mode hook this file configures, cc-mode's and tree-sitter's alike.")

(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
  (add-hook hook #'rc-cc-set-cc-mode-style))

;; A project with its own .clang-format is reformatted to that file's rules on
;; every save, so typing to a different width means each save moves the line
;; that was just written. clang-format resolves the whole cascade, BasedOnStyle
;; and nested directories included, so ask it rather than parsing the YAML.
(defvar rc-cc--style-cache (make-hash-table :test #'equal)
  "Resolved clang-format configurations, keyed by directory.
Resolving costs a process, and every file in a directory resolves alike.")

(defun rc-cc--dump-config (file)
  "Return clang-format's resolved configuration for FILE, or nil."
  (when (executable-find "clang-format")
    (with-temp-buffer
      (and (eq 0 (call-process "clang-format" nil t nil "--dump-config"
                               (concat "-assume-filename=" file)))
           (buffer-string)))))

(defun rc-cc--config-value (config key)
  "Return the value KEY takes in CONFIG, or nil if it is absent."
  (and (string-match (format "^%s: +\\(.+?\\) *$" (regexp-quote key)) config)
       (match-string 1 config)))

;; Only inside a function body does this land where clang-format does. Every
;; c-ts-mode style cascades the indent of a brace that opens a namespace or a
;; class on its own line, so those two come out wrong whatever is chosen here.
(defun rc-cc--brace-style (breaking)
  "Return the c-ts-mode indent style matching a BreakBeforeBraces of BREAKING."
  (pcase breaking
    ("Allman" 'bsd)
    ("GNU" 'gnu)
    ;; No Whitesmiths or Stroustrup here, and the attaching styles all land on
    ;; K&R anyway.
    (_ 'k&r)))

(defun rc-cc-follow-clang-format ()
  "Indent this buffer the way clang-format will reformat it on save."
  (when buffer-file-name
    (let* ((dir (file-name-directory buffer-file-name))
           (config (with-memoization (gethash dir rc-cc--style-cache)
                     (or (rc-cc--dump-config buffer-file-name) ""))))
      (when-let* ((width (rc-cc--config-value config "IndentWidth")))
        (setq-local c-ts-indent-offset (string-to-number width))
        (setq-local c-basic-offset (string-to-number width)))
      (when-let* ((tabs (rc-cc--config-value config "UseTab")))
        (setq-local indent-tabs-mode (not (equal tabs "Never"))))
      (when-let* (((derived-mode-p 'c-ts-mode 'c++-ts-mode))
                  (braces (rc-cc--config-value config "BreakBeforeBraces")))
        ;; Buffer-local, and rebuilds the indent rules from the new style.
        (c-ts-mode-set-style (rc-cc--brace-style braces))))))

(dolist (hook rc-cc--hooks)
  ;; Depth, so the defaults above are in place before this overrides them.
  (add-hook hook #'rc-cc-follow-clang-format 90))

;;; CMake

(defcustom rc-cc-cmake-build-directory "build"
  "Directory, relative to the project root, holding the CMake build tree.
clangd looks for compile_commands.json at the project root and in this
directory, so the two have to agree."
  :type 'string
  :group 'rc-cc)

(defun rc-cc--cmake-root ()
  "Return the project root, if it holds a CMakeLists.txt."
  (when-let* ((project (project-current))
              (root (project-root project)))
    (and (file-exists-p (expand-file-name "CMakeLists.txt" root)) root)))

(defun rc-cc-cmake-configure ()
  "Configure the CMake build tree so that it exports a compilation database.
clangd reads compile_commands.json out of the build directory, and until
it exists every file is compiled with guessed flags."
  (interactive)
  (let* ((root (or (rc-cc--cmake-root)
                   (user-error "No CMakeLists.txt at the project root")))
         (default-directory root))
    (compile (format "cmake -S . -B %s -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                     (shell-quote-argument rc-cc-cmake-build-directory)))))

;; Absolute, so `compile' builds the project from whichever directory the
;; buffer happens to sit in.
(defun rc-cc-set-compile-command ()
  "Point `compile' at the CMake build tree, when the project has one."
  (when-let* ((root (rc-cc--cmake-root)))
    (setq-local compile-command
                (format "cmake --build %s"
                        (shell-quote-argument
                         (expand-file-name rc-cc-cmake-build-directory root))))))

(dolist (hook rc-cc--hooks)
  (add-hook hook #'rc-cc-set-compile-command))

;;; Eglot

;; --background-index indexes the whole project, so references and callers are
;; answered from more than the open buffers, and --clang-tidy folds its checks
;; into the diagnostics Flymake shows. The two flags nvim/lsp/clangd.lua passes.
;;
;; Prepended, which is what makes it win over Eglot's own entry for these modes.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                 . ("clangd" "--background-index" "--clang-tidy")))

  ;; Everything else is already bound: `M-.' and `M-,' reach definitions and
  ;; back through Eglot's xref backend, `C-M-.' reaches workspace symbols, and
  ;; `M-g f' lists the diagnostics through consult-flymake.
  (keymap-set eglot-mode-map "C-c l a" #'eglot-code-actions)
  (keymap-set eglot-mode-map "C-c l d" #'eldoc-doc-buffer)
  (keymap-set eglot-mode-map "C-c l f" #'eglot-format)
  (keymap-set eglot-mode-map "C-c l i" #'eglot-inlay-hints-mode)
  (keymap-set eglot-mode-map "C-c l r" #'eglot-rename)
  (keymap-set eglot-mode-map "C-c l D" #'eglot-find-declaration)
  (keymap-set eglot-mode-map "C-c l R" #'eglot-reconnect)
  (keymap-set eglot-mode-map "C-c l m" #'eglot-find-implementation)
  (keymap-set eglot-mode-map "C-c l t" #'eglot-find-typeDefinition))

;; Resolved once at startup, after rc-defaults has repaired PATH. A machine
;; without clangd then opens C files with no server rather than raising an
;; error in every buffer, which is how nvim/lsp/ treats a missing binary too.
(when (executable-find "clangd")
  (dolist (hook rc-cc--hooks)
    (add-hook hook #'eglot-ensure)))

;;; Source and header

;; clangd reads the pairing off the compilation database, which beats matching
;; basenames once the header is under include/ and the source under src/.
;; `ff-find-other-file' is the fallback for a buffer with no server.
(defun rc-cc-find-other-file ()
  "Switch between this file and its header or source counterpart."
  (interactive)
  (let* ((server (and (fboundp 'eglot-current-server) (eglot-current-server)))
         (uri (and server buffer-file-name
                   (jsonrpc-request server :textDocument/switchSourceHeader
                                    (list :uri (eglot-path-to-uri
                                                buffer-file-name))))))
    (if (and (stringp uri) (not (string-empty-p uri)))
        (find-file (eglot-uri-to-path uri))
      (ff-find-other-file))))

(with-eval-after-load 'cc-mode
  (keymap-set c-mode-base-map "C-c o" #'rc-cc-find-other-file))

(with-eval-after-load 'c-ts-mode
  (keymap-set c-ts-base-mode-map "C-c o" #'rc-cc-find-other-file))

(provide 'rc-cc)
;;; rc-cc.el ends here
