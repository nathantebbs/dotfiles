# .emacs.d

Emacs configuration built on [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d)
(v1.5.1), using the built-in `package.el`. No third-party package manager and
no `use-package`.

## Layout

`early-init.el` and `init.el` are upstream's, copied verbatim. **Do not edit
them**; replace them wholesale when updating minimal-emacs.d. Everything else
is mine, hooked into the points upstream provides.

| File | Role |
|------|------|
| `pre-early-init.el` | Puts `configs/` on `load-path`; reports startup time into `*scratch*` |
| `pre-init.el` | The package manifest, read before `package-initialize` |
| `post-init.el` | Installs what is missing, loads `custom-file`, requires the modules |
| `configs/rc-*.el` | The actual configuration, one file per concern |

Modules are ordinary Elisp with `provide`/`require`. `rc-defaults` comes first
because it repairs `PATH`, which everything shelling out depends on.

| Module | Covers |
|--------|--------|
| `rc-defaults` | `PATH`, recentf, savehist, saveplace, auto-revert, Dired, auto-save |
| `rc-ui` | Font, theme, line numbers, mode line, scrolling, `winner-mode` |
| `rc-completion` | Vertico, Orderless, Marginalia, Consult, Embark, Corfu, Cape |
| `rc-evil` | Evil, evil-collection, evil-mc, evil-surround, undo-fu, move-text |
| `rc-editing` | Outline folding, stripspace, apheleia, YASnippet, spelling |
| `rc-elisp` | paredit, aggressive-indent, highlight-defined, helpful |
| `rc-programming` | C/C++ style, Haskell, Python, Zig, Markdown |
| `rc-odin` | `odin-ts-mode`, written here rather than installed |
| `rc-org` | Org, agenda, pdf-tools |
| `rc-git` | Magit |
| `rc-session` | easysession |
| `rc-terminal` | ghostel and its Evil integration |

## Packages

Declared in `pre-init.el` as `package-selected-packages` and installed on
first start. Everything comes from ELPA; nothing is pulled from a repository.

Org is deliberately **not** in the manifest: Emacs ships a current one, and a
second copy from ELPA races the built-in for load order.

To update everything:

```
M-x package-upgrade-all
```

## Odin

`odin-mode` was a regex mode that never matched procedure calls or field
access, which left most of a buffer unfontified, so `configs/rc-odin.el`
replaces it with a tree-sitter mode written here. It covers font lock,
indentation, imenu and the compilation error format `odin build` emits.

The grammar is not vendored. On a new machine:

```
M-x rc-odin-install-grammar
```

That compiles [tree-sitter-odin](https://github.com/tree-sitter-grammars/tree-sitter-odin)
into `.emacs.d/tree-sitter/`, which is gitignored.

## Emacs itself

Built from source with
[build-emacs-for-macos](https://github.com/jimeh/build-emacs-for-macos) and
installed to `/Applications/Emacs.app`. Do **not** `brew install emacs`; a
second install means two binaries and two launchd agents contending for the
same daemon socket.

## Daemon

Emacs runs as a launchd agent started at login, and `emacsclient` is the way
in. See the [Emacs daemon](../README.md#emacs-daemon) section for `emacsctl`.

Because the daemon has no frame at startup, the font and theme are applied
from `server-after-make-frame-hook` rather than at load time, and
`exec-path-from-shell` runs an interactive login shell so that `PATH` picks up
everything `config.zsh` exports.

## No LSP

Eglot is not configured. Formatting is handled by apheleia, and cc-mode
indents C and C++ as you type.
