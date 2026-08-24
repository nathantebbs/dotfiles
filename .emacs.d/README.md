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
| `rc-programming` | Python (tree-sitter, pyvenv, Flymake), Markdown |
| `rc-cc` | C and C++: tree-sitter modes, Eglot driving clangd, CMake |
| `rc-odin` | `odin-ts-mode`, written here rather than installed |
| `rc-org` | Org, agenda, pdf-tools |
| `rc-git` | Magit |
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

## Python

`python-ts-mode` through `treesit-enabled-modes`, but only where the grammar is
present, so a fresh clone falls back to `python-mode` rather than erroring.
python.el registers its own commit-pinned grammar source and its own remap
entry, so neither is repeated here. On a new machine:

```
M-x rc-programming-install-python-grammar
```

The toolchain is [uv](https://github.com/astral-sh/uv) and
[ruff](https://github.com/astral-sh/ruff), both from the Brewfile.

uv puts its environment at `.venv` in the project root and exports nothing, so
opening a Python file walks up for that directory and hands it to pyvenv. That
is what puts the project's own interpreter and its `ruff` on `exec-path`, which
is what everything below then finds.

Linting is Flymake over `ruff check`, falling back to `flake8` on a machine
where `brew bundle` has not run. Both print `stdin:line:col: CODE message`,
which is already the pattern Emacs expects. Only a file that will not run is an
error: an undefined name and a syntax error. Import ordering and complexity are
notes, and everything else is a warning.

Formatting is apheleia running `ruff-isort` then `ruff format`, which is the
pair that replaces isort and black. apheleia's own default for Python is black,
which a uv setup does not install, so it is overridden.

## C and C++

The target is a CMake project that exports a compilation database. On a new
machine, once:

```
M-x rc-cc-install-grammars
```

That builds the C, C++, Doxygen and CMake grammars into
`.emacs.d/tree-sitter/`. Then, once per project:

```
M-x rc-cc-cmake-configure
```

That runs `cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON`. **Nothing
crossing a translation unit works before that database exists**: clangd falls
back to guessing the flags for every file. clangd finds `build/` on its own,
so there is nothing to point at it. `compile` is set to `cmake --build
<root>/build` in a CMake project, so `C-c b` builds the same tree.

`c-ts-mode` and `c++-ts-mode` come in through `treesit-enabled-modes`, but only
where both grammars are present. Without them Emacs would still enter
`c-ts-mode` and leave the buffer with no font lock and no indentation, so a
fresh clone stays in cc-mode until the install command has run.

### Formatting and indentation

Formatting on save is apheleia running `clang-format`, and it **does** pick up
a project's own `.clang-format`: apheleia passes `-assume-filename` with the
buffer's real path, so clang-format walks up from the file the way it would on
the command line. A project file beats `~/.clang-format`.

That leaves a gap, which `rc-cc-follow-clang-format` closes. Emacs indents
while you type from its own settings, so in a project whose `.clang-format`
says two columns you would type at four and watch every save move the line.
Opening a C or C++ buffer asks `clang-format --dump-config` what actually
applies to that file and sets `c-ts-indent-offset`, `c-basic-offset` and
`indent-tabs-mode` from the answer. Asking clang-format rather than parsing the
YAML is what resolves `BasedOnStyle` and nested directories correctly. One
process per directory, cached.

`BreakBeforeBraces` maps onto a `c-ts-mode` indent style as well, but only
inside a function body does that land where clang-format does. Every
`c-ts-mode` style cascades the indent of a brace that opens a namespace or a
class on its own line, so an Allman project still has those two corrected on
save rather than while typing.

Defaults for a file no `.clang-format` covers are K&R at four columns with no
tabs, matching `~/.clang-format`. The `linux` style is the closer match to that
file's `BreakBeforeBraces` but forces `indent-tabs-mode` on, which the same
file turns off.

### The server

Eglot drives `clangd --background-index --clang-tidy`, the same two flags as
[`nvim/lsp/clangd.lua`](../nvim/lsp/clangd.lua). Background indexing answers
references and callers from the whole project rather than the open buffers, and
clang-tidy folds its checks into the diagnostics Flymake shows. A machine
without `clangd` opens C files with no server rather than erroring per buffer.

Completion is Corfu, with Eglot's capf landing ahead of the Cape backends
`rc-completion` adds at depth 90. Diagnostics are Flymake, which Eglot turns on
itself. Neither needed anything in `rc-cc.el`.

| Key | Does |
|-----|------|
| `M-.` / `M-,` | Definition, and back |
| `C-M-.` | Workspace symbol |
| `M-g f` | Diagnostics, through `consult-flymake` |
| `C-c b` | Build, through `compile` |
| `C-c o` | Switch between source and header |
| `C-c l a` | Code actions |
| `C-c l d` | Documentation buffer |
| `C-c l f` | Format region or buffer through the server |
| `C-c l i` | Toggle inlay hints |
| `C-c l r` | Rename |
| `C-c l D` | Declaration |
| `C-c l m` | Implementation |
| `C-c l t` | Type definition |
| `C-c l R` | Reconnect the server |

`C-c o` asks clangd for the counterpart, which beats matching basenames once
the header is under `include/` and the source under `src/`. It falls back to
`ff-find-other-file` in a buffer with no server.

No debugger is wired in. `lldb` runs in a terminal.

C and C++ are the only things here that start a server, which is why the Eglot
configuration lives in `rc-cc.el` rather than a module of its own. A second
consumer is when to lift it out.

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
everything `config.bash` exports. Interactive matters: `~/.bashrc` returns
immediately in a non-interactive shell, and it is the file that sources
`config.bash`, so `-l` on its own would come back with none of it.
