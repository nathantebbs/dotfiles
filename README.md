# dotfiles

This repository stores configuration for the developer tools I use on macOS
and Linux. Shared configuration lives at the root. Platform integration lives
under [`macos/`](macos/) and [`linux/`](linux/).

The repository deploys configuration. It does not install Linux packages or
manage Linux services. macOS has an optional minimal
[`Brewfile`](macos/Brewfile). The macOS setup remains explicit.

## Layout

| Path | Ownership |
|------|-----------|
| `.emacs.d/`, `nvim/`, `.vimrc` | Shared editor configuration |
| `bashrc`, `bash_profile`, `config.bash` | Shared shell configuration |
| `tmux/`, `wezterm/` | Shared terminal configuration |
| `gitconfig`, `clang-format` | Shared tool configuration |
| `fonts/` | Font assets used by the terminal and editors |
| `macos/` | Homebrew manifest, launchd, AeroSpace, Karabiner, and macOS shell behavior |
| `linux/` | Distribution-neutral Linux behavior |
| `notes/` | Audits, historical notes, and Org design documents |

## Deployment

Clone the repository and run setup:

```sh
git clone --depth=1 https://github.com/nathantebbs/dotfiles
cd dotfiles
bash setup.sh
```

`setup.sh` detects macOS or Linux. It deploys the shared link manifest and the
selected platform manifest. Existing targets move to timestamped backups.
Correct links remain unchanged on later runs.

Review deployment without changing the home directory:

```sh
bash setup.sh --dry-run
```

The platform can be selected for testing:

```sh
bash setup.sh --platform macos --dry-run
bash setup.sh --platform linux --dry-run
```

Package installation, fonts, editor plugin bootstrap, and macOS system setup
are separate actions. See [`macos/README.md`](macos/README.md) or
[`linux/README.md`](linux/README.md) for the platform boundary.

The optional helpers are:

```sh
bash util/scripts/install-fonts.sh
bash util/scripts/install-vimplug.sh
bash macos/scripts/install-bash.sh
bash macos/scripts/make-emacsclient-app.sh
```

Shared link policy lives in `util/links.tsv`. Platform link policy lives in
`macos/links.tsv` and `linux/links.tsv`.

## Editors

Emacs is the primary GUI editor. Neovim owns `$EDITOR` and `$VISUAL` because
terminal editor calls are usually short tasks such as Git commit messages.
Vim remains the minimal backup editor.

### Vim

My `.vimrc` uses [vim-plug](https://github.com/junegunn/vim-plug) for plugin management. To install vim-plug on a new machine:

```sh
bash util/scripts/install-vimplug.sh
```

Then from within vim:

```txt
:PlugInstall
```

**Plugins:** fzf + fzf.vim, vim-surround, vim-todo-highlight, undotree

No language plugins. Vim 9.1 ships filetype detection, syntax, ftplugin and
indent for everything in use, odin and typst included, and its Odin files are
habamax/vim-odin upstreamed into the runtime.

**Key bindings:**

The leader is `<Space>`. Every binding of mine hangs off it, so Vim keeps its own Ctrl keys.

| Binding | Action |
|---------|--------|
| `<leader> f` | Files (fzf) |
| `<leader> b` | Buffers (fzf) |
| `<leader> l` | Lines in buffer (fzf `BLines`) |
| `<leader> /` | Grep the project (fzf `Rg`) |
| `<leader> m` | Maps (fzf) |
| `<leader> e` | Open netrw |
| `<leader> k` | Delete buffer |
| `<leader> w` | Write |
| `<leader> u` | Toggle undotree |
| `C-l` | Redraw and clear search highlight |
| `Y` | Yank to end of line, matching `D` and `C` |

`:PlugInstall` and `:PlugClean` have no binding. They run a few times a year, which is not often enough to spend a key on.

### Neovim

`nvim/init.lua` is a desktop-only Neovim config using [lazy.nvim](https://github.com/folke/lazy.nvim). lazy.nvim is bootstrapped automatically on first launch. No manual install needed.

It tracks the `.vimrc` deliberately: same leader, same bindings, same search and split and wildmenu behavior. Only what Neovim does not already do by default is restated, so settings like `hidden`, `autoread` and the `Y` and `C-l` mappings appear in the `.vimrc` and not here.

**Plugins:** Telescope (+ telescope-fzf-native), vim-surround, lightline, nvim-autopairs, todo-comments, presenting.nvim, undotree, vim-colors-modus, typst.vim

No vim-polyglot here, and none in `.vimrc` either. Neovim 0.12 and Vim 9.1 both ship filetype detection, syntax and indent for every language in use, including odin, zig and typst. typst.vim stays for its compiler integration, not its syntax.

**LSP:** Neovim's built-in client, so there is no lspconfig plugin. Each server's table is a file in `nvim/lsp/`, picked up off the runtimepath by name. A server whose binary is missing is skipped, so a machine without the toolchain still starts clean.

| Server | Languages | Command |
|--------|-----------|--------------|
| `clangd` | C, C++, Obj-C, CUDA | `clangd` |
| `gopls` | Go | `gopls` |
| `pyright` | Python | `pyright-langserver` |
| `tinymist` | Typst | Project environment |
| `ols` | Odin | Project environment |
| `zls` | Zig | `zls` |

**Key bindings:**

The leader is `<Space>`, the same as Vim's, and the bindings below it are the
same keys doing the same things. Telescope stands in for fzf.vim.

| Binding | Action |
|---------|--------|
| `<leader> f` | Files (Telescope) |
| `<leader> b` | Buffers (Telescope) |
| `<leader> l` | Lines in buffer (Telescope) |
| `<leader> /` | Grep the project (Telescope) |
| `<leader> m` | Maps (Telescope) |
| `<leader> e` | Open netrw |
| `<leader> k` | Delete buffer |
| `<leader> w` | Write |
| `<leader> u` | Toggle undotree |
| `<` / `>` (visual) | Shift and keep the selection |
| `C-c C-p i` | Lazy sync |
| `C-c C-p c` | Lazy clean |
| `gd` / `gD` / `gy` | LSP definition / declaration / type definition |
| `C-c C-d` | LSP diagnostics for the line |
| `C-c C-f` | LSP format buffer |

Neovim already binds `grn`, `gra`, `grr`, `gri`, `gO` and `K` for rename, code action, references, implementation, symbols and hover, so those are not rebound.

Theme: `modus` with a transparent terminal background. Lightline uses the
`one` colorscheme.

### Emacs

`.emacs.d/` is built on [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) and uses the built-in `package.el`. No third-party package manager, and no `use-package`: the configuration is plain Elisp split into `configs/rc-*.el` modules that `post-init.el` requires. See [`.emacs.d/README.md`](.emacs.d/README.md).

Emacs itself is built with [build-emacs-for-macos](https://github.com/jimeh/build-emacs-for-macos).

Notable packages:

- **Editing:** Evil, evil-collection, evil-surround, evil-mc, undo-fu (+ session), paredit, move-text, aggressive-indent, stripspace
- **Completion / navigation:** Vertico, Consult, Marginalia, Embark, Orderless, Corfu + Cape
- **Languages:** C/C++ (tree-sitter, Eglot/clangd), Zig (Eglot/ZLS), Odin, Python (tree-sitter, uv/ruff, pyvenv), CMake (tree-sitter), Markdown, Org
- **Tooling:** Magit, Apheleia (formatting), YASnippet, pdf-tools, helpful, ghostel

C and C++ use Eglot with `clangd`. Zig uses Eglot with ZLS. Both server commands
match the Neovim configuration. Python uses Flymake with `ruff` instead.
Apheleia owns formatting. Its Zig formatter runs `zig fmt`. `pdf-tools` has its
`epdfinfo` server built, so PDFs open without a build prompt.

Theme: modus-themes. Font: Zenbones Brainy at 17pt. See [Fonts](#fonts).

The mode line is the stock one with the dead weight cut, set in `rc-ui.el`:
no coding-system block, no client-frame `@`, no frame identification, no
percentage through the buffer. Minor mode lighters collapse to a single `…`
that menus the full list on click. This replaced doom-modeline, which drew the
same information and cost a package to do it.

### Emacs daemon

Emacs uses a daemon so `emacsclient` can open frames without starting another
editor process. macOS launchd integration lives in `macos/`. The Linux host
owns its daemon implementation.

```sh
emacsctl status    # is the daemon answering?
emacsctl restart   # after rebuilding Emacs, or editing the unit
emacsctl stop      # stop the job (it will not be resurrected)
emacsctl start     # start it again
emacsctl logs      # tail the daemon's log
```

The `emacsctl` helper is available on macOS. It manages the launchd agent at
`macos/emacs/dev.nathantebbs.emacs.plist`. Linux daemon commands depend on the
host and are not defined here.

**Reaching the daemon:**

| Command | Result |
|---------|--------|
| `emacs` | New GUI frame, terminal free immediately (`emacsclient -c -n`) |
| `e file` | Open a file in an existing frame (`emacsclient -n`) |
| `et` | Frame inside the current terminal (`emacsclient -t`) |
| Emacsclient.app (macOS) | Same as `emacs`, from Spotlight, the Dock or Aerospace |

None of these pass `-a ""`. They report a missing daemon instead of starting
an unmanaged one.

`Emacsclient.app` is macOS-only. Build it explicitly with
`macos/scripts/make-emacsclient-app.sh`. It is not part of root deployment.

`open -a Emacs` (macOS) does **not** use the daemon; it launches a second independent Emacs.

## Terminal

### WezTerm

`wezterm/wezterm.lua` configures the [WezTerm](https://wezterm.org/) terminal emulator.

- Shell: the login shell configured on the host
- Font: Zenbones Brainy, 14pt, with Symbols Nerd Font Mono as fallback for icon glyphs
- Background opacity: 0.92
- Cursor: steady block, which is only what an application that reshapes nothing gets
- Tab bar hidden when only one tab is open
- No confirmation prompt on window close
- Scrollback: 10000 lines, inherited from what `tmux.conf` set back when tmux did this job
- Bell: silent, because Vim rings it for a failed search

**Keyboard:**

The Kitty keyboard protocol applies on both platforms. Option handling and the
bindings below apply only on macOS. Linux keeps WezTerm's default bindings.

- **Left Option is Meta.** macOS otherwise composes `Option+key` into a glyph, so Vim never sees `<M-...>`, Emacs never sees `M-p`, and readline never sees `M-b`. Right Option still composes, which is where accented characters come from
- **Dead keys off.** A dead key waits for a second press to combine with, eating the first when a plain keystroke was meant
- **Kitty keyboard protocol advertised.** Terminals cannot otherwise distinguish `<C-i>` from `<Tab>` or `<C-[>` from `<Esc>`, since they arrive as the same byte. Applications opt in, so anything that does not ask is unaffected

**Key bindings:**

Everything hangs off `Cmd`, the one modifier macOS never forwards to the terminal, so no binding here can shadow a Vim, Emacs or readline key. The cost is `Cmd+h`, which no longer hides the app; `Cmd+m` still does. Use the **left** diamond: Karabiner claims the right one for the window manager.

| Binding | Action |
|---------|--------|
| `Cmd d` | Split right |
| `Cmd D` | Split down |
| `Cmd h/j/k/l` | Focus pane by direction |
| `Cmd Ctrl h/j/k/l` | Resize pane by direction |
| `Cmd z` | Zoom pane |
| `Cmd x` | Close pane |
| `Cmd [` / `Cmd ]` | Previous / next tab |
| `Cmd K` | Clear scrollback |
| `Cmd X` | Copy mode, which navigates with `hjkl`, `v` and `y` |
| `Cmd Shift Space` | Quick select: label every path and URL on screen, jump by typing its letters |
| `Cmd Enter` | Fullscreen |

`Cmd+t`, `Cmd+w` and `Cmd+1`..`9` keep their defaults.

## Window manager

macOS-only. [AeroSpace](https://github.com/nikitabobko/AeroSpace) is an i3-like
tiling window manager. Its config lives at `macos/aerospace.toml`.
[Karabiner-Elements](https://karabiner-elements.pqrs.org/) supplies the modifier
key it binds to. Both applications are listed in `macos/Brewfile`.

### Mod key

The setup is tuned for a HHKB Pro 2, which has no dedicated arrow or super key. Karabiner remaps the **right diamond (Cmd)** key: held, it emits `Cmd+Ctrl+Opt`, a combination no app claims, so it acts as a free i3-style super. Tapped alone it still sends a normal Cmd, and the left diamond is untouched, so native macOS shortcuts keep working. "mod" below means holding the right diamond.

### Key bindings

| Binding | Action |
|---------|--------|
| `mod + 1..9` | Switch to workspace 1-9 |
| `mod + Shift + 1..9` | Move focused window to workspace |
| `mod + h/j/k/l` | Focus left/down/up/right |
| `mod + Shift + h/j/k/l` | Move window in the tree |
| `mod + e` | Toggle split orientation |
| `mod + s` | Accordion layout |
| `mod + f` | Fullscreen |
| `mod + -` / `mod + =` | Shrink / grow window |
| `mod + Tab` | Last workspace |
| `mod + ;` | Service mode (`esc` reload, `r` reset layout, `backspace` close others) |

### Workspace assignments

Apps open on a fixed workspace via `on-window-detected` rules. The scheme is browsers on 1, terminals on 2, editors on 3, comms on 4, AI on 5, docs on 6, media on 7, VMs on 8, games on 9; 7 currently has no rule because nothing on this machine claims it. Only installed apps are listed, so adding an app means adding its block. Finder floats instead of tiling, as does `sdl3-sandbox`, matched on its executable name because an unbundled binary has no bundle id.

Find an app's bundle id with `aerospace list-apps`.

### Karabiner

`macos/karabiner/karabiner.json` holds the Hyper remap plus HHKB-specific
fixes. Karabiner owns this file and rewrites it when settings change in its
GUI. Do not run another window manager alongside AeroSpace.

## bash

macOS ships Bash 3.2. The minimal Brewfile installs a current Bash. Set it as
the login shell with:

```sh
bash macos/scripts/install-bash.sh
```

The script resolves Homebrew's Bash prefix. It adds that path to `/etc/shells`
and runs `chsh`. It is idempotent.

`bash_profile` loads `~/.bashrc` for login shells.

**`bashrc` provides** the parts that are bash's own:

- History worth having: 100k lines, deduplicated, appended per prompt so two
  terminals do not overwrite each other. bash defaults to 500 lines and
  last-window-wins
- `shopt`: `globstar`, `autocd`, `cdspell`, `dirspell`, `checkwinsize`
- `set -o emacs`, matching the editor. It is bash's default, stated rather than inherited
- The prompt: two lines, built per prompt by `__prompt`. See [Prompt](#prompt)

**`config.bash` provides** everything portable, which is the whole of the old
`config.zsh` bar the bun completions:

- Aliases: `emacs` (new GUI frame on the daemon), `e` (open a file in an existing frame), `et` (frame in this terminal), `keys` (fzf alias search), `lsa` (`ls -lah`)
- The oh-my-zsh git aliases, by hand: `g`, `gst`, `gss`, `ga`, `gaa`, `gc`, `gcmsg`, `gcam`, `gc!`, `gca!`, `gco`, `gcb`, `gsw`, `gswc`, `gd`, `gds`, `gb`, `gl`, `gp`, `glo`, `glog`, `grs`, `grst`, `gsta`, `gstp`, `gstl`. Upstream's names, so the muscle memory carries over. The omz plugin defined several hundred; these are the ones in use
- On macOS, `emacsctl` manages the Emacs daemon:
  ```sh
  emacsctl start    # launch daemon
  emacsctl stop     # kill daemon
  emacsctl restart  # kill and relaunch
  emacsctl status   # check if running
  ```
- `$EDITOR` and `$VISUAL`, both `nvim`. Git commit messages and other terminal
  editor calls open in Neovim.
- `ls` colors. macOS gets `CLICOLOR` plus an `LSCOLORS` palette with cyan directories; Linux gets `ls --color=auto`, since GNU ls ignores `LSCOLORS` entirely. oh-my-zsh used to supply an `ls -G` alias, so without this the switch would have lost colors outright
- PATH additions, each guarded so a machine missing the toolchain still gets a working shell: `~/.local/bin`, `~/.cargo/bin`, `~/go/bin`, Emacs.app, and Bun. macOS also prepends Homebrew, because `/etc/paths.d/homebrew` appends and the system Bash 3.2 would otherwise shadow the 5.x the Brewfile installs

`~/.bun/_bun` did not come across. It is a zsh `compdef` file with no bash
equivalent, so bun contributes only its binary now.

## Prompt

Two lines. The first says who and where and what state things are in, the
second is the command. Splitting them is what lets the path be the full path:
it can run as long as it likes without pushing the cursor to the right margin,
and what gets typed always starts at column three.

```
nathan@host [~/source/repos/dotfiles] (main*? ↑2) 1m4s &1 ✗130
$
```

Everything after the directory is conditional and absent when it has nothing
to say. A clean checkout with nothing running is just the first two segments.

| Segment | Shown when | Meaning |
| --- | --- | --- |
| `nathan@host` | always | host turns red under `SSH_CONNECTION` |
| `[~/path]` | always | `\w`, so `$HOME` collapses to `~` |
| `(main)` | inside a repo | branch, or `@` and a short SHA when detached |
| `+` | index differs from HEAD | staged |
| `*` | worktree differs from index | unstaged |
| `?` | files git does not track | untracked |
| `!` | unmerged paths | conflict |
| `↑2 ↓1` | upstream has diverged | commits ahead, commits behind |
| `py:name` | `VIRTUAL_ENV` is set | active virtualenv |
| `1m4s` | last command took 5s or more | wall time |
| `&1` | background jobs exist | how many |
| `✗130` | last command failed | its exit code |

A failing command also turns the `$` red, so the eye finds it without reading.

`__prompt` runs the whole thing out of `PROMPT_COMMAND`, and it is bash
builtins bar two forks: one `git status --porcelain=v2 --branch`, which is
where branch, flags and divergence all come from at once, and one `jobs -p`.
That costs about 13ms in a repo. Outside one it is 0.6ms, because the walk up
the tree looking for `.git` is builtins only and skips the `git` exec when
there is no repo to ask about.

Command duration comes from a `DEBUG` trap recording `SECONDS`. The trap is
armed by the previous prompt and disarms itself on the first command after,
so it times the command and not the minutes spent typing it.

Branch names and the virtualenv name have `\`, `` ` `` and `$` stripped out of
them. Bash expands `PS1` again on every draw, and a directory someone else
named is not trusted input.

The `\[ \]` pairs are not decoration. They tell bash which bytes print as
zero width, and without them it miscounts the line and long commands wrap on
top of themselves.

This replaced starship, which had replaced the oh-my-zsh theme. Neither earned
a dependency: a prompt generator that shells out on every prompt, built from
source and kept current, to draw what a page of bash draws.

## C style

`clang-format` is [git's own `.clang-format`](https://github.com/git/git/blob/master/.clang-format)
vendored, linked to `~/.clang-format`. The short version: 4-space indent and no
tabs, no enforced column limit, braces attached except on a function definition,
pointers bound to the name (`char *p`), return type on the same line as the
name, and no short `if` or loop collapsed onto one line.

Two things diverge from upstream, both marked in the file. git indents with
8-wide hard tabs and this uses four spaces. And git's `ForEachMacros` and
`IfMacros` lists name symbols that only exist inside git.git, so they are
dropped. Re-sync by overwriting the file with that repo's `.clang-format`, then
re-applying those two blocks.

`ColumnLimit: 0` is deliberate. It means clang-format never reflows a line you
broke by hand, which is the reason 80 columns stays a convention here rather
than a rule the formatter enforces.

Emacs formats through apheleia, which shells out to `clang-format` itself.
clang-format walks up from the file being formatted looking for a
`.clang-format`, so this one applies to anything under `$HOME` that does not
ship its own. A project with its own file still wins, which is the point.

## tmux

`tmux/tmux.conf` is linked to `~/.config/tmux` by the deploy script.

- vi mode keys, mouse enabled
- Windows and panes are 1-indexed and renumber on close
- `prefix h` / `prefix v`: split horizontally / vertically in the current pane's directory
- `prefix r`: reload config
- Copy mode: `v` to select, `y` to yank

## Fonts

Configs use **Zenbones Brainy** (WezTerm, Emacs) and **Symbols Nerd Font Mono**
(`fonts/NFM.ttf`, the WezTerm icon fallback). Both are committed under
`fonts/`. Install them explicitly with `util/scripts/install-fonts.sh`.
