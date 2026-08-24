# dotfiles


The purpose of this repository is to store all configuration of any essential developer tools that I use.

## Dependencies

`setup.sh` installs these automatically when they are missing, via `apt`
(Linux) or `brew` (macOS). They are the floor: enough for a usable shell and a
working editor.

| Package | Purpose |
|---------|---------|
| `git` | Version control |
| `curl` | Script downloads |
| `neovim` | Backup editor, and what `$EDITOR` falls back to |
| `ripgrep` | Telescope live grep |
| `fzf` | Fuzzy finder (vim + shell) |
| `make` | Build telescope-fzf-native |

`bash` is not in that list because it needs more than an install: the Homebrew
build has to reach `/etc/shells` and `chsh` before it is the login shell.
`util/scripts/install-bash.sh` does all three, and `setup.sh` runs it as its
last stage. See [bash](#bash).

Emacs is not installed by `setup.sh` either. It is built from source with
[build-emacs-for-macos](https://github.com/jimeh/build-emacs-for-macos), and a
Homebrew Emacs alongside it would mean two binaries and two launchd agents
contending for one daemon socket. `setup.sh` checks for `/Applications/Emacs.app`
and prints the build instructions if it is absent.

Everything beyond the floor is the [Brewfile](Brewfile), which is macOS-only
and applied by `setup.sh` with `brew bundle install`. WezTerm, AeroSpace and
Karabiner-Elements are casks in there, so they arrive with it.

## Usage

### Installation

1. Clone the repository

```sh
git clone --depth=1 https://github.com/nathantebbs/dotfiles
cd dotfiles
```

2. Run setup (dependencies, Brewfile, fonts, symlinks, vim-plug, bash)

```sh
bash setup.sh
```

> **macOS:** [Homebrew](https://brew.sh) must be installed before running `setup.sh`.

### Individual scripts

If you only need part of the setup:

```sh
bash util/scripts/deploy.sh               # symlinks only
bash util/scripts/install-fonts.sh        # fonts only
bash util/scripts/install-bash.sh         # Homebrew bash + login shell
bash util/scripts/install-vimplug.sh
bash util/scripts/make-emacsclient-app.sh # Emacsclient.app launcher (macOS)
```

### deploy.sh

Located at `util/scripts/deploy.sh`, this script creates symlinks for all configurations to their expected locations. It safely backs up any existing files before creating symlinks.

| Config | Target |
|--------|--------|
| `.emacs.d/` | `~/.emacs.d` |
| `.vimrc` | `~/.vimrc` |
| `tmux/` | `~/.config/tmux` |
| `nvim/` | `~/.config/nvim` |
| `wezterm/` | `~/.config/wezterm` |
| `bashrc` | `~/.bashrc` |
| `bash_profile` | `~/.bash_profile` |
| `gitconfig` | `~/.gitconfig` |
| `clang-format` | `~/.clang-format` |
| `aerospace.toml` | `~/.aerospace.toml` |
| `karabiner/karabiner.json` | `~/.config/karabiner/karabiner.json` |
| `emacs/dev.nathantebbs.emacs.plist` | `~/Library/LaunchAgents/` (macOS only) |

### install-fonts.sh

Font files (`.ttf`/`.otf`) live in the `fonts/` directory at the repo root. To install them:

```sh
bash util/scripts/install-fonts.sh
```

Installs fonts to `~/.local/share/fonts` (Linux) or `~/Library/Fonts` (macOS) and refreshes the font cache. See [Fonts](#fonts).

## Editors

Emacs is the primary editor and owns `$EDITOR`. Vim and Neovim are the
backups: stable, unchanging, and what `$EDITOR` falls back to when no daemon
is answering.

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

| Server | Languages | Installed by |
|--------|-----------|--------------|
| `clangd` | C, C++, Obj-C, CUDA | Xcode CommandLineTools |
| `gopls` | Go | Brewfile (`go install`) |
| `pyright` | Python | Brewfile |
| `tinymist` | Typst | Brewfile |
| `ols` | Odin | Homebrew |

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

Theme: `modus` (dark background), lightline with the `one` colorscheme.

### Emacs

`.emacs.d/` is built on [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) and uses the built-in `package.el`. No third-party package manager, and no `use-package`: the configuration is plain Elisp split into `configs/rc-*.el` modules that `post-init.el` requires. See [`.emacs.d/README.md`](.emacs.d/README.md).

Emacs itself is built with [build-emacs-for-macos](https://github.com/jimeh/build-emacs-for-macos), not Homebrew.

Notable packages:

- **Editing:** Evil, evil-collection, evil-surround, evil-mc, undo-fu (+ session), paredit, move-text, aggressive-indent, stripspace
- **Completion / navigation:** Vertico, Consult, Marginalia, Embark, Orderless, Corfu + Cape
- **Languages:** Odin, Python (tree-sitter, uv/ruff, pyvenv), CMake (tree-sitter), Markdown, Org
- **Tooling:** Magit, Apheleia (formatting), YASnippet, pdf-tools, helpful, ghostel

No LSP is configured here, unlike Neovim: formatting is apheleia's job, cc-mode indents C and C++ as you type, and Python is linted by Flymake driving `ruff`. `pdf-tools` has its `epdfinfo` server built, so PDFs open without a build prompt.

Theme: modus-themes. Font: Zenbones Brainy at 17pt. See [Fonts](#fonts).

The mode line is the stock one with the dead weight cut, set in `rc-ui.el`:
no coding-system block, no client-frame `@`, no frame identification, no
percentage through the buffer. Minor mode lighters collapse to a single `…`
that menus the full list on click. This replaced doom-modeline, which drew the
same information and cost a package to do it.

### Emacs daemon

Emacs runs as a launchd agent started at login, so `emacsclient` always has a server to attach to.

```sh
emacsctl status    # is the daemon answering?
emacsctl restart   # after rebuilding Emacs, or editing the plist
emacsctl stop      # unload the job (KeepAlive will not resurrect it)
emacsctl start     # load it again
emacsctl logs      # tail the daemon's stderr
```

The agent is `emacs/dev.nathantebbs.emacs.plist`, linked into `~/Library/LaunchAgents` by `deploy.sh`. launchd reads it when the job loads, so changes need an `emacsctl restart`. Logs go to `~/Library/Logs/emacs-daemon.{out,err}`.

**Reaching the daemon:**

| Command | Result |
|---------|--------|
| `emacs` | New GUI frame, terminal free immediately (`emacsclient -c -n`) |
| `e file` | Open a file in an existing frame (`emacsclient -n`) |
| `et` | Frame inside the current terminal (`emacsclient -t`) |
| Emacsclient.app | Same as `emacs`, from Spotlight, the Dock or Aerospace |

None of these pass `-a ""`. That flag would start a daemon outside launchd, which `emacsctl` could then neither stop nor restart. When there is no socket the fix is `emacsctl start`.

`Emacsclient.app` is generated into `/Applications` by `util/scripts/make-emacsclient-app.sh` and rebuilt on every `setup.sh` run. It is a build product, not a symlink, so it is not in this repo. If the daemon is down it kickstarts the launchd job rather than spawning its own.

`open -a Emacs` does **not** use the daemon; it launches a second independent Emacs.

## Terminal

### WezTerm

`wezterm/wezterm.lua` configures the [WezTerm](https://wezterm.org/) terminal emulator.

- Shell: the Homebrew bash, as a login shell. Named explicitly rather than taken from the password database, so a machine where `chsh` has not run still opens bash and not the macOS 3.2
- Font: Zenbones Brainy, 14pt, with Symbols Nerd Font Mono as fallback for icon glyphs
- Background opacity: 0.92
- Cursor: steady block, which is only what an application that reshapes nothing gets
- Tab bar hidden when only one tab is open
- No confirmation prompt on window close
- Scrollback: 10000 lines, inherited from what `tmux.conf` set back when tmux did this job
- Bell: silent, because Vim rings it for a failed search

**Keyboard:**

Three settings exist for the editors running inside the terminal rather than for the terminal itself.

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

macOS-only. [AeroSpace](https://github.com/nikitabobko/AeroSpace) is an i3-like tiling window manager, configured in `aerospace.toml` (linked to `~/.aerospace.toml`). [Karabiner-Elements](https://karabiner-elements.pqrs.org/) supplies the modifier key it binds to. Both install from the Brewfile.

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

Apps open on a fixed workspace via `on-window-detected` rules. The scheme is browsers on 1, terminals on 2, editors on 3, comms on 4, AI on 5, docs on 6, media on 7, VMs on 8, games on 9; 7 currently has no rule because nothing on this machine claims it. Only installed apps are listed, so adding an app means adding its block. Finder floats instead of tiling.

Find an app's bundle id with `aerospace list-apps`.

### Karabiner

`karabiner/karabiner.json` holds the Hyper remap plus HHKB-specific fixes. Karabiner owns this file and rewrites it when settings change in its GUI, so expect it to reformat on edit. Do not run a second window manager such as Rectangle or yabai alongside AeroSpace; they fight over window placement.

## bash

macOS ships bash 3.2 and never will ship a newer one, so the shell here is the
Homebrew build. `chsh` only accepts a shell listed in `/etc/shells`, so that
file has to be edited first:

```sh
bash util/scripts/install-bash.sh
```

That installs `bash` from Homebrew, adds it to `/etc/shells` (needs sudo) and
runs `chsh`. It is idempotent, so re-running it is a no-op.

`bash_profile` exists because macOS terminals open login shells, which read it
instead of `~/.bashrc`. It runs `brew shellenv` and then sources `~/.bashrc`,
in that order, so the guarded prepends in `config.bash` land in front of
Homebrew. This is what `~/.zprofile` used to do for zsh; bash never reads that
file, and without the call `/opt/homebrew/bin` arrives last via `path_helper`
and `/opt/homebrew/sbin` never arrives at all.

**`bashrc` provides** the parts that are bash's own:

- History worth having: 100k lines, deduplicated, appended per prompt so two
  terminals do not overwrite each other. bash defaults to 500 lines and
  last-window-wins
- `shopt`: `globstar`, `autocd`, `cdspell`, `dirspell`, `checkwinsize`
- `set -o emacs`, matching the editor. It is bash's default, stated rather than inherited
- `PS1`: `user@host` in color, then the working directory in brackets

**`config.bash` provides** everything portable, which is the whole of the old
`config.zsh` bar the bun completions:

- Aliases: `emacs` (new GUI frame on the daemon), `e` (open a file in an existing frame), `et` (frame in this terminal), `keys` (fzf alias search), `lsa` (`ls -lah`)
- The oh-my-zsh git aliases, by hand: `g`, `gst`, `gss`, `ga`, `gaa`, `gc`, `gcmsg`, `gcam`, `gc!`, `gca!`, `gco`, `gcb`, `gsw`, `gswc`, `gd`, `gds`, `gb`, `gl`, `gp`, `glo`, `glog`, `grs`, `grst`, `gsta`, `gstp`, `gstl`. Upstream's names, so the muscle memory carries over. The omz plugin defined several hundred; these are the ones in use
- `emacsctl` manages the Emacs daemon:
  ```sh
  emacsctl start    # launch daemon
  emacsctl stop     # kill daemon
  emacsctl restart  # kill and relaunch
  emacsctl status   # check if running
  ```
- `$EDITOR` and `$VISUAL`, both `emacsclient -t -a nvim`. No `-n`, so git waits for the buffer; finish it with `C-x #`, not `C-x C-c`. `-a` runs nvim when no daemon answers, which is what vim and nvim are kept for. The paths are absolute and OS-specific, since a bare environment does not get `config.bash`'s PATH
- `ls` colors. macOS gets `CLICOLOR` plus an `LSCOLORS` palette with cyan directories; Linux gets `ls --color=auto`, since GNU ls ignores `LSCOLORS` entirely. oh-my-zsh used to supply an `ls -G` alias, so without this the switch would have lost colors outright
- PATH additions, each guarded so a machine missing the toolchain still gets a working shell: `~/.local/bin`, `~/.cargo/bin`, `~/go/bin`, Emacs.app, and Bun

`~/.bun/_bun` did not come across. It is a zsh `compdef` file with no bash
equivalent, so bun contributes only its binary now.

## Prompt

Set at the bottom of `bashrc`: cyan user, magenta host, green brackets, blue
directory. One path component, since the full path is what `pwd` is for.

```sh
PS1='\[\e[1;36m\]\u\[\e[1;33m\]@\[\e[1;35m\]\h \[\e[1;32m\][\[\e[1;34m\]\W\[\e[1;32m\]] \[\e[0m\]\$ '
```

```
nathan@host [dotfiles] $
```

The `\[ \]` pairs are not decoration. They tell bash which bytes print as
zero width, and without them it miscounts the line and long commands wrap on
top of themselves.

This replaced starship, which had replaced the oh-my-zsh theme. Neither earned
a dependency: a prompt generator that shells out on every prompt, built from
source and kept current, to draw what one line of bash draws.

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

Configs use **Zenbones Brainy** (WezTerm, Emacs) and **Symbols Nerd Font Mono** (`fonts/NFM.ttf`, the WezTerm icon fallback). Both are committed under `fonts/` and installed by `util/scripts/install-fonts.sh`, which runs as part of `setup.sh`.
