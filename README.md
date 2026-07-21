# dotfiles


The purpose of this repository is to store all configuration of any essential developer tools that I use.

## Dependencies

`setup.sh` installs missing dependencies automatically via `apt` (Linux) or `brew` (macOS).

| Package | Purpose |
|---------|---------|
| `git` | Version control |
| `curl` | Script downloads |
| `zsh` | Shell |
| `neovim` | Primary editor (desktop/server) |
| `emacs` | Secondary editor |
| `ripgrep` | Telescope live grep |
| `fzf` | Fuzzy finder (vim + shell) |
| `make` | Build telescope-fzf-native |

WezTerm is not installed by `setup.sh` — install it separately from [wezterm.org](https://wezterm.org/) if you want the terminal config.

## Usage

### Installation

1. Clone the repository

```sh
git clone --depth=1 https://github.com/nathantebbs/dotfiles
cd dotfiles
```

2. Run setup (installs dependencies, fonts, symlinks, zsh)

```sh
bash setup.sh
```

> **macOS:** [Homebrew](https://brew.sh) must be installed before running `setup.sh`.

### Individual scripts

If you only need part of the setup:

```sh
bash util/scripts/deploy.sh        # symlinks only
bash util/scripts/install-fonts.sh # fonts only
bash util/scripts/install-omz.sh   # oh-my-zsh + config.zsh
bash util/scripts/install-vimplug.sh
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
| `clang-format` | `~/.clang-format` |

### install-fonts.sh

Font files (`.ttf`/`.otf`) live in the `fonts/` directory at the repo root. To install them:

```sh
bash util/scripts/install-fonts.sh
```

Installs fonts to `~/.local/share/fonts` (Linux) or `~/Library/Fonts` (macOS) and refreshes the font cache. See [Fonts](#fonts).

## Editors

### Vim

My `.vimrc` uses [vim-plug](https://github.com/junegunn/vim-plug) for plugin management. To install vim-plug on a new machine:

```sh
bash util/scripts/install-vimplug.sh
```

Then from within vim:

```txt
:PlugInstall
```

**Plugins:** fzf + fzf.vim, vim-surround, lightline, vim-polyglot, vim-todo-highlight, undotree

**Key bindings:**

| Binding | Action |
|---------|--------|
| `C-x f` | Files (fzf) |
| `C-x b` | Buffers (fzf) |
| `C-x l` | Lines in buffer (fzf `BLines`) |
| `C-x m` | Maps (fzf) |
| `C-x k` | Delete buffer |
| `C-c C-u` | Toggle undotree |
| `C-c C-e` | Open netrw |
| `C-c C-p i` | `:PlugInstall` |
| `C-c C-p c` | `:PlugClean` |

### Neovim

`nvim/init.lua` is a desktop-only Neovim config using [lazy.nvim](https://github.com/folke/lazy.nvim). lazy.nvim is bootstrapped automatically on first launch — no manual install needed.

**Plugins:** Telescope (+ telescope-fzf-native), vim-surround, lightline, nvim-autopairs, vim-polyglot, todo-comments, presenting.nvim, undotree, vim-colors-modus, typst.vim

**Key bindings:**

| Binding | Action |
|---------|--------|
| `C-x f` | Find files (Telescope) |
| `C-x b` | Buffers (Telescope) |
| `C-x l` | Live grep (Telescope) |
| `C-x m` | Keymaps (Telescope) |
| `C-x k` | Delete buffer |
| `C-c C-u` | Toggle undotree |
| `C-c C-e` | Open netrw |
| `C-c C-p i` | Lazy sync |
| `C-c C-p c` | Lazy clean |

Theme: `modus` (dark background), lightline with the `one` colorscheme.

### Emacs

`.emacs.d/` is built on [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) and uses [Elpaca](https://github.com/progfolio/elpaca) as the package manager.

Notable packages:

- **Editing:** Evil, evil-collection, evil-surround, evil-mc, undo-fu (+ session), paredit, move-text, aggressive-indent, stripspace
- **Completion / navigation:** Vertico, Consult, Marginalia, Embark, Orderless, Corfu + Cape
- **Languages:** Haskell (+ ormolu), Zig, Odin, Python (pyvenv), Markdown, Org (+ ox-reveal for Reveal.js export)
- **Tooling:** Magit, Apheleia (formatting), YASnippet, easysession, pdf-tools, helpful, doom-modeline, ghostel

Theme: modus-themes. Font: Zenbones Brainy at 17pt — see [Fonts](#fonts).

## Terminal

### WezTerm

`wezterm/wezterm.lua` configures the [WezTerm](https://wezterm.org/) terminal emulator.

- Font: Zenbones Brainy, 14pt, with Symbols Nerd Font Mono as fallback for icon glyphs
- Background opacity: 0.92
- Cursor: steady block
- Tab bar hidden when only one tab is open
- No confirmation prompt on window close

## zsh

I use zsh with [oh-my-zsh](https://ohmyz.sh/) for theme and plugins. To automate installation:

```sh
bash util/scripts/install-omz.sh
```

This installs oh-my-zsh and appends a `source` line for `config.zsh` to `~/.zshrc`.

**`config.zsh` provides:**

- Aliases: `emacs` (open a frame via `emacsclient`), `vim` → `nvim`, `keys` (fzf alias search), `cc` → `claude`
- `emacsctl` — manage the Emacs daemon:
  ```sh
  emacsctl start    # launch daemon
  emacsctl stop     # kill daemon
  emacsctl restart  # kill and relaunch
  emacsctl status   # check if running
  ```
- OS-specific `$EDITOR` (nvim path varies between macOS and Linux)
- PATH additions, each guarded so a machine missing the toolchain still gets a working shell: `~/.local/bin`, `~/.cargo/bin`, `~/go/bin`, and Bun
- asdf, sourced from Homebrew on macOS or `~/.asdf` on Linux

## tmux

`tmux/tmux.conf` is linked to `~/.config/tmux` by the deploy script.

- vi mode keys, mouse enabled
- Windows and panes are 1-indexed and renumber on close
- `prefix h` / `prefix v` — split horizontally / vertically in the current pane's directory
- `prefix r` — reload config
- Copy mode: `v` to select, `y` to yank

## Fonts

Configs use **Zenbones Brainy** (WezTerm, Emacs) and **Symbols Nerd Font Mono** (`fonts/NFM.ttf`, the WezTerm icon fallback). Both are committed under `fonts/` and installed by `util/scripts/install-fonts.sh`, which runs as part of `setup.sh`.
