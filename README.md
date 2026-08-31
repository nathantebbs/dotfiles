# dotfiles

My configuration for the tools I use on macOS and Linux. Shared files live at
the root. Platform integration lives under [`macos/`](macos/) and
[`linux/`](linux/).

The setup script deploys configuration. It does not install Linux packages or
manage Linux services. The macOS side has a small optional
[`Brewfile`](macos/Brewfile), but I keep system setup explicit.

## Deploy

```sh
git clone --depth=1 https://github.com/vetr0s/dotfiles
cd dotfiles
bash setup.sh
```

`setup.sh` detects the platform and links the matching files into place. It
moves an existing target to a timestamped backup. Later runs leave correct
links alone.

Preview the changes first:

```sh
bash setup.sh --dry-run
```

The platform can also be selected for testing:

```sh
bash setup.sh --platform macos --dry-run
bash setup.sh --platform linux --dry-run
```

Package installation and host services stay outside the root setup script.
The platform READMEs define that boundary:

- [macOS setup and services](macos/README.md)
- [Linux setup](linux/README.md)

## What I use

Emacs is my primary GUI editor. I run it as a daemon so `emacsclient` can open
frames without starting another process. Its configuration has a separate
[README](.emacs.d/README.md).

Neovim owns `$EDITOR` and `$VISUAL` for terminal work. Vim remains the small
fallback. Both use `<Space>` as the leader and share the bindings I use most.

Kitty is the terminal. AeroSpace handles tiling on macOS. Karabiner maps the
right diamond key to the modifier AeroSpace uses. Bash is the login shell and
tmux remains available for remote sessions.

The repository also carries my Git configuration, C formatting rules, and the
fonts used by Kitty and Emacs.

## Layout

| Path | Contents |
| --- | --- |
| `.emacs.d/`, `nvim/`, `.vimrc` | Editor configuration |
| `bashrc`, `bash_profile`, `config.bash` | Shell configuration |
| `kitty/`, `tmux/` | Terminal configuration |
| `gitconfig`, `clang-format` | Tool configuration |
| `fonts/` | Editor and terminal font assets |
| `macos/` | Homebrew, launchd, AeroSpace, and Karabiner |
| `linux/` | Distribution-neutral Linux behavior |
| `util/links.tsv` | Shared deployment manifest |

## Optional helpers

These are separate from deployment:

```sh
bash util/scripts/install-fonts.sh
bash util/scripts/install-vimplug.sh
bash macos/scripts/install-bash.sh
bash macos/scripts/make-emacsclient-app.sh
```
