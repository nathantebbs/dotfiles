# dotfiles

The purpose of this repository is to store all configuration of any essential developer tools that I use.

## Using this repo

1. Clone the repo into your home directory

```bash
cd ~/
git clone https://github.com/nathantebbs/dotfiles.git
```

2. Symlink

```bash
ln -s ~/dotfiles/nvim/ ~/.config/nvim/
ln -s ~/dotfiles/ghostty/ ~/.config/ghostty/
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.emacs ~/.emacs
```

## Notes On Emacs

- Any file specific bindings (**especially org-mode**) heavily rely on file structure.
if you receive any errors on startup, read the .emacs file to see if you are missing any dependencies or
file structure requirements. If the issue is related to org-mode I suggest changing the files org-mode relies
on [here](https://github.com/nathantebbs/dotfiles/blob/main/.emacs#L157).

## TODO

- [ ] Dependency Checking
- [X] Backup -> Install Script
