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
ln -s ~/dotfiles/.emacs ~/.emacs
```

> [!NOTE]
> before symlinking files ensure that you have backed up any existing configurations already
> present. For specific configuration(s) see below.

## Emacs

- Any file specific bindings (**especially org-mode**) heavily rely on file structure.
if you receive any errors on startup, read the .emacs file to see if you are missing any dependencies or
file structure requirements. If the issue is related to org-mode I suggest changing the files org-mode relies
on.

> [!NOTE]
> By default, my configuration expects ~/org to exist. I usually clone a backup of my private org repository.
> Alternatively, you could remove this dependency within the .emacs file.

### MacOS

- I regularly switch between my MacBook Pro and Linux desktop. When using emacs on MacOS I highly recommend using [emacs-plus@31](https://github.com/d12frosted/homebrew-emacs-plus).
This installs a lot of usefull prerequisites and gives the user tons of install options. I have only experienced errors with my configuration when using emacs-app.

### Usage

Backup/Remove old emacs configuration file(s)

```bash
mv ~/.config/emacs ~/.config/emacs.back
mv ~/.config/doom ~/.config/doom.back
mv ~/.emacs.d ~/.emacs.d.back
```
