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
ln -s ~/dotfiles/nvim/ ~/.config/
ln -s ~/dotfiles/alacritty/ ~/.config/
ln -s ~/dotfiles/.bashrc ~/.bashrc
ln -s ~/dotfiles/.emacs ~/.emacs
```

- Note: before symlinking these files ensure that you have backed up any existing configurations already
present. For specific configuration(s) see below.

## Emacs

- Any file specific bindings (**especially org-mode**) heavily rely on file structure.
if you receive any errors on startup, read the .emacs file to see if you are missing any dependencies or
file structure requirements. If the issue is related to org-mode I suggest changing the files org-mode relies
on.

>[!NOTE]
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

## Alacritty

- Omarchy utilizes it's own custom configuration requirements so I have to split the alacritty.toml file
into seperate directories. The structure is self explanitory. For future reference, any configuration files
that have platform specific dependencies or configurations then please refer to the config/ directory.

```bash
# Backup files
mv ~/.config/alacritty ~/.config/alacritty.back

# Omarchy
ln -s ~/dotfiles/config/omarchy/alacritty ~/.config/

# Macos
ln -s ~/dotfiles/config/macos/alacritty ~/.config/
```
