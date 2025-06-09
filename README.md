# dotfiles

> [!WARNING]
> This repo is for my personal dotfiles on my arch linux system. Portability is not a consideration. Any documentation is so I can remember later.

## Using this repo

1. Clone the repo into your home directory

```bash
cd ~/
git clone https://github.com/nathantebbs/dotfiles.git
```

2. Run the install script to symlink individual dotfiles

```bash
cd ~/dotfiles
./install.sh
# Optional neovim installation [--nvim|-n]
./install.sh --nvim
```

## How to restore system packages via pkglist.txt

If you are using a linux distrobution that supports installing packages via a text file:

### [Arch Linux](https://archlinux.org/)

```bash
pacman -S - < pkglist.txt
```
