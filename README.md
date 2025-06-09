# dotfiles

> [!WARNING]
> This repo is for my personal dotfiles on my arch linux system. Portability is not a consideration. Any documentation is so I can remember later.

## Using this repo

- Clone the repo into your home directory

```bash
cd ~/
git clone https://github.com/nathantebbs/dotfiles.git
```

- Inside the new dotfiles directory. Use the pkglists directory install the official pkglist backup

```bash
cd ~/dotfiles
pacman -S - < pkglists/pkglist.txt
```

> [!NOTE] See the pkglists/pkglist-aur.txt for the aur packages I use. It is importatnt to use the official pkglist.txt for initial installations on new systems.

- Run the install script to symlink individual dotfiles

```bash
cd ~/dotfiles
./install.sh
# Optional neovim installation [--nvim|-n]
./install.sh --nvim
```

## Supported Linux Distros

If you are using a linux distrobution that supports installing packages via a text file:

### [Arch Linux](https://archlinux.org/)

```bash
pacman -S - < pkglist.txt
```
