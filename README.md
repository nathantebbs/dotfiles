# dotfiles

> [!WARNING]
> This repo is for my personal dotfiles on my arch linux system. Portability is not a consideration. Any documentation is so I can remember later.

## How to restore system packages via pkglist.txt

If you are using a linux distrobution that supports installing packages via a text file:

### [Arch Linux](https://archlinux.org/)

```bash
$ sudo pacman -S - < pkglist.txt
```
