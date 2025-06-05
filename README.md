# dotfiles

> [!WARNING]
> This repo is for my personal dotfiles on my arch linux system. Portability is not a consideration. Any documentation is so I can remember later.

## How to add a new dotfile dependency

1. Include file in .gitignore for file
```
*
!.bashrc
!.emacs
!pkglist.txt
...
!<file>

```
2. Add file to start tracking it's changes
```bash
$ dotfiles add <file>
```

This is important as to not push important information to GitHub.

> See .bashrc for dotfiles alias!

## How to restore system packages via pkglist.txt

If you are using [Arch Linux](https://archlinux.org/), or some other linux distrobution that supports downloading from dependencies from a file.

```bash
$ sudo pacman -S - < pkglist.txt
```
