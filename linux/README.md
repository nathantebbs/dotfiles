# Linux configuration

This directory holds Linux behavior that works across distributions. It does
not install packages, name distribution packages, enable services, or assume a
desktop environment.

The root setup script deploys shared configuration first. It then reads
`linux/links.tsv`. That manifest is empty because the repository has no
Linux-only files to link yet.

Run deployment from the repository root:

```sh
bash setup.sh
```

Install the committed fonts when needed:

```sh
bash util/scripts/install-fonts.sh
```

The shell uses `nvim` for `$EDITOR` and `$VISUAL`. It enables GNU `ls` colors.
The host owns package installation and Emacs daemon setup.
