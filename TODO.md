# TODO

## Done

- [x] Fix scripts: less clutter in root (moved to `util/scripts/`)
- [x] Alacritty -> Kitty -> WezTerm
- [x] i3 -> Hyprland, then dropped the Wayland stack entirely
- [x] Automate font installation (`util/scripts/install-fonts.sh` — fonts live in `fonts/`)
- [x] Automate omz `.zshrc` configuration (absolute path in `install-omz.sh`)
- [x] Emacs: Helm -> Vertico/Consult/Marginalia/Embark
- [x] Drop `hypr/`, `waybar/`, `rofi/` — no longer running a Linux desktop
- [x] Remove desktop symlinks from `deploy.sh` (they pointed at the deleted dirs and
      aborted the script on bare-metal Linux)
- [x] Remove stale `external/.emacs.d` submodule declaration from `.gitmodules`
- [x] Update README to match the current configs

## Open

- [ ] `.gitignore` ignores `*.json` repo-wide, which silently excludes
      `nvim/lazy-lock.json` — decide whether to commit the lockfile and narrow the
      pattern to the Emacs files it was meant for
- [ ] `deploy.sh` assumes `~/.config` exists; `mkdir -p` it before symlinking
- [ ] Neovim: no LSP config yet (bare `TODO` above the language support block in
      `nvim/init.lua`)
