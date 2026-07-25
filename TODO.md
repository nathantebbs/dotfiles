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
- [x] `.gitignore` no longer ignores `*.json`; `nvim/lazy-lock.json` is tracked and
      the Emacs tree ignores only Claude's `settings.local.json`
- [x] `deploy.sh` creates `~/.config` before symlinking into it
- [x] Emacs: Elpaca -> `package.el`, and `use-package` dropped for plain Elisp
      modules under `.emacs.d/configs/`
- [x] Emacs: update vendored minimal-emacs.d 1.3.1 -> 1.5.1
- [x] Emacs: run the daemon under launchd instead of starting it by hand, and
      remove the two orphaned Homebrew agents that failed on every login

## Open

- [ ] Neovim: no LSP config yet (bare `TODO` above the language support block in
      `nvim/init.lua`)
- [ ] `pdf-tools` has no built `epdfinfo` on this machine, so opening a PDF will
      prompt to build the server
