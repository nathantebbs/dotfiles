# Account rename recovery

On 2026-08-29 this laptop was brought back in sync after a large dotfiles
reorganization done on the Linux workstation. Most of the damage turned out to
have nothing to do with that reorganization. The macOS account had been renamed
from `nathantebbs` to `vetr0s`, and `/Users/nathantebbs` no longer existed.

This note records the failure mode so the next rename is cheaper to recover.

## What the reorganization broke

Three symlinks. The repository moved platform files under `macos/`, so
`~/.aerospace.toml`, `~/.config/karabiner/karabiner.json` and the launchd plist
still pointed at the old top-level paths. Re-running `setup.sh` fixed all three.

That was the whole of it.

## What the rename broke

Everything that had stored an absolute path into the home directory.

- The login shell. `dscl` still named
  `/Users/nathantebbs/.nix-profile/bin/bash`. That binary was gone. macOS fell
  back to zsh and reported nothing, so `bashrc`, `config.bash` and the prompt
  never loaded again.
- The CLI toolchain. `nvim`, `tmux`, `fzf`, `gh`, `clang-format`, `aspell` and
  `gpg` all came from a Nix profile rooted in the old home. They vanished
  together. Nix was uninstalled and the Brewfile now owns these.
- The agent skill links under `~/.claude`, `~/.agents` and `~/.codex`.
- Scattered single links: `~/.tmux.conf`, `~/.local/bin/claude`,
  `~/.bun/bin/bunx`, `~/.swiftpm`, `~/Library/Fonts/.home-manager-fonts-version`.
- Application configs holding stale paths rather than links: tree-sitter,
  exercism, cabal, podman, qBittorrent.

A deploy script sees none of this. `setup.sh` only knows the paths in its own
manifests, and every one of those was already correct.

## Finding it

Dangling symlinks are the fastest signal. A renamed home turns every absolute
link into a broken one at once.

```sh
find ~ -type l ! -exec test -e {} \; -print
```

Prune the noisy trees before reading the output. Browsers and Electron apps
keep `SingletonLock` and `RunningChromeVersion` links that dangle by design.

Files that hold a path instead of linking to one need a text search:

```sh
grep -rl '/Users/<old-name>' ~/.config ~/Library/LaunchAgents
```

The login shell and `/etc/shells` are neither links nor config files, so check
them by hand:

```sh
dscl . -read /Users/$USER UserShell
cat /etc/shells
```

Failing launchd jobs are worth a look too. Status 78 means the job is loaded
and its program is missing:

```sh
launchctl list | awk 'NR > 1 && $2 != 0 && $2 != "-"'
```

## Lesson

The dotfiles survived the rename because every link the repository owns is
regenerated from a manifest. Nothing outside that manifest was regenerable. A
rename therefore costs one `setup.sh` run plus a manual sweep of everything the
manifests do not cover.
