# macOS terminals start login shells, and a login bash reads this file
# instead of ~/.bashrc.

# Homebrew's PATH, MANPATH, INFOPATH and HOMEBREW_* variables. ~/.zprofile
# did this for zsh and bash never read it, so without this /opt/homebrew/bin
# arrives last via path_helper and /opt/homebrew/sbin not at all.
#
# shellenv is given "bash" explicitly rather than left to guess from $SHELL,
# which is wrong inside anything that did not update it.
for _brew in /opt/homebrew/bin/brew /usr/local/bin/brew /home/linuxbrew/.linuxbrew/bin/brew; do
  if [ -x "$_brew" ]; then
    eval "$("$_brew" shellenv bash)"
    break
  fi
done
unset _brew

# Last, so the guarded prepends in config.bash land in front of Homebrew.
# This is the order zsh had: .zprofile then .zshrc.
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
