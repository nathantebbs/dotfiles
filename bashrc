# Interactive bash.
#
# The split from the zsh setup carries over: this file is the shell-specific
# half (history, shopt, keymap, prompt) and config.bash is the portable half
# (aliases, functions, PATH) that any POSIX shell can source.

# Non-interactive shells want none of this. scp and rsync break if a
# startup file writes to stdout, so bail before anything can.
case $- in
  *i*) ;;
  *) return ;;
esac

# History. bash defaults to 500 lines and last-window-wins on exit; the
# append plus per-prompt flush is what makes two terminals share a history.
HISTSIZE=100000
HISTFILESIZE=100000
HISTCONTROL=ignoreboth:erasedups
HISTIGNORE='ls:ll:cd:pwd:exit:clear:history'
HISTTIMEFORMAT='%F %T '
shopt -s histappend cmdhist
PROMPT_COMMAND='history -a'

shopt -s checkwinsize          # recompute LINES/COLUMNS after a resize
shopt -s globstar              # ** recurses
shopt -s autocd                # a bare directory name cds into it
shopt -s cdspell dirspell      # fix small typos in path arguments

# vi keys, matching vim, evil and tmux copy-mode.
set -o vi

# ~/.bashrc is a symlink into the repo, so BASH_SOURCE points at the link.
# Resolve it and config.bash loads no matter where the repo is cloned.
_rc="${BASH_SOURCE[0]}"
while [ -L "$_rc" ]; do
  _target="$(readlink "$_rc")"
  case "$_target" in
    /*) _rc="$_target" ;;
    *)  _rc="$(dirname "$_rc")/$_target" ;;
  esac
done
DOTFILES_DIR="$(cd -P "$(dirname "$_rc")" && pwd)"
unset _rc _target

. "$DOTFILES_DIR/config.bash"

# starship owns PS1, so it goes last and nothing above may set a prompt.
command -v starship >/dev/null 2>&1 && eval "$(starship init bash)"
