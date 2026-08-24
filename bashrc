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

# Emacs keys, matching the editor. bash defaults to this; the line is here
# so the keymap is stated rather than inherited.
set -o emacs

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

# Prompt. Cyan user, magenta host, green brackets, blue directory. \[ \] wraps
# every escape so bash counts the printed width and long lines wrap correctly.
PS1='\[\e[1;36m\]\u\[\e[1;33m\]@\[\e[1;35m\]\h \[\e[1;32m\][\[\e[1;34m\]\W\[\e[1;32m\]] \[\e[0m\]\$ '
