#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias Hub
alias ls='ls --color=auto'
alias cl='peaclock'
alias fp='find ~/Programming -type f | fzf'
alias dotfiles=/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME
alias less='less --RAW-CONTROL-CHARS'
alias sshmc='ssh root@146.190.151.145'
alias sleep='systemctl sleep'
alias dr='~/.config/emacs/bin/doom sync'
alias dd='~/.config/emacs/bin/doom doctor'
alias emacs='emacsclient -c -a emacs'

# Color options
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

PATH=/usr/local/texlive/2025/bin/x86_64-linux:$PATH

PS1='[\[\e[0;31m\]\u\[\e[0m\]@\h:\[\e[34m\]\w\[\e[0m\]]$ '
