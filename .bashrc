#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Alias Hub
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias cl='peaclock'
alias fp='find ~/Github -type f | fzf'
alias dotfiles=/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME
alias less='less --RAW-CONTROL-CHARS'
alias emacs='emacs & disown'
alias sshmc='ssh root@146.190.151.145'

# Color options
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

PATH=/usr/local/texlive/2025/bin/x86_64-linux:$PATH

PS1='[\[\e[0;31m\]\u\[\e[0m\]@\h:\[\e[34m\]\w\[\e[0m\]]$ '
