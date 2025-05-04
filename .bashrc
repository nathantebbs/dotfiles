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

# Color options
export GREP_OPTIONS='--color=auto' GREP_COLOR='1;32'
export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad

PS1='[\u@\h \W]\$ '
