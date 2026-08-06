# -n is what frees the terminal. The old "&disown" backgrounded the alias
# itself, which also threw away emacsclient's errors when the daemon was down.
# No -a "": that would start a daemon outside launchd, which emacsctl could
# not then stop. If these say no socket, the answer is emacsctl start.
alias emacs='emacsclient -c -n'
alias e='emacsclient -n'
alias et='emacsclient -t'
alias keys="alias | fzf"
alias lsa="ls -lah"

# The daemon is a launchd job, so it has to be driven through launchctl.
# Killing it directly would just make KeepAlive start it again.
emacsctl() {
  local label="dev.nathantebbs.emacs"
  local target="gui/$(id -u)/$label"
  case "$1" in
    start)
      # stop boots the job out entirely, so start has to load it again;
      # kickstart alone only works on a job launchd already knows about.
      launchctl bootstrap "gui/$(id -u)" \
        "$HOME/Library/LaunchAgents/$label.plist" 2>/dev/null \
        || launchctl kickstart "$target" ;;
    stop)
      launchctl bootout "$target" ;;
    restart)
      launchctl kickstart -k "$target" ;;
    status)
      command emacsclient -e "(emacs-version)" >/dev/null 2>&1 \
        && echo "Emacs daemon: running" \
        || echo "Emacs daemon: stopped" ;;
    logs)
      tail -n 40 "$HOME/Library/Logs/emacs-daemon.err" ;;
    *)
      echo "Usage: emacsctl {start|stop|restart|status|logs}" ;;
  esac
}

# The omz git plugin by hand, cut to what I actually type. The names are
# upstream's, so the muscle memory carries over from zsh.
alias g='git'
alias gst='git status'
alias gss='git status --short'
alias ga='git add'
alias gaa='git add --all'
alias gc='git commit -v'
alias gcmsg='git commit -m'
alias gcam='git commit -a -m'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gsw='git switch'
alias gswc='git switch -c'
alias gd='git diff'
alias gds='git diff --staged'
alias gb='git branch'
alias gl='git pull'
alias gp='git push'
alias glo='git log --oneline --decorate'
alias glog='git log --oneline --decorate --graph'
alias grs='git restore'
alias grst='git restore --staged'
alias gsta='git stash push'
alias gstp='git stash pop'
alias gstl='git stash list'

# omz's amend aliases. `!=` is one of the few things history expansion leaves
# alone, and a trailing `!` is literal, so these are safe in an interactive shell.
alias gc!='git commit -v --amend'
alias gca!='git commit -v -a --amend'

# OS Specific customizations. EDITOR is exported: git, crontab and anything
# else spawning an editor read it from the environment, not from the shell.
#
# Emacs is the editor, reached through the daemon. No -n, so git waits for the
# buffer to be finished with C-x #. -a hands off to nvim when no daemon is
# answering, which is the whole of what vim and nvim are kept for now. Unlike
# -a "", that starts nothing outside launchd for emacsctl to lose track of.
case "$OSTYPE" in
  darwin*)
    export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -t -a /opt/homebrew/bin/nvim'
    export VISUAL="$EDITOR"
    # BSD ls. CLICOLOR rather than an ls -G alias, so anything invoking ls
    # gets the colors. LSCOLORS is 11 foreground/background letter pairs, in
    # the order ls documents.
    export CLICOLOR=1
    export LSCOLORS="GxFxBxdxCxDxdxabagacad"
    ;;
  linux*)
    export EDITOR='/usr/bin/emacsclient -t -a /usr/bin/nvim'
    export VISUAL="$EDITOR"
    # GNU ls ignores LSCOLORS and needs the flag. Its built-in LS_COLORS is
    # already reasonable, so there is nothing to set.
    alias ls='ls --color=auto'
    ;;
esac

# PATH. Each entry is guarded so a machine missing the toolchain doesn't break
# its shell, and so this file stays portable between macOS and Linux.
[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"
[ -d "$HOME/go/bin" ]     && export PATH="$HOME/go/bin:$PATH"
[ -d "/Applications/Emacs.app/Contents/MacOS/bin" ] && export PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"

# ~/.bun/_bun is a zsh compdef file with no bash equivalent, so only the
# binary comes across.
if [ -d "$HOME/.bun" ]; then
  export BUN_INSTALL="$HOME/.bun"
  export PATH="$BUN_INSTALL/bin:$PATH"
fi
