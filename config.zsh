# -n is what frees the terminal. The old "&disown" backgrounded the alias
# itself, which also threw away emacsclient's errors when the daemon was down.
# No -a "": that would start a daemon outside launchd, which emacsctl could
# not then stop. If these say no socket, the answer is emacsctl start.
alias emacs='emacsclient -c -n'
alias e='emacsclient -n'
alias et='emacsclient -t'
alias keys="alias | fzf"
alias cc="claude"

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

# OS Specific customizations
case "$OSTYPE" in
  darwin*) EDITOR=/opt/homebrew/bin/nvim ;;
  linux*) EDITOR=/usr/bin/nvim ;;
esac

# PATH. Each entry is guarded so a machine missing the toolchain doesn't break
# its shell, and so this file stays portable between macOS and Linux.
[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"
[ -d "$HOME/go/bin" ]     && export PATH="$HOME/go/bin:$PATH"
[ -d "/Applications/Emacs.app/Contents/MacOS/bin" ] && export PATH="/Applications/Emacs.app/Contents/MacOS/bin:$PATH"

if [ -d "$HOME/.bun" ]; then
  export BUN_INSTALL="$HOME/.bun"
  export PATH="$BUN_INSTALL/bin:$PATH"
  [ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"
fi
