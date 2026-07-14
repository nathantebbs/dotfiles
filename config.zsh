alias emacs="emacsclient -c &disown"
alias vim="nvim"
alias keys="alias | fzf"
alias cc="claude"

emacsctl() {
  case "$1" in
    start)
      command emacs --daemon ;;
    stop)
      command emacsclient -e "(kill-emacs)" >/dev/null 2>&1 ;;
    restart)
      command emacsclient -e "(kill-emacs)" >/dev/null 2>&1 || true
      command emacs --daemon ;;
    status)
      command emacsclient -e "(emacs-version)" >/dev/null 2>&1 \
        && echo "Emacs daemon: running" \
        || echo "Emacs daemon: stopped" ;;
    *)
      echo "Usage: emacsctl {start|stop|restart|status}" ;;
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

if [ -d "$HOME/.bun" ]; then
  export BUN_INSTALL="$HOME/.bun"
  export PATH="$BUN_INSTALL/bin:$PATH"
  [ -s "$BUN_INSTALL/_bun" ] && source "$BUN_INSTALL/_bun"
fi

# asdf ships as a brew formula on macOS and a git checkout on Linux.
if command -v brew >/dev/null 2>&1 && [ -f "$(brew --prefix asdf 2>/dev/null)/libexec/asdf.sh" ]; then
  source "$(brew --prefix asdf)/libexec/asdf.sh"
elif [ -f "$HOME/.asdf/asdf.sh" ]; then
  source "$HOME/.asdf/asdf.sh"
fi
