export EDITOR="nvim"
export VISUAL="$EDITOR"
alias ls='ls --color=auto'

emacsctl() {
  case "$1" in
    start|stop|restart) systemctl --user "$1" emacs.service ;;
    status) _emacsctl_status ;;
    logs) journalctl --user -u emacs.service -f ;;
    *) echo "Usage: emacsctl {start|stop|restart|status|logs}" ;;
  esac
}
