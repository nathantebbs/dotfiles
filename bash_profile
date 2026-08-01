# macOS terminals start login shells, and a login bash reads this file
# instead of ~/.bashrc. Everything lives in ~/.bashrc; this only pulls it in.
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
