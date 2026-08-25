# macOS terminals start login shells, and a login bash reads this file
# instead of ~/.bashrc.

# Home Manager writes the stable profile paths here. The file is portable
# between macOS and Linux.
if [ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
  . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
fi
export PATH="$HOME/.nix-profile/bin:$PATH"

# Load the interactive shell after the managed environment.
[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
