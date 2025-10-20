# All the default Omarchy aliases and functions
# (don't mess with these directly, just overwrite them here!)
source ~/.local/share/omarchy/default/bash/rc

# Add your own exports, aliases, and functions here.
#
# Make an alias for invoking commands you use constantly
# alias p='python'
alias doom='./.config/emacs/bin/doom'
alias ll='ls -la'
alias update='sudo pacman -Syu && yay'

# Tailscale / VPN
alias tup='sudo tailscale up'
alias tdown='sudo tailscale down'
alias ts='tailscale status'
alias teup='sudo tailscale set --exit-node=$(tailscale exit-node suggest | sed -n "s/^Suggested exit node: \(.*\)\.$/\1/p")'
alias tedown='sudo tailscale set --exit-node='
