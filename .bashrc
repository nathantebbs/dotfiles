# All the default Omarchy aliases and functions
# (don't mess with these directly, just overwrite them here!)
source ~/.local/share/omarchy/default/bash/rc

# ALIAS(s):

# Tailscale / VPN
alias tup='sudo tailscale up'
alias tdown='sudo tailscale down'
alias ts='tailscale status'
alias teup='sudo tailscale set --exit-node=$(tailscale exit-node suggest | sed -n "s/^Suggested exit node: \(.*\)\.$/\1/p")'
alias tedown='sudo tailscale set --exit-node='


# CUSTOM FUNCTIONS:

# Dotfiles
dsync() {
  local dirs=("$HOME/org" "$HOME/dotfiles")
  local cwd
  cwd=$(pwd)

  for dir in "${dirs[@]}"; do
    if [ -d "$dir/.git" ]; then
      echo "📂 Syncing $dir..."
      cd "$dir" || continue
      git pull --rebase --autostash
      echo
    else
      echo "⚠️  $dir is not a git repository, skipping."
    fi
  done

  cd "$cwd" || return
  echo "✅ Done syncing all repositories."
}

# MISC:

# nvm
source /usr/share/nvm/init-nvm.sh
