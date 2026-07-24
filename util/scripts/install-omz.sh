#!/usr/bin/env bash
set -e

command -v zsh >/dev/null 2>&1 || { echo "zsh not installed"; exit 1; }

# Install oh-my-zsh without taking over the session
export RUNZSH=no
export CHSH=no
export KEEP_ZSHRC=yes

sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# The tracked zshrc sources config.zsh itself, resolving its own path, so there
# is nothing to append. KEEP_ZSHRC=yes leaves the symlinked ~/.zshrc untouched.
echo "oh-my-zsh installed"
