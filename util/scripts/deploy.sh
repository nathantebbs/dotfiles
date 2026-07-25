#!/usr/bin/env bash

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Function to create symlink safely
create_symlink() {
    local source="$1"
    local target="$2"
    local name="$3"

    # Check if source exists
    if [ ! -e "$source" ]; then
        echo -e "${RED}[ERROR]${NC} Source not found: $source"
        return 1
    fi

    # If target already exists
    if [ -e "$target" ] || [ -L "$target" ]; then
        # If it's already a symlink pointing to the right place
        if [ -L "$target" ] && [ "$(readlink "$target")" = "$source" ]; then
            echo -e "${GREEN}[OK]${NC} $name already linked correctly"
            return 0
        fi

        # Otherwise, backup the existing file/directory
        backup="${target}.backup.$(date +%Y%m%d_%H%M%S)"
        echo -e "${YELLOW}[INFO]${NC} Backing up existing $name to $backup"
        mv "$target" "$backup"
    fi

    # Create the symlink
    ln -s "$source" "$target"
    echo -e "${GREEN}[OK]${NC} Linked $name"
}

echo "Setting up dotfiles symlinks..."
echo ""

create_symlink "$DOTFILES_DIR/.emacs.d" "$HOME/.emacs.d" "emacs"
create_symlink "$DOTFILES_DIR/.vimrc" "$HOME/.vimrc" "vim"

# A fresh macOS account has no ~/.config; create it before linking into it.
mkdir -p "$HOME/.config"
create_symlink "$DOTFILES_DIR/tmux" "$HOME/.config/tmux" "tmux"
create_symlink "$DOTFILES_DIR/nvim" "$HOME/.config/nvim" "nvim"
create_symlink "$DOTFILES_DIR/wezterm" "$HOME/.config/wezterm" "wezterm"
create_symlink "$DOTFILES_DIR/zshrc" "$HOME/.zshrc" "zsh"
create_symlink "$DOTFILES_DIR/gitconfig" "$HOME/.gitconfig" "git"
create_symlink "$DOTFILES_DIR/clang-format" "$HOME/.clang-format" "clang-format"
create_symlink "$DOTFILES_DIR/aerospace.toml" "$HOME/.aerospace.toml" "aerospace"

# Karabiner owns its config dir; link only the file, not the whole directory.
mkdir -p "$HOME/.config/karabiner"
create_symlink "$DOTFILES_DIR/karabiner/karabiner.json" "$HOME/.config/karabiner/karabiner.json" "karabiner"

# The Emacs daemon agent is macOS-only. launchd reads the plist at load time,
# so it has to be reloaded after this changes: emacsctl restart.
case "$OSTYPE" in
  darwin*)
    mkdir -p "$HOME/Library/LaunchAgents" "$HOME/Library/Logs"
    create_symlink "$DOTFILES_DIR/emacs/dev.nathantebbs.emacs.plist" \
      "$HOME/Library/LaunchAgents/dev.nathantebbs.emacs.plist" "emacs daemon"
    ;;
esac

echo ""
echo -e "${GREEN}[OK]${NC} Symlink deployment complete!"
