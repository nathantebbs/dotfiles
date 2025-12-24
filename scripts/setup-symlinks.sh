#!/usr/bin/env bash

# Setup symlinks for emacs, vim, and tmux configurations
# This script creates symlinks from the home directory to the dotfiles repository

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

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
        echo -e "${RED}✗${NC} Source not found: $source"
        return 1
    fi

    # If target already exists
    if [ -e "$target" ] || [ -L "$target" ]; then
        # If it's already a symlink pointing to the right place
        if [ -L "$target" ] && [ "$(readlink "$target")" = "$source" ]; then
            echo -e "${GREEN}✓${NC} $name already linked correctly"
            return 0
        fi

        # Otherwise, backup the existing file/directory
        backup="${target}.backup.$(date +%Y%m%d_%H%M%S)"
        echo -e "${YELLOW}!${NC} Backing up existing $name to $backup"
        mv "$target" "$backup"
    fi

    # Create the symlink
    ln -s "$source" "$target"
    echo -e "${GREEN}✓${NC} Linked $name"
}

echo "Setting up dotfiles symlinks..."
echo ""

# Emacs configuration
create_symlink "$DOTFILES_DIR/external/.emacs.d" "$HOME/.emacs.d" "emacs"

# Vim configuration
create_symlink "$DOTFILES_DIR/.vimrc" "$HOME/.vimrc" "vim"

# Tmux configuration
create_symlink "$DOTFILES_DIR/tmux/tmux.conf" "$HOME/.tmux.conf" "tmux"

echo ""
echo -e "${GREEN}✓${NC} Symlink setup complete!"
