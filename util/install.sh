#!/bin/bash

set -xe

DOTFILES_DIR="$HOME/dotfiles"
NVIM_CONFIG_REPO="https://github.com/nathantebbs/nvim-config.git"  # Replace with your repo URL
NVIM_CONFIG_DIR="$DOTFILES_DIR/nvim"

INSTALL_NVIM=false

while [ $# -gt 0 ]; do
    case "$1" in
        --nvim|-n)
            INSTALL_NVIM=true
            shift
            ;;
        *)
            echo "ERROR: Unknown opt -> $1"
            echo "USAGE: $0 [--nvim|-n]"
            exit 1
            ;;
    esac
done

# Define dotfiles and their destinations
declare -A DOTFILES=(
    [".bashrc"]="$HOME/.bashrc"
    ["pkglists"]="$HOME/pkglists"
    [".alacritty.toml"]="$HOME/.alacritty.toml"
    ["i3"]="$HOME/.config/i3" # i3wm configuration
)

# If neovim flag is passed we add it to the list of dirs/files we are symlinking
if [ "$INSTALL_NVIM" = true ]; then
    DOTFILES["nvim"]="$HOME/.config/nvim"
fi

# Ensure dotfiles directory exists
if [ ! -d "$DOTFILES_DIR" ]; then
    echo "ERROR: Dotfiles dir -> $DOTFILES_DIR [NOT FOUND]"
    exit 1
fi

# Backup directory
BACKUP_DIR="$HOME/dotfiles_backup/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Function to create symlinks
create_symlink() {
    local src="$1"
    local dest="$2"

    if [ -e "$dest" ] || [ -L "$dest" ]; then
        # Backup existing file or directory
        mv "$dest" "$BACKUP_DIR/"
        echo "BACKED: $dest -> $BACKUP_DIR"
    fi

    ln -sf "$src" "$dest"
    echo "SYMLINK: $dest -> $src"
}

# Function to pull nvim-config repo
pull_nvim_config() {
    if [ -d "$NVIM_CONFIG_DIR" ]; then
        echo "UPDATING: nvim-config in $NVIM_CONFIG_DIR"
        cd "$NVIM_CONFIG_DIR" || exit 1
        git pull origin main  # Adjust branch name if needed
        cd - >/dev/null
    else
        echo "CLONING: nvim-config to $NVIM_CONFIG_DIR"
        git clone "$NVIM_CONFIG_REPO" "$NVIM_CONFIG_DIR"
        rm -rf "$NVIM_CONFIG_DIR/.git"
    fi
}

# Main install
echo "INSTALL: src -> $DOTFILES_DIR..."

# Pull or clone nvim-config repo if neovim flag is set
if [ "$INSTALL_NVIM" = true ]; then
    pull_nvim_config
fi

for file in "${!DOTFILES[@]}"; do
    src="$DOTFILES_DIR/$file"
    dest="${DOTFILES[$file]}"

    dest="${dest/#\$HOME/$HOME}"

    if [ -e "$src" ]; then
        create_symlink "$src" "$dest"
    else
        echo "WARNING: src file/dir -> $src NOT FOUND... skipping"
    fi
done

# Ensure Neovim plugins are installed (optional, for lazy.nvim or similar)
if [ "$INSTALL_NVIM" = true ] && command -v nvim >/dev/null 2>&1; then
    echo "SYNCING: Neovim plugins"
    nvim --headless "+Lazy sync" +qa  # Adjust for your plugin manager (e.g., PackerSync for packer.nvim)
fi

echo "INSTALLATION COMPLETE"
