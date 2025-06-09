#! /bin/bash

DOTFILES_DIR="$HOME/dotfiles"

declare -A DOTFILES=(
    ["bashrc"]=".$HOME/.bashrc"
    ["pkglist"]=".$HOME/pkglist.txt"
)

# Ensure dotfiles directory
if [ ! -d "DOTFILES_DIR" ]; then
    echo "ERROR: Dotfiles dir -> $DOTFILES_DIR [NOT FOUND]"
    exit 1
fi

# Backup directory
BACKUP_DIR="$HOME/.dotfiles_backup/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Function to create symlinks
create_symlink() {
    local src="$1"
    local dest="$2"

    if [ -e "$dest" ] || [ -L "$dest" ]; then
        # Backup existing file
        mv "$dest" "$BACKUP_DIR/"
        echo "BACKED: $dest -> $BACKUP_DIR"
    fi

    ln -sf "$src" "$dest"
    echo "SYMLINK: $dest -> $src"
}

# Main install
echo "INSTALL: src -> $DOTFILES_DIR..."

for file in "${!DOTFILES[@]}"; do
    src="$DOTFILES_DIR/$file"
    dest="${DOTFILES[$file]}"

    dest="${dest/#\$HOME/$HOME}"

    if [ -e "$src" ]; then
        create_symlink "$src" "$dest"
    else
        echo "WARNING: src file -> $src NOT FOUND... skipping"
    fi
done
