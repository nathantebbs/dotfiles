# dotfiles

> [!WARNING]
> This repo is for my personal dotfiles on my arch linux system. Portability is not a consideration. Any documentation is so I can remember later.

## Using this repo

- Clone the repo into your home directory

```bash
cd ~/
git clone https://github.com/nathantebbs/dotfiles.git
```

- Inside the new dotfiles directory. Use the pkglists directory install the official pkglist backup

```bash
cd ~/dotfiles
pacman -S - < pkglists/pkglist.txt
```

> [!NOTE]
> See the pkglists/pkglist-aur.txt for the aur packages I use. It is importatnt to use the official pkglist.txt for initial installations on new systems.

- Run the install script to symlink individual dotfiles

```bash
cd ~/dotfiles
./util/install.sh
# Optional neovim installation [--nvim|-n]
./util/install.sh --nvim
```

## Util

### Installation Script

<details>
<summary>The following script will take a specified set of files present within this repository and symlink them to the approraite configuration
directories on your computer.</summary>

```bash
#!/bin/bash

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
    ["pkglist.txt"]="$HOME/pkglist.txt"
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
```
</details>

### System Backups

Within the `util` directory in the root of this repository you will find the backup script for arch linux which seperates your system packages into
3 lists. pklist.txt are official archlinux packages, pkglist-aur.txt are [AUR](https://aur.archlinux.org/) packages, and pkglist-full.txt combines both
lists into one document

## Supported Linux Distros

If you are using a linux distrobution that supports installing packages via a text file:

### [Arch Linux](https://archlinux.org/)

```bash
pacman -S - < pkglist.txt
```
