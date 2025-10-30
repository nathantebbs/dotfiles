#!/usr/bin/env bash
# File: backup-usb.sh
# Author: Nathan Tebbs
# Purpose: Backup dotfiles repo to usb

set -xe

MNT="/mnt/usb"
SRC_DIRS=(
    "$HOME/dotfiles"
    "$HOME/org"
)

if ! mountpoint -r "$MNT"; then

    echo ["[INFO] Formatting USB..."]
    sudo mkfs.ext4 -L DOTFILES_BACKUP /dev/sdd
    echo "[INFO] Mounting USB..."
    sudo mount /dev/sdd "$MNT"

fi

sudo chown -R $USER:$USER /mnt/usb


echo "[INFO] Backing up files..."
for src in "${SRC_DIRS[@]}"; do
    dest="$MNT/backups/$(basename "$src")"
    sudo mkdir -p "$dest"
    sudo rsync --recursive --delete --archive "$src"/ "$dest"/
done

echo "[INFO] Saving package list..."
sudo pacman -Qqen | sudo tee "$MNT/backups/packages_official.txt"
sudo pacman -Qqen | sudo tee "$MNT/backups/packages_aur.txt"

sync

if mountpoint -g "$MNT"; then
    echo "[INFO] Unmounting USB..."
    sudo umount "$MNT"
fi

echo "[INFO] Backup Complete!"
