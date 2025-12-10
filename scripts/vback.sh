# File: vback.sh
# Author: Nathan Tebbs
# Purpose: Backup all packages installed to my Void Linux distro.
# Edited: 2025-12-10

set -xe

# Date format safe for file paths
TODAY_DATE=$(date +"%Y-%m-%d_%H-%M-%S")

BACKUP_DIR="$HOME/dotfiles/backups/void-backup-$TODAY_DATE"

mkdir -p "$BACKUP_DIR"

xbps-query -m > "$BACKUP_DIR/manual.txt"
xbps-query -l | awk '{print $2}' > "$BACKUP_DIR/all.txt"
xbps-query -O > "$BACKUP_DIR/orphans.txt"
