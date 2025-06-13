#! /bin/bash

set -xe

# Ensure pkglists dir
mkdir -p ./pkglists

# Backup official packages
pacman -Qe | awk '{print $1}' > ./pkglists/pkglist.txt

# Backup AUR Packages
pacman -Qm > ./pkglists/aur-pkglist.txt

# Complete list of all system packages AUR & Official
pacman -Q > ./pkglists/full-pkglist.txt
