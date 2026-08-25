#!/usr/bin/env bash

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

info() { echo -e "${GREEN}[OK]${NC} $*"; }
warn() { echo -e "${YELLOW}[INFO]${NC} $*"; }
err()  { echo -e "${RED}[ERROR]${NC} $*"; }

echo "=== dotfiles setup ==="
echo ""

# 1. Nix packages
echo "Nix packages"
echo ""

command -v nix >/dev/null 2>&1 || {
  err "Nix not found. Install it from https://nixos.org/download/"
  exit 1
}

case "$OSTYPE" in
  darwin*) profile="nathantebbs@macbook" ;;
  linux*) profile="nathantebbs@linux" ;;
  *)
    err "Unsupported OS: $OSTYPE"
    exit 1
    ;;
esac

flake_attr="homeConfigurations.\"$profile\".activationPackage"
activation_package="$(
  nix --extra-experimental-features 'nix-command flakes' build \
    "$DOTFILES_DIR#$flake_attr" --no-link --print-out-paths
)"
"$activation_package/activate"
info "Home Manager profile applied: $profile"
unset activation_package flake_attr profile

echo ""

# Emacs is built from source via github.com/jimeh/build-emacs-for-macos, not
# installed by Nix. Installing another Emacs alongside it leaves two
# binaries and two launchd agents fighting over the same daemon socket.
case "$OSTYPE" in
  darwin*)
    if [ -d "/Applications/Emacs.app" ]; then
      info "Emacs.app found"
      # Spotlight/Dock launcher for the daemon. Regenerated every run so it
      # tracks the script; it is a build product, not a symlink.
      bash "$DOTFILES_DIR/util/scripts/make-emacsclient-app.sh"
    else
      warn "Emacs.app not found. Build it with:"
      warn "  https://github.com/jimeh/build-emacs-for-macos"
      warn "Do not install another Emacs; it conflicts with the daemon agent."
    fi
    ;;
  linux*)
    command -v emacs >/dev/null 2>&1 || warn "Emacs not found; install it with your package manager"
    ;;
esac

echo ""

# 2. Fonts
echo "Fonts"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-fonts.sh"
echo ""

# 3. Symlinks
echo "Symlinks"
echo ""
bash "$DOTFILES_DIR/util/scripts/deploy.sh"
echo ""

# 4. vim-plug
echo "vim-plug"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-vimplug.sh"
echo ""

# 5. Default shell
echo "bash"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-bash.sh"
echo ""

echo -e "${GREEN}=== Setup complete! ===${NC}"
echo "Open a new terminal to start using bash."
