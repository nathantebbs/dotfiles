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

# ── 1. System dependencies ───────────────────────────────────────────────────
echo "--- Dependencies ---"
echo ""

# package name : command to check
DEPS=(
  "neovim:nvim"
  "ripgrep:rg"
  "fzf:fzf"
  "make:make"
  "zsh:zsh"
  "curl:curl"
  "git:git"
)

missing=()
for entry in "${DEPS[@]}"; do
  pkg="${entry%%:*}"
  cmd="${entry##*:}"
  command -v "$cmd" >/dev/null 2>&1 || missing+=("$pkg")
done

if [ ${#missing[@]} -gt 0 ]; then
  warn "Missing packages: ${missing[*]}"
  case "$OSTYPE" in
    linux*)
      sudo apt update
      sudo apt install -y "${missing[@]}"
      ;;
    darwin*)
      if ! command -v brew >/dev/null 2>&1; then
        err "Homebrew not found. Install it from https://brew.sh then re-run this script."
        exit 1
      fi
      brew install "${missing[@]}"
      ;;
    *)
      err "Unsupported OS: $OSTYPE"
      exit 1
      ;;
  esac
else
  info "All dependencies already installed"
fi

echo ""

# Emacs is built from source via github.com/jimeh/build-emacs-for-macos, not
# installed by brew. Installing a Homebrew Emacs alongside it leaves two
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
      warn "Do not 'brew install emacs'; it conflicts with the daemon agent."
    fi
    ;;
  linux*)
    command -v emacs >/dev/null 2>&1 || warn "Emacs not found; install it with your package manager"
    ;;
esac

echo ""

# ── 1b. Brewfile ─────────────────────────────────────────────────────────────
# The dependency list above is the bare minimum needed to get a usable shell.
# The Brewfile is the full package set; it is macOS-only.
case "$OSTYPE" in
  darwin*)
    echo "--- Brewfile ---"
    echo ""
    if [ -f "$DOTFILES_DIR/Brewfile" ]; then
      brew bundle install --file="$DOTFILES_DIR/Brewfile"
      info "Brewfile applied"
    else
      warn "No Brewfile found, skipping"
    fi
    echo ""
    ;;
esac

# ── 2. Fonts ─────────────────────────────────────────────────────────────────
echo "--- Fonts ---"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-fonts.sh"
echo ""

# ── 3. Symlinks ───────────────────────────────────────────────────────────────
echo "--- Symlinks ---"
echo ""
bash "$DOTFILES_DIR/util/scripts/deploy.sh"
echo ""

# ── 4. Oh-my-zsh + config.zsh ────────────────────────────────────────────────
echo "--- zsh ---"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-omz.sh"
echo ""

# ── 5. vim-plug ───────────────────────────────────────────────────────────────
echo "--- vim-plug ---"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-vimplug.sh"
echo ""

# ── 6. starship ──────────────────────────────────────────────────────────────
echo "--- starship ---"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-starship.sh"
echo ""

# ── 7. Default shell ─────────────────────────────────────────────────────────
echo "--- bash ---"
echo ""
bash "$DOTFILES_DIR/util/scripts/install-bash.sh"
echo ""

echo -e "${GREEN}=== Setup complete! ===${NC}"
echo "Open a new terminal to start using bash."
