#!/usr/bin/env bash
#
# Build starship from source into ~/source/third_party/starship.
#
# Not the Homebrew bottle and not the curl installer: this machine already
# keeps hand-built tools under third_party, and building here means the
# prompt tracks upstream main rather than a packaged release. The binary
# lands in ~/.local/bin, which config.bash already puts on PATH.

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

info() { echo -e "${GREEN}[OK]${NC} $*"; }
warn() { echo -e "${YELLOW}[INFO]${NC} $*"; }
err()  { echo -e "${RED}[ERROR]${NC} $*"; }

SRC_DIR="$HOME/source/third_party/starship"
PREFIX="$HOME/.local"

command -v cargo >/dev/null 2>&1 || { err "cargo not found; install rust first"; exit 1; }

if [ -d "$SRC_DIR/.git" ]; then
  # Track whatever branch the clone is on rather than hardcoding a name.
  # The clone is shallow, so origin/HEAD is not set and FETCH_HEAD is.
  BRANCH="$(git -C "$SRC_DIR" symbolic-ref --short HEAD)"
  warn "Updating $SRC_DIR ($BRANCH)"
  git -C "$SRC_DIR" fetch --depth=1 origin "$BRANCH"
  git -C "$SRC_DIR" reset --hard FETCH_HEAD
else
  mkdir -p "$(dirname "$SRC_DIR")"
  git clone --depth=1 https://github.com/starship/starship.git "$SRC_DIR"
fi

# --locked builds the dependency set upstream tested, so a fresh clone does
# not resolve to a semver-compatible crate that happens to be broken today.
cargo install --path "$SRC_DIR" --root "$PREFIX" --locked

info "$("$PREFIX/bin/starship" --version | head -1) at $PREFIX/bin/starship"
