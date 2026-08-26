#!/usr/bin/env bash
#
# Install a modern bash and make it the login shell. macOS only: it ships
# bash 3.2 at /bin/bash and will not ship a newer one, so the Nix build is
# the real shell there. Linux distros already ship a current bash as
# /bin/bash, so setup.sh does not call this script there.
#
# chsh refuses any shell that is not listed in /etc/shells, so that file has
# to be edited first, and that needs root. Everything below is idempotent.

set -e

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

info() { echo -e "${GREEN}[OK]${NC} $*"; }
warn() { echo -e "${YELLOW}[INFO]${NC} $*"; }
err()  { echo -e "${RED}[ERROR]${NC} $*"; }

BASH_PATH="$HOME/.nix-profile/bin/bash"

[ -x "$BASH_PATH" ] || { err "No bash at $BASH_PATH"; exit 1; }
info "bash: $BASH_PATH ($("$BASH_PATH" -c 'echo $BASH_VERSION'))"

# Match the complete path. A similarly named shell is not equivalent.
if grep -qxF "$BASH_PATH" /etc/shells; then
  info "$BASH_PATH already in /etc/shells"
else
  warn "Adding $BASH_PATH to /etc/shells (needs sudo)"
  echo "$BASH_PATH" | sudo tee -a /etc/shells >/dev/null
  info "Added to /etc/shells"
fi

if [ "$(dscl . -read "/Users/$USER" UserShell 2>/dev/null | awk '{print $2}')" = "$BASH_PATH" ] \
  || [ "$SHELL" = "$BASH_PATH" ]; then
  info "Login shell is already $BASH_PATH"
else
  chsh -s "$BASH_PATH"
  info "Login shell set to $BASH_PATH. Open a new terminal to pick it up."
fi
