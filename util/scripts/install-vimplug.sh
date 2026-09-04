#!/usr/bin/env bash

set -e

version="0.14.0"
curl -fLo "$HOME/.vim/autoload/plug.vim" --create-dirs \
  "https://raw.githubusercontent.com/junegunn/vim-plug/$version/plug.vim"
