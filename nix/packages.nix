{ lib, pkgs, ... }:
let
  commonPackages = with pkgs; [
    aspell
    bashInteractive
    clang-tools
    cmake
    codex
    curl
    fzf
    gh
    git
    gnumake
    gnupg
    go
    gopls
    neovim
    ninja
    pkg-config
    poppler-utils
    pyright
    python3
    ripgrep
    ruff
    tmux
    uv
  ];

  linuxPackages = with pkgs; [
    gcc
  ];
in
{
  home.packages = commonPackages
    ++ lib.optionals pkgs.stdenv.isLinux linuxPackages;
}
