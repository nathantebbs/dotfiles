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
    sdl3
    sdl3.dev
    sdl3-image
    sdl3-image.dev
    sdl3-mixer
    sdl3-mixer.dev
    sdl3-ttf
    tmux
    uv
  ];

  linuxPackages = with pkgs; [
    gcc
    sdl3-shadercross
  ];
in
{
  home.packages = commonPackages
    ++ lib.optionals pkgs.stdenv.isLinux linuxPackages;
}
