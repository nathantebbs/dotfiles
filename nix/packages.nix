{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bashInteractive
    curl
    fzf
    git
    gnumake
    neovim
    ripgrep
  ];
}
