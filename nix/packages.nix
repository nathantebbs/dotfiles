{ pkgs, ... }:
{
  home.packages = with pkgs; [
    curl
    fzf
    git
    gnumake
    neovim
    ripgrep
  ];
}
