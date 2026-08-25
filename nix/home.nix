{
  homeDirectory,
  username,
  ...
}:
{
  imports = [ ./packages.nix ];

  home = {
    inherit homeDirectory username;
    sessionVariables.PKG_CONFIG_PATH = "$HOME/.nix-profile/lib/pkgconfig";
    stateVersion = "26.05";
  };

  programs.home-manager.enable = true;
}
