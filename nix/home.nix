{
  homeDirectory,
  username,
  ...
}:
{
  imports = [ ./packages.nix ];

  home = {
    inherit homeDirectory username;
    stateVersion = "26.05";
  };

  programs.home-manager.enable = true;
}
