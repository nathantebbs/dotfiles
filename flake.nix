{
  description = "Cross-platform development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { home-manager, nixpkgs, ... }:
    let
      mkHome =
        {
          system,
          username,
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          extraSpecialArgs = {
            inherit username;
            homeDirectory =
              if nixpkgs.lib.hasSuffix "-darwin" system then
                "/Users/${username}"
              else
                "/home/${username}";
          };
          modules = [ ./nix/home.nix ];
        };
    in
    {
      homeConfigurations = {
        "nathantebbs@macbook" = mkHome {
          system = "aarch64-darwin";
          username = "nathantebbs";
        };

        "nathantebbs@linux" = mkHome {
          system = "x86_64-linux";
          username = "nathantebbs";
        };

        "vetr0s@linux" = mkHome {
          system = "x86_64-linux";
          username = "vetr0s";
        };
      };
    };
}
