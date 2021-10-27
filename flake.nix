{
  description = "Dustin's NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { home-manager, nixpkgs, ... }: {
    nixosConfigurations = {
      felix = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./build/base-configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.dustin = import ./build/home.nix;
          }
        ];
      };
    };
  };
}
