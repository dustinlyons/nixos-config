{
  description = "Dustin's NixOS and MacOS configuration";

  inputs = {
    nixpkgs = {
      url = "github:dustinlyons/nixpkgs/master";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
    };
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, flake-utils, darwin, home-manager, nixpkgs, disko, ... }@inputs: {
    # My Macbook Pro 16"
    darwinConfigurations = {
      "Dustins-MBP" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./darwin
        ];
        inputs = { inherit darwin home-manager nixpkgs; };
      };
     };

    # The PC in my office
    nixosConfigurations = let
      felixDefault = {
        system = "x86_64-linux";
        modules = [
          ./nixos
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.dustin = import ./nixos/home-manager.nix;
          }
        ];
      };

    in {
      felix = nixpkgs.lib.nixosSystem {
        inherit (felixDefault) system;
        modules = felixDefault.modules ++ [
        ];
      };
    };
  };
}
