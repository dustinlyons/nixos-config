{

  description = "Dustin's NixOS and MacOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, darwin, home-manager, nixpkgs, ... }: {

    # My Macbook Pro 16"
    darwinConfigurations = {
      "Dustins-Air" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./build/macos-config.nix
        # ./build/macos-home.nix
        ];
        inputs = { inherit darwin home-manager nixpkgs; };
      };
     };

   # My home-lab Desktop, felix
    nixosConfigurations = {
      felix = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./build/nixos-config.nix
          ./build/hardware/felix.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.dustin = import ./build/nixos-home.nix;
          }
        ];
      };
    };
  };
}
