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
  };

  outputs = { self, flake-utils, darwin, home-manager, nixpkgs, ... }@inputs: {
    # My Macbook Pro 16"
    darwinConfigurations = {
      "Dustins-MBP" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./macos
        ];
        inputs = { inherit darwin home-manager nixpkgs; };
      };
     };

    # My NixOS machine
    nixosConfigurations = {
      felix = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos
          ./hardware/felix.nix
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.dustin = import ./nixos/home-manager.nix;
          }
        ];
        specialArgs = {
          inherit inputs;
        };
      };
    };

    apps = {
      repl-darwin = flake-utils.lib.mkApp {
        drv = nixpkgs.legacyPackages.aarch64-darwin.pkgs.writeShellScriptBin "repl-aarch64-darwin" ''
          deps=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$deps
          trap "rm $deps" EXIT
          nix repl $deps
        '';
      };
      repl-linux = flake-utils.lib.mkApp {
        drv = nixpkgs.legacyPackages.x86_64-linux.pkgs.writeShellScriptBin "repl-x86_64-linux" ''
          deps=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$deps
          trap "rm $deps" EXIT
          nix repl $deps
        '';
      };
    };
  };
}
