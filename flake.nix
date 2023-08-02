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

  apps.x86_64-linux.bootstrap = {
    type = "app";
    program = "${self.bootstrapCommand}";
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

    # Add this inside your outputs
    bootstrapCommand = nixpkgs.writeShellScriptBin "bootstrap-nixos" ''
      # your commands go here
      sudo nix run ${disko} --extra-experimental-features run-command --extra-experimental-features flakes -- --mode zap_create_mount --flake ${self}#felix

      # Link your configuration
      mkdir -p ~/.local/share/src/
      git clone https://github.com/dustinlyons/nixos-config ~/.local/share/src/nixos-config
      ln -s ~/.local/share/src/nixos-config/flake.nix /mnt/etc/nixos/flake.nix

      # Install and reboot
      sudo nixos-install --flake /mnt/etc/nixos/#felix
      reboot
    '';

  };
}
