{
  description = "Dustin's Configuration for NixOS and MacOS";

  inputs = {
    nixpkgs.url = "github:dustinlyons/nixpkgs/master"; # @todo: submit packages upstream
    home-manager.url = "github:nix-community/home-manager";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, darwin, home-manager, nixpkgs, disko, ... }@inputs:
  let
    bootstrapScript = nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "bootstrap-nixos" ''
      set -e
      sudo nix run ${disko} run-command -- --mode zap_create_mount --flake ${self}#nixosConfigurations.felix

      mkdir -p ~/.local/share/src/
      if ! git clone https://github.com/dustinlyons/nixos-config ~/.local/share/src/nixos-config; then
        echo "Git clone failed!" >&2
        exit 1
      fi
          
      ln -s ~/.local/share/src/nixos-config/flake.nix /mnt/etc/nixos/flake.nix

      sudo nixos-install --flake /mnt/etc/nixos/#nixosConfigurations.felix
      reboot
    '';
  in
  {
    darwinConfigurations = {
      "Dustins-MBP" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [ ./darwin ];
      };
    };

    nixosConfigurations = {
      felix = {
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
    };

    apps = {
      x86_64-linux.bootstrap-nixos = {
        type = "app";
        program = "${bootstrapScript}/bin/bootstrap-nixos";
      };
    };

    defaultApp.x86_64-linux = bootstrapScript;
  };
}
