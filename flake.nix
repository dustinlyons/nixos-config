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

  outputs = { self, flake-utils, darwin, home-manager, nixpkgs, disko, ... }@inputs:

  let
    emacsOverlaySha256 = "1xz956v01l3d1nzmcjbn016sn669mfq2wx9asgl85yyvvz7m7f38";
    pkgs = import nixpkgs {};

    bootstrapCommand = pkgs.writeShellScriptBin "bootstrap-nixos" ''
      sudo nix run ${disko} run-command -- --mode zap_create_mount --flake ${self}#felix

      mkdir -p ~/.local/share/src/
      git clone https://github.com/dustinlyons/nixos-config ~/.local/share/src/nixos-config
      ln -s ~/.local/share/src/nixos-config/flake.nix /mnt/etc/nixos/flake.nix

      sudo nixos-install --flake /mnt/etc/nixos/#felix
      reboot
    '';

  in {
    darwinConfigurations = {
      "Dustins-MBP" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./darwin
        ];
        inputs = { inherit darwin home-manager nixpkgs; };
      };
    };

    nixosConfigurations = let
      felixDefault = {
        inputs = { inherit nixpkgs; };
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
          modules = felixDefault.modules;
        };
      };

    apps = {
      x86_64-linux.bootstrap = {
        type = "app";
        program = bootstrapCommand;
      };
    };
  };
}
