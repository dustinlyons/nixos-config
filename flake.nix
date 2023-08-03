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

  outputs = { self, darwin, home-manager, nixpkgs, disko, ... }@inputs: {

    darwinConfigurations = {
      "Dustins-MBP" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [ ./darwin ];
      };
    };

    nixosConfigurations = let user = "dustin"; in {
      felix = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${user} = import ./nixos/home-manager.nix;
          }
        ];
      };
    };

    apps = {
      x86_64-linux.install = let user = "dustin"; in {
        type = "app";
        program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "install" ''
        # ... rest of the script ...

        # Prompt for user's password
        echo -e "\033[1;33mSetting password for user\033[0m"
        sudo passwd ${user}
        # ... rest of the script ...
        '')}/bin/install";
      };

      aarch64-darwin.install = {
        type = "app";
        program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "install" ''
        @todo: bootstrap darwin environment
        '')}/bin/install";
      };
    };
  };
}
