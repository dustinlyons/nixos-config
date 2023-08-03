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

    nixosConfigurations = {
      felix = nixpkgs.lib.nixosSystem {
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
      x86_64-linux.bootstrap = {
        type = "app";
        program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "bootstrap" ''

        #!/usr/bin/env bash
        set -e
        trap 'echo -e "\033[1;31mError occurred!\033[0m"' ERR

        if [ -e /etc/NIXOS ]; then
            echo -e "\033[1;32mRunning in the NixOS installer environment.\033[0m"
        else
            echo -e "\033[1;31mNot running in the NixOS installer environment.\033[0m"
        fi

        echo -e "\033[1;32mSetting up directory structure...\033[0m"
        sudo mkdir -p /mnt/etc/nixos && cd /mnt/etc/nixos || { echo -e "\033[1;31mDirectory structure setup failed!\033[0m"; exit 1; }

        echo -e "\033[1;32mCleaning previous configuration...\033[0m"
        rm -rf nixos-config-main.zip && rm -rf nixos-config-main && rm -rf nixos-config

        echo -e "\033[1;33mDownloading nixos-config from Github...\033[0m"
        curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip || { echo -e "\033[1;31mDownload failed!\033[0m"; exit 1; }
        echo -e "\033[1;32mDownload complete.\033[0m"

        unzip nixos-config-main.zip && mv nixos-config-main nixos-config || { echo -e "\033[1;31mExtraction or moving failed!\033[0m"; exit 1; }

        echo -e "\033[1;33mRunning disko...\033[0m"
        sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes github:nix-community/disko -- --mode zap_create_mount ./nixos/disk-config.nix || { echo -e "\033[1;31mDisko run failed!\033[0m"; exit 1; }
        echo -e "\033[1;32mPartition and filesystem complete.\033[0m"

        echo -e "\033[1;33mInstalling NixOS...\033[0m"
        sudo nixos-install --flake .#felix || { echo -e "\033[1;31mNixOS installation failed!\033[0m"; exit 1; }
        echo -e "\033[1;32mInstallation complete.\033[0m"

        # Prompt the user to reboot
        read -p "\033[1;32mDo you want to reboot now? (y/yes)\033[0m " choice
        case "$choice" in 
        y|Y|yes|YES ) echo -e "\033[1;32mRebooting...\033[0m" && sudo reboot;;
        * ) echo -e "\033[1;33mReboot skipped.\033[0m";;
        esac

        '')}/bin/bootstrap-nixos";
      };
    };
  };
}
