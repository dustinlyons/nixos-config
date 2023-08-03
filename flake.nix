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
      x86_64-linux.bootstrap-nixos = {
        type = "app";
        program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "bootstrap-nixos" ''
                    #!/bin/sh -e

                    GREEN='\033[1;32m'
                    YELLOW='\033[1;33m'
                    RED='\033[1;31m'
                    CLEAR='\033[0m'

                    set -e
                    trap 'echo -e "${RED}Error occurred!${CLEAR}"' ERR

                    if [ -e /etc/NIXOS ]; then
                        echo -e "${GREEN}Running in the NixOS installer environment.${CLEAR}"
                    else
                        echo -e "${RED}Not running in the NixOS installer environment.${CLEAR}"
                    fi

                    echo -e "${GREEN}Setting up directory structure...${CLEAR}"
                    sudo mkdir -p /mnt/etc/nixos && cd /mnt/etc/nixos || { echo -e "${RED}Directory structure setup failed!${CLEAR}"; exit 1; }

                    echo -e "${GREEN}Cleaning previous configuration...${CLEAR}"
                    rm -rf nixos-config-main.zip && rm -rf nixos-config-main && rm -rf nixos-config

                    echo -e "${YELLOW}Downloading nixos-config from Github...${CLEAR}"
                    curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip || { echo -e "${RED}Download failed!${CLEAR}"; exit 1; }
                    echo -e "${GREEN}Download complete.${CLEAR}"

                    unzip nixos-config-main.zip && mv nixos-config-main nixos-config || { echo -e "${RED}Extraction or moving failed!${CLEAR}"; exit 1; }

                    echo -e "${YELLOW}Running disko...${CLEAR}"
                    sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes github:nix-community/disko -- --mode zap_create_mount ./nixos/disk-config.nix || { echo -e "${RED}Disko run failed!${CLEAR}"; exit 1; }
                    echo -e "${GREEN}Partition and filesystem complete.${CLEAR}"

                    echo -e "${YELLOW}Installing NixOS...${CLEAR}"
                    sudo nixos-install --flake .#felix || { echo -e "${RED}NixOS installation failed!${CLEAR}"; exit 1; }
                    echo -e "${GREEN}Installation complete.${CLEAR}"

                    # Prompt the user to reboot
                    read -p "${GREEN}Do you want to reboot now? (y/yes)${CLEAR} " choice
                    case "$choice" in 
                    y|Y|yes|YES ) echo -e "${GREEN}Rebooting...${CLEAR}" && sudo reboot;;
                    * ) echo -e "${YELLOW}Reboot skipped.${CLEAR}";;
                    esac

        '')}/bin/bootstrap-nixos";
      };
    };
  };
}
