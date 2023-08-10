{
  description = "Dustin's Configuration for NixOS and MacOS";

  inputs = {
    nixpkgs.url = "github:dustinlyons/nixpkgs/master";
    agenix.url = "github:ryantm/agenix";
    home-manager.url = "github:nix-community/home-manager";
    utils.url = "github:numtide/flake-utils";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    secrets = {
      url = "git+ssh://git@github.com/dustinlyons/nix-secrets.git";
      flake = false;
    };
  };

  outputs = { self, darwin, home-manager, nixpkgs, disko, agenix, secrets, utils } @inputs:
    let
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      devShell = system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        default = with pkgs; mkShell {
          nativeBuildInputs = with pkgs; [ bashInteractive git age age-plugin-yubikey ];
          shellHook = with pkgs; ''
            export EDITOR=vim
          '';
        };
      };
    in
    {
      devShells = forAllSystems devShell;

      darwinConfigurations = {
        "Dustins-MBP" = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = inputs;
          modules = [
            ./darwin
          ];
        };
      };

      nixosConfigurations = let user = "dustin"; in {
        felix = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
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
        x86_64-linux.install = let
          red = "\033[1;31m";
          green = "\033[1;32m";
          yellow = "\033[1;33m";
          reset = "\033[0m";
        in {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "install" ''
            #!/usr/bin/env bash
            set -e
            trap 'echo -e "${red}Error occurred!${reset}"' ERR

            if [ -e /etc/NIXOS ]; then
                echo -e "${green}Running in the NixOS installer environment.${reset}"
            else
                echo -e "${red}Not running in the NixOS installer environment.${reset}"
            fi

            echo -e "${green}Cleaning previous configuration...${reset}"
            rm -rf nixos-config-main.zip && rm -rf nixos-config-main && rm -rf nixos-config

            echo -e "${yellow}Downloading nixos-config from Github...${reset}"
            curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip || { echo -e "${red}Download failed!${reset}"; exit 1; }
            echo -e "${green}Download complete.${reset}"

            unzip nixos-config-main.zip && mv nixos-config-main nixos-config || { echo -e "${red}Extraction or moving failed!${reset}"; exit 1; }

            echo -e "${yellow}Running disko...${reset}"
            sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes \
              github:nix-community/disko -- --mode zap_create_mount ./nixos-config/nixos/disk-config.nix || { echo -e "${red}Disko run failed!${reset}"; exit 1; }

            echo -e "${green}Partition and filesystem complete.${reset}"

            echo -e "${green}Setting up directory structure...${reset}"
            sudo mkdir -p /mnt/etc/nixos || { echo -e "${red}Directory structure setup failed!${reset}"; exit 1; }

            sudo cp -r nixos-config/* /mnt/etc/nixos && cd /mnt/etc/nixos || { echo -e "${red}Copying nixos-config failed!${reset}"; exit 1; }

            echo -e "${yellow}Installing NixOS...${reset}"
            sudo nixos-install --flake .#felix || { echo -e "${red}NixOS installation failed!${reset}"; exit 1; }
            echo -e "${yellow}Setting group permissions...${reset}"
            sudo chmod -R 775 /mnt/etc/nixos || { echo -e "${red}Failed to set group permissions on /mnt/etc/nixos!${reset}"; exit 1; }
            echo -e "${green}Installation complete.${reset}"

            # Prompt the user to reboot
            read -p "Do you want to reboot now? (y/yes) " choice
            case "$choice" in
            y|Y|yes|YES ) echo -e "${green}Rebooting...${reset}" && sudo reboot;;
            * ) echo -e "${yellow}Reboot skipped.${reset}";;
            esac

            '')}/bin/install";
        };
      };
    };
}
