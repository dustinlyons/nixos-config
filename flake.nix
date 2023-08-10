{
  description = "Dustin's Configuration for NixOS and MacOS";

  inputs = {
    nixpkgs.url = "github:dustinlyons/nixpkgs/master";
    agenix.url = "github:ryantm/agenix";
    home-manager.url = "github:nix-community/home-manager";
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

  outputs = { self, darwin, home-manager, nixpkgs, disko, agenix, secrets } @inputs:
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
      red = "\033[1;31m";
      green = "\033[1;32m";
      reset = "\033[0m";
    in
    {
      # Define a self-contained environment with age and yubikey-age-plugin
      ageEnvironment = nixpkgs.legacyPackages.x86_64-linux.stdenv.mkDerivation rec {
        name = "age-environment";
        buildInputs = with nixpkgs.legacyPackages.x86_64-linux; [ bashInteractive age age-plugin-yubikey ];
        buildCommand = ''
          mkdir -p $out/bin

          cat > $out/bin/decrypt <<'EOF'
          #!/usr/bin/env bash
          # Mounting USB stick
          mkdir -p /mnt/usb
          mount /dev/sdc /mnt/usb || { echo "${red}Mounting USB stick failed!${reset}"; exit 1; }
          echo "${green}USB stick mounted successfully.${reset}"

          # Decrypting the files
          age-plugin-yubikey --identity > identity 2>/dev/null
          cat /mnt/usb/id_ed25519_dustin.age | age -d -i identity > ~/.ssh/id_ed25519 || { echo "${red}Decryption of id_ed25519_dustin.age failed!${reset}"; exit 1; }
          cat /mnt/usb/id_ed25519_bootstrap.age | age -d -i identity > ~/.ssh/id_ed25519_bootstrap || { echo "${red}Decryption of id_ed25519_bootstrap.age failed!${reset}"; exit 1; }
          echo "${green}Decryption complete.${reset}"

          # Copying the .pub files
          cp /mnt/usb/id_ed25519.pub ~/.ssh/ || { echo "${red}Copying id_ed25519.pub failed!${reset}"; exit 1; }
          cp /mnt/usb/id_ed25519_bootstrap.pub ~/.ssh/ || { echo "${red}Copying id_ed25519_bootstrap.pub failed!${reset}"; exit 1; }
          echo "${green}.pub files copied successfully.${reset}"

          # Setting up the keys
          chmod 600 ~/.ssh/id_ed25519 || { echo "${red}Setting permissions for id_ed25519 failed!${reset}"; exit 1; }
          chmod 600 ~/.ssh/id_ed25519_bootstrap || { echo "${red}Setting permissions for id_ed25519_bootstrap failed!${reset}"; exit 1; }
          echo "${green}Key permissions set successfully.${reset}"

          # Unmounting the USB stick
          umount /mnt/usb || { echo "${red}Unmounting USB stick failed!${reset}"; exit 1; }
          echo "${green}USB stick unmounted successfully.${reset}"
          EOF

          chmod +x $out/bin/decrypt
        '';
      };

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
            if [ -e /etc/NIXOS ]; then
                echo "${green}Running in the NixOS installer environment.${reset}"
            else
                echo "${red}Not running in the NixOS installer environment.${reset}"
            fi

            echo "${green}Cleaning previous configuration...${reset}"
            rm -rf nixos-config-main.zip && rm -rf nixos-config-main && rm -rf nixos-config

            echo "${yellow}Downloading nixos-config from Github...${reset}"
            curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip || { echo "${red}Download failed!${reset}"; exit 1; }
            echo "${green}Download complete.${reset}"

            unzip nixos-config-main.zip && mv nixos-config-main nixos-config || { echo "${red}Extraction or moving failed!${reset}"; exit 1; }

            echo "${yellow}Running disko...${reset}"
            sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes \
              github:nix-community/disko -- --mode zap_create_mount ./nixos-config/nixos/disk-config.nix || { echo "${red}Disko run failed!${reset}"; exit 1; }

            echo "${green}Partition and filesystem complete.${reset}"

            echo "${green}Setting up directory structure...${reset}"
            sudo mkdir -p /mnt/etc/nixos || { echo "${red}Directory structure setup failed!${reset}"; exit 1; }

            sudo cp -r nixos-config/* /mnt/etc/nixos && cd /mnt/etc/nixos || { echo "${red}Copying nixos-config failed!${reset}"; exit 1; }

            echo "${yellow}Installing NixOS...${reset}"
            sudo nixos-install --flake .#felix || { echo "${red}NixOS installation failed!${reset}"; exit 1; }
            echo "${yellow}Setting group permissions...${reset}"
            sudo chmod -R 775 /mnt/etc/nixos || { echo "${red}Failed to set group permissions on /mnt/etc/nixos!${reset}"; exit 1; }
            echo "${green}Installation complete.${reset}"

            # Prompt the user to reboot
            read -p "Do you want to reboot now? (y/yes) " choice
            case "$choice" in
            y|Y|yes|YES ) echo "${green}Rebooting...${reset}" && sudo reboot;;
            * ) echo "${yellow}Reboot skipped.${reset}";;
            esac

            '')}/bin/install";
        };

        x86_64-linux.secrets = {
          type = "app";
          program = "${ageEnvironment}/bin/decrypt";
        };
      };
    };
}
