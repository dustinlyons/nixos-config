
{
  description = "Starter Configuration for NixOS and MacOS";

  inputs = {
    nixpkgs.url = "github:dustinlyons/nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-homebrew = {
      url = "github:zhaofengli-wip/nix-homebrew";
    };
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    }; 
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, darwin, nix-homebrew, homebrew-core, homebrew-cask, home-manager, nixpkgs, disko, } @inputs:
    let
      user = "%USER%";
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
      templates = {
        starter = {
          path = ./templates;
          description = "Starter config";
        };
      };

      devShells = forAllSystems devShell;

      darwinConfigurations = let user = "%USER%"; in {
        macos = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = inputs;
          modules = [
            nix-homebrew.darwinModules.nix-homebrew
            {
              nix-homebrew = {
                enable = true;
                user = "${user}";
                taps = {
                  "homebrew/homebrew-core" = homebrew-core;
                  "homebrew/homebrew-cask" = homebrew-cask;
                };
                mutableTaps = false;
                autoMigrate = true;
              };
            }
            ./darwin
          ];
        };
      };

      nixosConfigurations = let user = "%USER%"; in {
        nixos = nixpkgs.lib.nixosSystem {
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

      # Imperative install command for NixOS and age-encrypted secrets
      apps = {
        x86_64-linux.install = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "install" ''
            #!/usr/bin/env bash
            set -e

            add_git_to_path() {
              PATH=${nixpkgs.legacyPackages.x86_64-linux.git}/bin:$PATH
            }

            check_installer() {
              if [ -e /etc/NIXOS ]; then
                echo -e "\e[1;32mRunning in the NixOS installer environment.\e[0m"
              else
                echo -e "\e[1;31mNot running in the NixOS installer environment.\e[0m"
                exit 1
              fi
            }

            cleanup() {
              rm -rf nixos-config-main.zip nixos-config-main nixos-config
            }

            download_config() {
              curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip
              unzip nixos-config-main.zip
              mv nixos-config-main/templates nixos-config
            }

            run_disko() {
              sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes \
                github:nix-community/disko -- --mode zap_create_mount ./nixos-config/nixos/disk-config.nix
            }

            setup_files() {
              sudo mkdir -p /mnt/etc/nixos
              sudo cp -r nixos-config/* /mnt/etc/nixos
              cd /mnt/etc/nixos
            }

            install_nixos() {
              sudo nixos-install --flake .#felix
              sudo chmod -R 775 /mnt/etc/nixos
            }

            prompt_reboot() {
              read -p "Do you want to reboot now? (y/yes) " choice
              case "$choice" in
              y|Y|yes|YES ) echo -e "\e[1;32mRebooting...\e[0m" && sudo reboot;;
              * ) echo -e "\e[1;33mReboot skipped.\e[0m";;
              esac
            }

            add_git_to_path
            check_installer
            download_config
            run_disko
            setup_files
            install_nixos
            cleanup
            prompt_reboot

          '')}/bin/install";
        };

     };
  };
}
