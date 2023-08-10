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
      user = "dustin";
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

      # Imperative setup NixOS and our secrets
      apps = {
        x86_64-linux.install = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "install" ''
            #!/usr/bin/env bash
            set -e

            add_git_to_path() {
              PATH=${nixpkgs.legacyPackages.x86_64-linux.git}/bin:$PATH
            }

            check_nixos_environment() {
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

            download_and_extract_config() {
              curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip
              unzip nixos-config-main.zip
              mv nixos-config-main nixos-config
            }

            run_disko() {
              sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes \
                github:nix-community/disko -- --mode zap_create_mount ./nixos-config/nixos/disk-config.nix
            }

            setup_directories_and_files() {
              sudo mkdir -p /mnt/etc/nixos
              sudo cp -r nixos-config/* /mnt/etc/nixos
              cd /mnt/etc/nixos

              mkdir -p /root/.ssh
              touch /root/.ssh/known_hosts
              ssh-keyscan -t ed25519 github.com >> /root/.ssh/known_hosts
            }

            setup_ssh_keys() {
              mkdir -p /mnt/home/${user}/.ssh
              chown nixos /mnt/home/${user}/.ssh

              chown nixos /root/.ssh/id_ed25519_agenix{,.pub}
              cp --preserve=all /root/.ssh/id_ed25519_agenix{,/mnt/home/${user}/.ssh/id_ed25519}
              cp --preserve=all /root/.ssh/id_ed25519_agenix.pub /mnt/home/${user}/.ssh/id_ed25519.pub

              chmod 600 /mnt/home/${user}/.ssh/id_ed25519{,.pub}
            }

            link_home_directory() {
              ln -s /mnt/home/dustin /home/dustin # Used to grab initial secrets
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
            check_nixos_environment
            cleanup
            download_and_extract_config
            run_disko
            setup_directories_and_files
            setup_ssh_keys
            link_home_directory
            install_nixos
            prompt_reboot

          '')}/bin/install";
        };

        x86_64-linux.secrets = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "decrypt" ''
            #!/usr/bin/env bash
            set -e

            unmount_usb() {
              if mountpoint -q /mnt/usb; then
                sudo umount /mnt/usb || { echo -e "\e[0;31mUnmounting USB stick failed!\e[0m"; exit 1; }
                echo -e "\e[0;32mUSB stick unmounted successfully.\e[0m"
              fi
            }

            mount_usb() {
              if mountpoint -q /mnt/usb; then
                echo -e "\e[0;32mUSB stick already mounted.\e[0m"
              else
                for dev in sdc sdd sde sdf sdg sdh sdi sdj sdk sdl; do
                  if sudo blkid /dev/$dev | grep -iq 'TYPE="vfat"'; then
                    mkdir -p /mnt/usb
                    sudo mount /dev/$dev /mnt/usb && { echo -e "\e[0;32mUSB stick mounted successfully on /dev/$dev.\e[0m"; break; } || echo -e "\e[0;31mFailed to mount /dev/$dev.\e[0m"
                  fi
                done
              fi
            }

            setup_ssh_directory() {
              export SSH_DIR=/root/.ssh
              mkdir -p $SSH_DIR
            }

            copy_keys() {
              cp /mnt/usb/id_ed25519_{github.pub,agenix.pub} $SSH_DIR || { echo -e "\e[0;31mCopying public keys failed!\e[0m"; exit 1; }
              cp /mnt/usb/id_ed25519_{github,agenix} $SSH_DIR || { echo -e "\e[0;31mCopying private keys failed!\e[0m"; exit 1; }
            }

            set_permissions() {
              chmod 600 $SSH_DIR/id_ed25519{,.pub,_agenix,_agenix.pub} || { echo -e "\e[0;31mSetting permissions failed!\e[0m"; exit 1; }
            }

            change_ownership() {
              chown nixos:wheel $SSH_DIR/id_ed25519{,.pub,_agenix,_agenix.pub} || { echo -e "\e[0;31mChanging ownership failed!\e[0m"; exit 1; }
            }

            trap unmount_usb EXIT

            mount_usb
            setup_ssh_directory
            copy_keys
            set_permissions
            change_ownership
            unmount_usb

          '')}/bin/decrypt";
        };
      };
    };
}
