
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
      templates = {
        full = {
          path = ./templates;
          description = "Full installation";
        };
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
              mv nixos-config-main nixos-config
            }

            run_disko() {
              sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes \
                github:nix-community/disko -- --mode zap_create_mount ./nixos-config/nixos/disk-config.nix
            }

            setup_files() {
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
              cp --preserve=all /root/.ssh/id_ed25519_agenix /mnt/home/${user}/.ssh/id_ed25519
              cp --preserve=all /root/.ssh/id_ed25519_agenix.pub /mnt/home/${user}/.ssh/id_ed25519.pub

              chmod 600 /mnt/home/${user}/.ssh/id_ed25519{,.pub}
            }

            link_home_dir() {
              ln -s /mnt/home/${user} /home/${user} # Used to grab initial secrets
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
            setup_ssh_keys
            link_home_dir
            install_nixos
            cleanup
            prompt_reboot

          '')}/bin/install";
        };

        x86_64-linux.copyKeys = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "copy_keys" ''
            #!/usr/bin/env bash
            set -e

            unmount_usb() {
              if mountpoint -q /mnt/usb; then
                sudo umount /mnt/usb
              fi
            }

            mount_usb() {
              if mountpoint -q /mnt/usb; then
                echo -e "\e[0;32mUSB drive already mounted.\e[0m"
              else
                for dev in sdc sdd sde sdf sdg sdh sdi sdj sdk sdl; do
                  if sudo blkid /dev/$dev | grep -iq 'TYPE="vfat"'; then
                    mkdir -p /mnt/usb
                    sudo mount /dev/$dev /mnt/usb && { echo -e "\e[0;32mUSB drive mounted successfully on /dev/$dev.\e[0m"; break; } || echo -e "\e[0;31mFailed to mount /dev/$dev.\e[0m"
                  fi
                done
              fi
            }
            handle_no_usb() {
            echo -e ''${RED}No USB drive found or mounted.''${NC}"
            echo -e ''${GREEN}If you have not yet set up your keys, run the script to generate new SSH keys.''${NC}"
            exit 1
            }

            setup_ssh_directory() {
              export SSH_DIR=/root/.ssh
              mkdir -p $SSH_DIR
            }

            copy_keys() {
              cp /mnt/usb/id_ed25519_agenix.pub $SSH_DIR
              cp /mnt/usb/id_ed25519_agenix $SSH_DIR
              chmod 600 $SSH_DIR/id_ed25519_{agenix,agenix.pub}
            }

            set_keys() {
              cp /mnt/usb/id_ed25519_github.pub $SSH_DIR/id_ed25519.pub
              cp /mnt/usb/id_ed25519_github $SSH_DIR/id_ed25519
              chmod 600 $SSH_DIR/id_ed25519
              chmod 644 $SSH_DIR/id_ed25519.pub
            }

            change_ownership() {
              chown nixos:wheel $SSH_DIR/id_ed25519{,.pub}
              chown nixos:wheel $SSH_DIR/id_ed25519_{agenix,agenix.pub}
            }

            trap unmount_usb EXIT

            setup_ssh_directory
            mount_usb
            copy_keys
            set_keys
            change_ownership
            unmount_usb

          '')}/bin/copy_keys";
        };

        aarch64-darwin.copyKeys = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.aarch64-darwin.writeShellScriptBin "copy_keys" ''
            #!/usr/bin/env bash
            set -e

            RED='\033[0;31m'
            GREEN='\033[0;32m'
            NC='\033[0m'

            echo -e "Please enter your username: "
            read username

            username=''${USER}

            export SSH_DIR=/Users/''${username}/.ssh

            unmount_usb() {
            if mount | grep -q '/mnt/usb'; then
                umount /mnt/usb
            fi
            }

            mount_usb() {
            if mount | grep -q '/mnt/usb'; then
                echo -e "''${GREEN}USB drive already mounted.''${NC}"
            else
                for dev in ''$(diskutil list | grep -o 'disk[0-9]'); do
                if diskutil info /dev/''${dev} | grep -iq 'Type (Bundle):[[:space:]]*msdos'; then
                    mkdir -p /mnt/usb
                    mount -t msdos /dev/''${dev} /mnt/usb && { echo -e "''${GREEN}USB drive mounted successfully on /dev/''${dev}.''${NC}"; break; }
                fi
                done
            fi
            }

            setup_ssh_directory() {
            mkdir -p ''${SSH_DIR}
            }

            copy_keys() {
            cp /mnt/usb/id_ed25519_agenix.pub ''${SSH_DIR}
            cp /mnt/usb/id_ed25519_agenix ''${SSH_DIR}
            chmod 600 ''${SSH_DIR}/id_ed25519_{agenix,agenix.pub}
            }

            set_keys() {
            cp /mnt/usb/id_ed25519_github.pub ''${SSH_DIR}/id_ed25519.pub
            cp /mnt/usb/id_ed25519_github ''${SSH_DIR}/id_ed25519
            chmod 600 ''${SSH_DIR}/id_ed25519
            chmod 644 ''${SSH_DIR}/id_ed25519.pub
            }

            change_ownership() {
            chown ''${username}:staff ''${SSH_DIR}/id_ed25519{,.pub}
            chown ''${username}:staff ''${SSH_DIR}/id_ed25519_{agenix,agenix.pub}
            }

            trap unmount_usb EXIT

            setup_ssh_directory
            mount_usb
            copy_keys
            if ! mount | grep -q '/mnt/usb'; then
                handle_no_usb
            fi
            set_keys
            change_ownership
            unmount_usb

            '')}/bin/copy_keys";
          };

          aarch64-darwin.createKeys = {
            type = "app";
            program = "${(nixpkgs.legacyPackages.aarch64-darwin.writeShellScriptBin "create_keys" ''
                #!/usr/bin/env bash
                set -e

                RED='\033[0;31m'
                GREEN='\033[0;32m'
                NC='\033[0m'

                echo -e "Please enter your username: "
                read username

                export SSH_DIR=/Users/''${username}/.ssh

                setup_ssh_directory() {
                mkdir -p ''${SSH_DIR}
                }

                generate_keys() {
                ssh-keygen -t ed25519 -f "''${SSH_DIR}/id_ed25519" -N ""
                ssh-keygen -t ed25519 -f "''${SSH_DIR}/id_ed25519_agenix" -N ""
                chown ''${username}:staff ''${SSH_DIR}/id_ed25519{,_agenix}{,.pub}
                }

                setup_ssh_directory
                generate_keys

                echo -e "''${GREEN}New SSH keys have been generated.''${NC}"
                echo -e "''${GREEN}1) Add the Github key to Github.''${NC}"
                cat "''${SSH_DIR}/id_ed25519.pub"
                echo -e "''${GREEN}2) Create a private nix-secrets repo in Github, even if it's empty.''${NC"

          '')}/bin/create_keys";
        };
      };
    };
}
