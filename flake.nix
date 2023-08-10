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

      apps = {
        x86_64-linux.install = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "install" ''
            #!/usr/bin/env bash
            set -e

            # Add Git to the PATH
            PATH=${nixpkgs.legacyPackages.x86_64-linux.git}/bin:$PATH

            if [ -e /etc/NIXOS ]; then
                echo -e "\033[1;32mRunning in the NixOS installer environment.\033[0m"
            else
                echo -e "\033[1;31mNot running in the NixOS installer environment.\033[0m"
            fi

            echo -e "\033[1;32mCleaning previous configuration...\033[0m"
            rm -rf nixos-config-main.zip && rm -rf nixos-config-main && rm -rf nixos-config

            echo -e "\033[1;33mDownloading nixos-config from Github...\033[0m"
            curl -LJ0 https://github.com/dustinlyons/nixos-config/archive/main.zip -o nixos-config-main.zip || { echo -e "\033[1;31mDownload failed!\033[0m"; exit 1; }
            echo -e "\033[1;32mDownload complete.\033[0m"

            unzip nixos-config-main.zip && mv nixos-config-main nixos-config || { echo -e "\033[1;31mExtraction or moving failed!\033[0m"; exit 1; }

            echo -e "\033[1;33mRunning disko...\033[0m"
            sudo nix run --extra-experimental-features nix-command --extra-experimental-features flakes \
              github:nix-community/disko -- --mode zap_create_mount ./nixos-config/nixos/disk-config.nix || { echo -e "\033[1;31mDisko run failed!\033[0m"; exit 1; }
            echo -e "\033[1;32mPartition and filesystem complete.\033[0m"

            echo -e "\033[1;32mSetting up directory structure...\033[0m"
            sudo mkdir -p /mnt/etc/nixos || { echo -e "\033[1;31mDirectory structure setup failed!\033[0m"; exit 1; }
            sudo cp -r nixos-config/* /mnt/etc/nixos && cd /mnt/etc/nixos || { echo -e "\033[1;31mCopying nixos-config failed!\033[0m"; exit 1; }

            # Ensure known_hosts file exists and add GitHub to known hosts
            echo -e "\033[1;32mSaving Github public key...\033[0m"
            mkdir -p /root/.ssh
            touch /root/.ssh/known_hosts
            ssh-keyscan -t ed25519 github.com >> /root/.ssh/known_hosts

            echo -e "\033[1;33mCopy keys...\033[0m"
            mkdir -p /mnt/home/${user}/.ssh || { echo -e "\033[1;31mFailed to create .ssh directory!\033[0m"; exit 1; }
            chown nixos /mnt/home/${user}/.ssh || { echo -e "\033[1;31mFailed to set ownership on .ssh directory!\033[0m"; exit 1; }

            chown nixos /root/.ssh/id_ed25519_agenix || { echo -e "\033[1;31mFailed to set ownership on private key!\033[0m"; exit 1; }
            chown nixos /root/.ssh/id_ed25519_agenix.pub || { echo -e "\033[1;31mFailed to set ownership on public key!\033[0m"; exit 1; }
            mv /root/.ssh/id_ed25519_agenix /mnt/home/${user}/.ssh/id_ed25519 || { echo -e "\033[1;31mFailed to copy private key!\033[0m"; exit 1; }
            mv /root/.ssh/id_ed25519_agenix.pub /mnt/home/${user}/.ssh/id_ed25519.pub || { echo -e "\033[1;31mFailed to copy public key!\033[0m"; exit 1; }

            chmod 600 /mnt/home/${user}/.ssh/id_ed25519.pub || { echo -e "\033[1;31mFailed to set permissions on public key!\033[0m"; exit 1; }
            chmod 600 /mnt/home/${user}/.ssh/id_ed25519 || { echo -e "\033[1;31mFailed to set permissions on private key!\033[0m"; exit 1; }

            ln -s /mnt/home/dustin /home/dustin # Used to grab initial secrets
            echo -e "\033[1;32mKeys copied.\033[0m"
 
            echo -e "\033[1;33mInstalling NixOS...\033[0m"
            sudo nixos-install --flake .#felix || { echo -e "\033[1;31mNixOS installation failed!\033[0m"; exit 1; }
            echo -e "\033[1;33mSetting group permissions...\033[0m"
            sudo chmod -R 775 /mnt/etc/nixos || { echo -e "\033[1;31mFailed to set group permissions on /mnt/etc/nixos!\033[0m"; exit 1; }
            echo -e "\033[1;32mInstallation complete.\033[0m"

            # Prompt the user to reboot
            read -p "Do you want to reboot now? (y/yes) " choice
            case "$choice" in
            y|Y|yes|YES ) echo -e "\033[1;32mRebooting...\033[0m" && sudo reboot;;
            * ) echo -e "\033[1;33mReboot skipped.\033[0m";;
            esac
          '')}/bin/install";
        };

        x86_64-linux.secrets = {
          type = "app";
          program = "${(nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "decrypt" ''
            #!/usr/bin/env bash
            set -e

            # Function to unmount USB if it's mounted
            function unmount_usb {
              if mountpoint -q /mnt/usb; then
                sudo umount /mnt/usb || { echo -e "\033[0;31mUnmounting USB stick failed!\033[0m"; exit 1; }
                echo -e "\033[0;32mUSB stick unmounted successfully.\033[0m"
              fi
            }

            # Trap to ensure unmounting in case of a failure
            trap unmount_usb EXIT

            # Check if USB is already mounted
            if mountpoint -q /mnt/usb; then
              echo -e "\033[0;32mUSB stick already mounted.\033[0m"
            else
              # Attempt to mount the USB stick by checking sdc, sdd, sde, etc.
              for dev in sdc sdd sde sdf sdg sdh sdi sdj sdk sdl; do
                if sudo blkid /dev/$dev | grep -iq 'TYPE="vfat"'; then
                  # Mounting USB stick
                  mkdir -p /mnt/usb
                  sudo mount /dev/$dev /mnt/usb && { echo -e "\033[0;32mUSB stick mounted successfully on /dev/$dev.\033[0m"; break; } || echo -e "\033[0;31mFailed to mount /dev/$dev.\033[0m"
                fi
              done
            fi

            # Setting up SSH directory
            SSH_DIR=/root/.ssh
            mkdir -p $SSH_DIR

            # Copying the .pub files
            cp /mnt/usb/id_ed25519_github.pub $SSH_DIR/id_ed25519.pub || { echo -e "\033[0;31mCopying id_ed25519.pub failed!\033[0m"; exit 1; }
            cp /mnt/usb/id_ed25519_agenix.pub $SSH_DIR || { echo -e "\033[0;31mCopying id_ed25519_agenix.pub failed!\033[0m"; exit 1; }
            echo -e "\033[0;32mPublic keys copied successfully.\033[0m"

            # Copying the private keys
            cp /mnt/usb/id_ed25519_github $SSH_DIR/id_ed25519 || { echo -e "\033[0;31mCopying id_ed25519 failed!\033[0m"; exit 1; }
            cp /mnt/usb/id_ed25519_agenix $SSH_DIR || { echo -e "\033[0;31mCopying id_ed25519_agenix failed!\033[0m"; exit 1; }
            echo -e "\033[0;32mPrivate keys copied successfully.\033[0m"
 
            # Setting permissions for the public keys
            chmod 600 $SSH_DIR/id_ed25519.pub || { echo -e "\033[0;31mSetting permissions for id_ed25519 failed!\033[0m"; exit 1; }
            chmod 600 $SSH_DIR/id_ed25519_agenix.pub || { echo -e "\033[0;31mSetting permissions for id_ed25519_agenix failed!\033[0m"; exit 1; }
            echo -e "\033[0;32mKey permissions set successfully.\033[0m"

            # Setting permissions for the private keys
            chmod 600 $SSH_DIR/id_ed25519 || { echo -e "\033[0;31mSetting permissions for id_ed25519 failed!\033[0m"; exit 1; }
            chmod 600 $SSH_DIR/id_ed25519_agenix || { echo -e "\033[0;31mSetting permissions for id_ed25519_agenix failed!\033[0m"; exit 1; }
            echo -e "\033[0;32mPrivate key permissions set successfully.\033[0m"

            # Changing ownership of the keys to user
            chown nixos:wheel $SSH_DIR/id_ed25519 $SSH_DIR/id_ed25519.pub $SSH_DIR/id_ed25519_agenix $SSH_DIR/id_ed25519_agenix.pub || { echo -e "\033[0;31mChanging ownership failed!\033[0m"; exit 1; }
            echo -e "\033[0;32mKeys ownership changed successfully.\033[0m"

            # Unmounting the USB stick
            unmount_usb
          '')}/bin/decrypt";
        };
      };
    };
}
