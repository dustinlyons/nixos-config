
{
  description = "Dustin's Configuration for NixOS and MacOS";

  inputs = {
    nixpkgs.url = "github:dustinlyons/nixpkgs/master";

    # My nixpkgs fork includes my feather-font package (https://github.com/dustinlyons/feather-font)
    # and a timeout setting that helps Emacs daemon take longer to build the first time. One day I'll 
    # try to merge these upstream, but until then follow these steps to use the official repo instead:
    #
    # Change the flake input
    # - Official repository
    #   nixpkgs.url = "github:NixOS/nixpkgs/master";
    # 
    # Remove this setting and retry builds if they sometimes timeout:
    # - NixOS configuration
    #   https://github.com/dustinlyons/nixos-config/blob/8114714c10d61cd5da34df842dd5bac0301f688a/nixos/default.nix#L280
    #
    # Replace feather-font with another font:
    # - Rofi:
    #   https://github.com/dustinlyons/nixos-config/blob/1290219734b53b26d9c20d13989846788462ff26/nixos/config/rofi/launcher.rasi#L42
    # 
    # - Polybar:
    #   https://github.com/dustinlyons/nixos-config/blob/1290219734b53b26d9c20d13989846788462ff26/nixos/home-manager.nix#L21
    #   https://github.com/dustinlyons/nixos-config/blob/1290219734b53b26d9c20d13989846788462ff26/nixos/config/rofi/styles.rasi#L49
    #   https://github.com/dustinlyons/nixos-config/blob/1290219734b53b26d9c20d13989846788462ff26/nixos/config/rofi/powermenu.rasi#L49
    #   https://github.com/dustinlyons/nixos-config/blob/1290219734b53b26d9c20d13989846788462ff26/nixos/config/rofi/networkmenu.rasi#L49
    # 
    # - Fonts:
    #   https://github.com/dustinlyons/nixos-config/blob/1290219734b53b26d9c20d13989846788462ff26/nixos/default.nix#L286 */

    agenix.url = "github:ryantm/agenix";
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
    secrets = {
      url = "git+ssh://git@github.com/dustinlyons/nix-secrets.git";
      flake = false;
    };
  };
  outputs = { self, darwin, nix-homebrew, homebrew-core, homebrew-cask, home-manager, nixpkgs, disko, agenix, secrets } @inputs:
    let
      user = "dustin";
      linuxSystems = [ "x86_64-linux" "aarch64-linux" ];
      darwinSystems = [ "aarch64-darwin" ];
      forAllLinuxSystems = f: nixpkgs.lib.genAttrs linuxSystems (system: f system);
      forAllDarwinSystems = f: nixpkgs.lib.genAttrs darwinSystems (system: f system);
      forAllSystems = f: nixpkgs.lib.genAttrs (linuxSystems ++ darwinSystems) (system: f system);
      devShell = system: let pkgs = nixpkgs.legacyPackages.${system}; in {
        default = with pkgs; mkShell {
          nativeBuildInputs = with pkgs; [ bashInteractive git age age-plugin-yubikey ];
          shellHook = with pkgs; ''
            export EDITOR=vim
          '';
        };
      };
      mkApp = scriptName: system: {
        type = "app";
        program = "${(nixpkgs.legacyPackages.${system}.writeScriptBin scriptName ''
          #!/usr/bin/env bash
          PATH=${nixpkgs.legacyPackages.${system}.git}/bin:$PATH
          echo "Running ${scriptName} for ${system}"
          exec ${self}/apps/${system}/${scriptName}
        '')}/bin/${scriptName}";
      };
      mkLinuxApps = system: {
        "install" = mkApp "install" system;
        "installWithSecrets" = mkApp "installWithSecrets" system;
        "copyKeys" = mkApp "copyKeys" system;
        "createKeys" = mkApp "createKeys" system;
        "checkKeys" = mkApp "checkKeys" system;
      };
      mkDarwinApps = system: {
        "copyKeys" = mkApp "copyKeys" system;
        "createKeys" = mkApp "createKeys" system;
        "checkKeys" = mkApp "checkKeys" system;
      };
    in
    {
      templates = {
        starter = {
          path = ./templates/starter;
          description = "Starter configuration";
        };
        starterWithSecrets = {
          path = ./templates/starterWithSecrets;
          description = "Starter configuration with secrets";
        };
      };
      devShells = forAllSystems devShell;
      apps = nixpkgs.lib.genAttrs linuxSystems mkLinuxApps // nixpkgs.lib.genAttrs darwinSystems mkDarwinApps;
      darwinConfigurations = let user = "dustin"; in {
        "Dustins-MBP" = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = inputs;
          modules = [
            home-manager.darwinModules.home-manager
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
      nixosConfigurations = nixpkgs.lib.genAttrs linuxSystems (system: nixpkgs.lib.nixosSystem {
        system = system;
        specialArgs = inputs;
        modules = [
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.${user} = import ./nixos/home-manager.nix;
          }
          ./nixos
        ];
     });
  };
}
