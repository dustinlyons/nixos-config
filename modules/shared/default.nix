{ config, pkgs, lib, ... }:

let
  emacsOverlaySha256 = "06413w510jmld20i4lik9b36cfafm501864yq8k4vxl5r4hn0j0h";
  standardUsers = [
    { name = "lessuseless"; }
    { name = "ar4s"; }
    # Add more users as needed
  ];

  userConfigs = lib.mapAttrs (user: attrs: {
    isNormalUser = true;
    home = if pkgs.stdenv.hostPlatform.isDarwin
           then "/Users/${user}"
           else "/home/${user}";
    createHome = true;
    hashedPassword = "<hashed-password>"; # Replace with the actual hashed password
    extraGroups = [ "nix_staff" ];
    shell = pkgs.bashInteractive;
  }) (lib.listToAttrs standardUsers);
in {
  users.groups.nix_staff = { };

  users.users = lib.mkMerge [
    userConfigs
    {
      admin = {
        isNormalUser = true;
        home = if pkgs.stdenv.hostPlatform.isDarwin
               then "/Users/admin"
               else "/home/admin";
        createHome = true;
        extraGroups = [ "wheel" ]; # 'wheel' group allows sudo access
        hashedPassword = "<hashed-password>"; # Replace with the actual hashed password
        shell = pkgs.bashInteractive;
      };
    }
  ];

  # Ensure sudo privileges for members of the "wheel" group
  security.sudo = {
    enable = true;
    wheelNeedsPassword = true; # Members of the wheel group must provide a password for sudo
  };

  # Nix settings
  nix = {
    package = pkgs.nix;
    settings = {
      trusted-users = [ "@admin" "@nix_staff" ];
      substituters = [ "https://nix-community.cachix.org" "https://cache.nixos.org" ];
      trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };

    overlays =
      # Apply each overlay found in the /overlays directory
      let path = ../../overlays; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))

      ++ [(import (builtins.fetchTarball {
               url = "https://github.com/dustinlyons/emacs-overlay/archive/refs/heads/master.tar.gz";
               sha256 = emacsOverlaySha256;
           }))];
  };
}