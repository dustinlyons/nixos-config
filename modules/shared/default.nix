{ config, pkgs, ... }:

let
  emacsOverlaySha256 = "11p1c1l04zrn8dd5w8zyzlv172z05dwi9avbckav4d5fk043m754";
  
  # Shared Emacs package configuration
  myEmacs = import ./emacs.nix { inherit pkgs; };
in
{

  nixpkgs = {
    config = {
      allowUnfree = true;
      #cudaSupport = true;
      #cudaCapabilities = ["8.0"];
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };

    overlays =
      # Apply each overlay found in the /overlays directory
      let
        path = ../../overlays;
        hostname = config.networking.hostName or "";
        excludeForHost = {
          "garfield" = [ "cider-appimage.nix" ];
        };
        excludedFiles = excludeForHost.${hostname} or [];
      in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n:
            (match ".*\\.nix" n != null ||
             pathExists (path + ("/" + n + "/default.nix")))
            && !(elem n excludedFiles))
                  (attrNames (readDir path)))

      ++ [(import (builtins.fetchTarball {
               url = "https://github.com/dustinlyons/emacs-overlay/archive/refs/heads/master.tar.gz";
               sha256 = emacsOverlaySha256;
           }))];
  };
}
