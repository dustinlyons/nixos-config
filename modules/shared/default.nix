{ config, pkgs, claude-code, ... }:

let
  emacsOverlaySha256 = "1wcihkc7j8kdx2wvbgv630is06vl1l2gfrxnmjxhggdxfnl8gb60";
  
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
        hostname =
          if (config.networking.hostName or null) == null
          then ""
          else config.networking.hostName;
        excludeForHost = {
          "garfield" = [ "cider-appimage.nix" "obsidian-appimage.nix" "curseforge-appimage.nix" ];
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
               url = "https://github.com/nix-community/emacs-overlay/archive/refs/heads/master.tar.gz";
               sha256 = emacsOverlaySha256;
           }))]
      ++ [ claude-code.overlays.default ];
  };
}
