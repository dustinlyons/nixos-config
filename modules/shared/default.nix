{ config, pkgs, claude-code, ... }:

let
  emacsOverlaySha256 = "17cigsz0nbc826vdziywanc30vr795ihasmrrpksr6y7d3gm4f6w";
  
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
