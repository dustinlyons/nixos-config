{ config, pkgs, claude-code, ... }:

let
  # NOTE: this pins a *moving* ref (refs/heads/master) by hash, so it goes
  # stale every time emacs-overlay pushes upstream and every build then fails
  # with a hash mismatch — for every host, not just the one being worked on.
  # Bumped 2026-08-31 because it was already broken. Pinning a tag or promoting
  # it to a proper flake input would stop this recurring.
  emacsOverlaySha256 = "16r9yzjkjvvgfx86n2qaykjj6qw2my9mmqibpggpjj9hkrv09ipi";
  
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
