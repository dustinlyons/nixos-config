{ config, pkgs, ...}:
{

  imports = [
    ./cachix # Community builds of Emacs so we don't have to
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };

    overlays =
      let path = ../overlays; in with builtins;
      # Load everything in overlays/ dir
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))
      # We use the nix-community Emacs patches
      ++ [(import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/refs/heads/master.tar.gz";
        sha256 = "0jnz4wplyvyfsdjbl6xxmd11pr5pl9f8dg90f6nq4fd84l2hpwbj";
      }))];
  };

}
