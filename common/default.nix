{ config, pkgs, ...}:
{

  imports = [
    ./cachix.nix # Community builds of Emacs so we don't have to
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
        url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        sha256 = "01p8wj6gb2h6q5l4kxaxjg17qkdl62062p1v542h7sbhhzxvpfl6";
      }))];
  };

}
