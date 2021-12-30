{ config, pkgs, ...}:
{

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
        sha256 = "1d6dxmpm6iq4alfyx2zhyrpp6glpmaganjgd119chsadsr0rvv35";
      }))];
  };

  networking.extraHosts =
  ''
    192.168.0.67 BRN008077D92A06.local
  '';
}
