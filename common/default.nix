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
      # Apply each overlay found in the /overlays directory
      let path = ../overlays; in with builtins;
      map (n: import (path + ("/" + n)))
          (filter (n: match ".*\\.nix" n != null ||
                      pathExists (path + ("/" + n + "/default.nix")))
                  (attrNames (readDir path)))

      ++ [(import (builtins.fetchTarball { 
	       url = "https://github.com/dustin-lyons/emacs-overlay/archive/refs/heads/master.tar.gz";
               sha256 = "0dzw4azhh96vdg1zl323b4l2ahg3lqkkf8lj22x10s5lg3hy9ryr";
	}))]; 
  };
}
