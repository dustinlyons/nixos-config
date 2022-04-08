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
		url = "https://github.com/nix-community/emacs-overlay/archive/refs/heads/master.tar.gz";
		sha256 = "1pyq0cygc1w5n51lbqch2k5fz9csrk2dxlkqhkflf21wxk7v155i";
	}))];
  };
}
