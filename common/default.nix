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
               url = "https://github.com/dustinlyons/emacs-overlay/archive/refs/heads/master.tar.gz";
               sha256 = "05s1pchmiwg7vs994lzrddrhb54zix3faq372g7vwrwyms9vf151";
           }))];
  };
}
