{ config, pkgs, ...}:
let
  emacsOverlaySha256 = if builtins.currentSystem  == "aarch64-darwin"
    then "07ks98m0zj61jz20fw2vqax1c61374hr06fzmw9c9xq70bsx1y9l"
    else "1nrpw2w2jfpgf85lxwddnnl0s8sv2j5pq0rb5jmgy65644skrsfq";
in
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
               sha256 = emacsOverlaySha256;
           }))];
  };
}
