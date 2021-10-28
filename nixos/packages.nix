{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  brave
  direnv
  fontconfig
  gimp
  home-manager
  openssh
  ripgrep
  tree
  unixtools.ifconfig
  unixtools.netstat
  xdg_utils
]
