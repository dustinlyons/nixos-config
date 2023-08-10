{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { inherit pkgs; }; in
common-packages ++ [
  dockutil
]
