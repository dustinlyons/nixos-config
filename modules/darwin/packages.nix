{ pkgs }:

with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [
  # D
  dockutil # Manage icons in the dock

  # F
  fswatch # File change monitor
  # rpi-imager # Raspberry PI SD card imager (commented out - broken in current nixpkgs, build fails trying to fetch git)
]
