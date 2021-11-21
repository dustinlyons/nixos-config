{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  brave
  mycrypto
  etcher
  electrum
  direnv
  fontconfig
  gimp
  home-manager
  openssh
  obs-studio
  ripgrep
  tree
  tdesktop
  unixtools.ifconfig
  unixtools.netstat
  xdg_utils
  wineWowPackages.stable
  yubikey-manager
  yubikey-manager-qt
  yubikey-agent
]
