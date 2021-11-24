{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  brave
  cabextract
  mycrypto
  etcher
  electrum
  direnv
  fontconfig
  gimp
  gnumake
  gnugrep
  libusb1
  home-manager
  openssh
  obs-studio
  ripgrep
  rnix-lsp
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
