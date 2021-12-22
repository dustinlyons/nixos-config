{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  brave
  cabextract
  etcher # flash USB drives
  electrum
  docker
  docker-compose
  discord
  direnv
  firefox
  fontconfig
  gimp
  gnumake
  gnugrep
  google-chrome
  libreoffice
  libusb1 # for Xbox controller
  home-manager
  openssh
  obs-studio # for streaming, video
  pinentry-curses
  postgresql
  ripgrep
  rnix-lsp # lsp-mode for nix
  tree
  tdesktop # telegram desktop
  unixtools.ifconfig
  unixtools.netstat
  xdg_utils
  wineWowPackages.stable
  yubikey-manager
  yubikey-manager-qt
  yubikey-agent
]
