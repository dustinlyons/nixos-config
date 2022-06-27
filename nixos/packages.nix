{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  brave
  cawbird
  cypress
  # Marked insecure due to EOL electron version Feb 19, 2022
  docker
  docker-compose
  direnv
  electrum
  firefox
  fontconfig
  gimp
  gnumake
  gnugrep
  google-chrome
  home-manager
  inotify-tools
  imagemagick
  libreoffice
  libusb1 # for Xbox controller
  ledger-live-desktop
  openssh
  pinentry-curses
  postgresql
  qmk
  ripgrep
  rnix-lsp # lsp-mode for nix
  spotify
  sqlite
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
