{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  appimage-run
  brave
  brlaser
  cawbird
  cider
  cypress
  direnv
  electrum
  emote
  firefox
  fontconfig
  fractal
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
  unixtools.ifconfig
  unixtools.netstat
  xdg_utils
  wineWowPackages.stable
  yubikey-manager
  yubikey-manager-qt
  tdesktop # telegram desktop
  yubikey-agent
]
