{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  appimage-run
  betterlockscreen
  brlaser
  cawbird
  cava
  cider
  cypress
  direnv
  discord
  electrum
  emote
  feh
  firefox
  fontconfig
  font-manager
  fractal
  flameshot
  gimp
  gnumake
  gnugrep
  google-chrome
  home-manager
  inotify-tools
  imagemagick
  keepassxc
  libreoffice
  libusb1 # for Xbox controller
  ledger-live-desktop
  openssh
  pavucontrol
  pinentry-curses
  postgresql
  playerctl
  pcmanfm
  qmk
  rofi
  rnix-lsp # lsp-mode for nix
  spotify
  sqlite
  tree
  unixtools.ifconfig
  unixtools.netstat
  xdg_utils
  vlc
  wineWowPackages.stable
  yubikey-manager
  yubikey-manager-qt
  tdesktop # telegram desktop
  yubikey-agent
]
