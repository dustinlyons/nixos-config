{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  brave
  brlaser
  cawbird
  cypress
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
  keepassxc
  pinentry-curses
  postgresql
  qmk
  ripgrep
  rnix-lsp # lsp-mode for nix
  spotify
  sqlite
  ssm-session-manager-plugin
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
