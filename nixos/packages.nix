{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  appimage-run
  brave
  brlaser
  cawbird
  cypress
  direnv
  electrum
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
  keepassxc
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

  # 6/23/22
  # Broken build, fix merged upstream
  # Try again later
  # 
  # yubikey-manager-qt
  # tdesktop # telegram desktop

  yubikey-agent
]
