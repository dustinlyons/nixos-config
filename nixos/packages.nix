{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  appimage-run
  betterlockscreen
  brlaser # printer driver

  # Terminal audio visualizer
  cava

  # Apple Music on Linux
  cider

  # Functional testing framework using headless chrome
  cypress

  cmake
  direnv
  discord
  electrum

  # Emoji picker
  emote

  # Manage wallpapers
  feh

  fontconfig
  font-manager

  # Matrix.org messaging app
  fractal

  # Screenshot tool
  flameshot

  gimp
  gnumake
  gnugrep
  google-chrome
  home-manager

  # inotifywait, inotifywatch
  # For file system events
  inotify-tools
  libnotify

  imagemagick
  keepassxc
  libreoffice
  libusb1 # for Xbox controller
  ledger-live-desktop
  openssh

  # Pulse audio controls
  pavucontrol

  # Control media players from command line
  # Used by other tools
  playerctl

  pinentry-curses
  postgresql

  # Our file browser
  pcmanfm

  qmk
  rofi
  rnix-lsp # lsp-mode for nix
  screenkey
  spotify
  sqlite
  tree
  tdesktop # telegram desktop
  unixtools.ifconfig
  unixtools.netstat
  xdg_utils
  vlc
  wineWowPackages.stable
  yubikey-manager
  yubikey-manager-qt
  yubikey-agent

  # PDF viewer
  zathura
]
