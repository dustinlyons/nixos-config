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

  notion-app-enhanced 
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
  rofi-calc
  rnix-lsp # lsp-mode for nix
  screenkey
  spotify
  sqlite
  tree
  tdesktop # telegram desktop
  unixtools.ifconfig
  unixtools.netstat
  xdg-utils
  vlc
  wineWowPackages.stable

  # I use yad-calendar with polybar
  yad
  xdotool 

  yubikey-manager
  yubikey-manager-qt
  yubikey-agent

  # PDF viewer
  zathura
]
