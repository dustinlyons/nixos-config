{ pkgs }:

with pkgs;
let common-packages = import ../common/packages.nix { pkgs = pkgs; }; in
common-packages ++ [
  _1password-gui
  appimage-run
  betterlockscreen
  brlaser # printer driver

  bc

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

  # Screenshot tools
  flameshot
  peek

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

  # games
  lutris
  steam

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

  yad # I use yad-calendar with polybar
  xdotool 

  yubikey-manager
  yubikey-manager-qt
  yubikey-agent

  xclip # For the org-download package in Emacs
  xorg.xwininfo # Provides a cursor to click and learn about windows
  # PDF viewer
  zathura
]
