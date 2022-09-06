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

  direnv
  discord
  electrum

  # Emoji picker
  emote

  # Manage wallpapers
  feh

  firefox
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
  gtk3
  glib
  home-manager

  # inotifywait, inotifywatch
  # For file system events
  inotify-tools

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

  # Context on pkg-config
  # https://nixos.wiki/wiki/FAQ#I_installed_a_library_but_my_compiler_is_not_finding_it._Why
  pkg-config

  qmk
  rofi
  rnix-lsp # lsp-mode for nix
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
