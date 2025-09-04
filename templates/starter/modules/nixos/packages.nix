{ pkgs }:

with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [

  # Security and authentication
  yubikey-agent

  # App and package management
  appimage-run
  gnumake
  cmake
  home-manager

  # Media and design tools
  fontconfig

  # Productivity tools

  # Audio tools
  pavucontrol # Pulse audio controls

  # Testing and development tools
  rofi
  rofi-calc
  libtool # for Emacs vterm

  # Screenshot and recording tools
  flameshot

  # Text and terminal utilities
  tree
  unixtools.ifconfig
  unixtools.netstat
  xclip # For the org-download package in Emacs
  xorg.xwininfo # Provides a cursor to click and learn about windows
  xorg.xrandr

  # File and system utilities
  inotify-tools # inotifywait, inotifywatch - For file system events
  libnotify
  pcmanfm # File browser
  sqlite
  xdg-utils

  # Other utilities
  google-chrome

  # PDF viewer
  zathura

  # Development tools
  firefox
  
  # Music and entertainment
]
