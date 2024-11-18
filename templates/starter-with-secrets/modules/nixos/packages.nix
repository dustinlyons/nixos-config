{ pkgs }:

with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [

  # Security and authentication
  yubikey-agent
  keepassxc

  # App and package management
  appimage-run
  gnumake
  cmake
  home-manager

  # Media and design tools
  vlc
  fontconfig
  font-manager

  # Productivity tools
  bc # old school calculator
  galculator

  # Audio tools
  pavucontrol # Pulse audio controls

  # Testing and development tools
  direnv
  rofi
  rofi-calc
  postgresql
  libtool # for Emacs vterm

  # Screenshot and recording tools
  flameshot

  # Text and terminal utilities
  feh # Manage wallpapers
  screenkey
  tree
  unixtools.ifconfig
  unixtools.netstat
  xclip # For the org-download package in Emacs
  xorg.xwininfo # Provides a cursor to click and learn about windows
  xorg.xrandr

  # File and system utilities
  inotify-tools # inotifywait, inotifywatch - For file system events
  i3lock-fancy-rapid
  libnotify
  pcmanfm # File browser
  sqlite
  xdg-utils

  # Other utilities
  yad # yad-calendar is used with polybar
  xdotool
  google-chrome

  # PDF viewer
  zathura

  # Music and entertainment
  spotify
]
