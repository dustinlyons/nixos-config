{ pkgs }:

with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [

  # Security and authentication
  _1password-gui
  yubikey-agent
  keepassxc

  # App and package management
  gnumake
  cmake
  home-manager

  # Media and design tools
  gimp
  vlc

  # Printers and drivers
  brlaser # printer driver

  # Calculators
  bc # old school calculator

  # Audio tools
  pavucontrol # Pulse audio controls

  # Messaging and chat applications
  cider # Apple Music on Linux
  discord

  # Testing and development tools
  chromedriver
  direnv
  qmk

  # Screenshot and recording tools
  simplescreenrecorder

  # Text and terminal utilities
  emote # Emoji picker
  screenkey
  tree
  unixtools.ifconfig
  unixtools.netstat
  xclip # For the org-download package in Emacs

  # File and system utilities
  inotify-tools # inotifywait, inotifywatch - For file system events
  libnotify
  playerctl # Control media players from command line

  # Other utilities
  google-chrome

  # PDF viewer
  zathura
]
