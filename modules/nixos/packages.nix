{ pkgs, inputs }:
with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [

  _1password-gui # Password manager
  
  apple-cursor # macOS-style cursor theme
  
  bemenu # Wayland-native application launcher
  
  cliphist # Clipboard history manager for Wayland

  brlaser # Printer driver

  chromedriver # Chrome webdriver for testing

  inputs.claude-desktop.packages."${pkgs.system}".claude-desktop-with-fhs

  discord # Voice and text chat

  gimp # Image editor
  google-chrome # Web browser
  
  hyprpicker # Wayland color picker

  imv # Lightweight Wayland image viewer
  
  keepassxc # Password manager

  pavucontrol # Pulse audio controls
  playerctl # Control media players from command line

  qmk # Keyboard firmware toolkit

  screenkey # Display pressed keys on screen
  simplescreenrecorder # Screen recording tool

  unixtools.ifconfig # Network interface configuration
  unixtools.netstat # Network statistics

  vlc # Media player

  # Wayland-specific tools for Niri
  grim # Screenshot tool for Wayland
  slurp # Area selection for screenshots
  swappy # Screenshot annotation tool
  swaylock # Screen locker for Wayland
  swayidle # Idle management daemon
  kanshi # Dynamic display configuration
  wdisplays # GUI display configurator for Wayland
  wev # Wayland event viewer (useful for debugging)
  swaybg # Wallpaper daemon for Wayland
  
  yubikey-agent # Yubikey SSH agent
  pinentry-qt # GPG pinentry

  zathura # PDF viewer
]
