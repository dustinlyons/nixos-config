{ pkgs, inputs }:
with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [

  _1password-gui # Password manager
  
  apple-cursor # macOS-style cursor theme
  
  cider-appimage # Apple Music client
  
  cliphist # Clipboard history manager for Wayland
  
  tableplus-appimage # Database management tool

  brlaser # Printer driver

  chromedriver # Chrome webdriver for testing

  inputs.claude-desktop.packages."${pkgs.system}".claude-desktop-with-fhs

  discord # Voice and text chat

  gimp # Image editor
  glow # Terminal markdown viewer
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
  glances # System monitoring tool with style

  vlc # Media player

  # Wayland-specific tools for Niri
  grim # Screenshot tool for Wayland
  slurp # Area selection for screenshots
  swappy # Screenshot annotation tool
  swayidle # Idle management daemon
  kanshi # Dynamic display configuration
  wdisplays # GUI display configurator for Wayland
  wev # Wayland event viewer (useful for debugging)
  swaybg # Wallpaper daemon for Wayland
  
  nautilus # GNOME file browser with excellent Wayland support
  
  yubikey-agent # Yubikey SSH agent
  pinentry-qt # GPG pinentry

  zathura # PDF viewer
  
  xwayland # X11 compatibility layer for Wayland
  
  # Terminal animations
  cava # Console-based audio visualizer
  asciiquarium # ASCII art aquarium animation
  tty-clock # Terminal digital clock
]
