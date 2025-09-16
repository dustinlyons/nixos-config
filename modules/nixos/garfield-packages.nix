{ pkgs }:

let
  shared-packages = import ../shared/packages.nix { inherit pkgs; };
in

# NixOS-specific packages for garfield (without gaming/AMD packages)
shared-packages ++ (with pkgs; [

  audacity # Audio editor
  
  brightnessctl # Control screen brightness

  _1password-gui # Password manager

  cliphist # Clipboard history manager for Wayland

  dconf # Configuration system (required for some GNOME apps)

  firefox # Web browser

  discord # Voice and text chat

  xclip # Manage clipboard from command line

  wine # Windows compatibility layer

  glow # Terminal markdown viewer

  imagemagick # Image manipulation toolkit

  iotop # I/O monitoring

  kdePackages.kate # Text editor
  kdePackages.spectacle # Screenshot utility
  kdePackages.kdialog # Dialog boxes

  mpv # Video player

  obsidian # Note-taking app
  kdePackages.okular # PDF viewer

  rofi # Application launcher (includes Wayland support)

  signal-desktop # Secure messaging
  telegram-desktop # Telegram client

  unzip # Archive extraction
  wl-clipboard # Wayland clipboard utilities

  vscode # Code editor

  wget # Download tool
])
