{ pkgs, inputs }:
with pkgs;
let 
  shared-packages = import ../shared/packages.nix { inherit pkgs; }; 
  
  # Custom scripts
  rofi-launcher = pkgs.writeShellScriptBin "rofi-launcher" ''
    # Use kstart5 to ensure proper KDE integration
    ${pkgs.kdePackages.kde-cli-tools}/bin/kstart5 --window "rofi" -- ${pkgs.rofi-wayland}/bin/rofi -show drun
  '';
  
  cheatsheet-viewer = pkgs.writeShellScriptBin "cheatsheet-viewer" ''
    CHEATSHEET_DIR="$HOME/cheatsheets"
    
    # Check if directory exists
    if [ ! -d "$CHEATSHEET_DIR" ]; then
        mkdir -p "$CHEATSHEET_DIR"
        echo "Created cheatsheet directory at $CHEATSHEET_DIR"
        echo "Add some .md files there and run this command again"
        exit 0
    fi
    
    cd "$CHEATSHEET_DIR" || exit 1
    
    # Check if there are any markdown files
    if ! ls *.md >/dev/null 2>&1; then
        echo "No markdown files found in $CHEATSHEET_DIR"
        echo "Add some .md files and try again"
        exit 0
    fi
    
    selected=$(ls *.md 2>/dev/null | sed 's/\.md$//' | \
        ${pkgs.rofi-wayland}/bin/rofi -dmenu -i -p "Cheatsheet")
    
    if [ -z "$selected" ]; then
        exit 0
    fi
    
    # Display with glow in alacritty (positioned and sized by KDE window rules)
    ${pkgs.alacritty}/bin/alacritty --class "cheatsheet-viewer" \
        --title "$selected - Cheatsheet" \
        -e sh -c "${pkgs.glow}/bin/glow -p \"$CHEATSHEET_DIR/''${selected}.md\"; echo; echo 'Press any key to close...'; read -n 1"
  '';
in
shared-packages ++ [

  _1password-gui # Password manager
  
  cider-appimage # Apple Music client
  
  cliphist # Clipboard history manager for Wayland
  
  tableplus-appimage # Database management tool

  bluez # Bluetooth

  brlaser # Printer driver

  chromedriver # Chrome webdriver for testing

  inputs.claude-desktop.packages."${pkgs.system}".claude-desktop-with-fhs

  discord # Voice and text chat

  xclip # Manage clipboard from command line

  wine # Windows compatibility layer
  winetricks # Wine configuration helper
  vulkan-tools # Vulkan utilities
  gamemode # Optimize system performance for games
  wowup-appimage # Game addon manager

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
  
  yubikey-agent # Yubikey SSH agent
  pinentry-qt # GPG pinentry

  zathura # PDF viewer
  
  xwayland # X11 compatibility layer for Wayland

  mariadb # mysql client
  
  # Terminal animations
  cava # Console-based audio visualizer
  asciiquarium # ASCII art aquarium animation
  tty-clock # Terminal digital clock
  
  # Custom scripts
  cheatsheet-viewer

]
