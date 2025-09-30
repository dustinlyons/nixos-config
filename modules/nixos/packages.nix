{ pkgs, inputs, config ? null }:
with pkgs;
let
  shared-packages = import ../shared/packages.nix { inherit pkgs; };
  hostname = if config != null then (config.networking.hostName or "") else "";

  # Custom scripts
  rofi-launcher = pkgs.writeShellScriptBin "rofi-launcher" ''
    # Use kstart5 to ensure proper KDE integration
    ${pkgs.kdePackages.kde-cli-tools}/bin/kstart5 --window "rofi" -- ${pkgs.rofi}/bin/rofi -show drun
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
        ${pkgs.rofi}/bin/rofi -dmenu -i -p "Cheatsheet")

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

] ++ lib.optionals (pkgs ? cider-appimage) [
  cider-appimage # Apple Music client
] ++ [
  
  cliphist # Clipboard history manager for Wayland
  
  tableplus-appimage # Database management tool

  bluez # Bluetooth

  brlaser # Printer driver
  
  chromium # Open-source web browser (for Playwright)

  # chromedriver # Chrome webdriver for testing (commented out due to nodejs version conflict)

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
  firefox # Web browser
  
  hyprpicker # Wayland color picker

  imv # Lightweight Wayland image viewer
  
  keepassxc # Password manager

  pavucontrol # Pulse audio controls
  playerctl # Control media players from command line
  playwright-deps # Playwright browser dependencies (from overlay)
  playwright-wrapper # Wrapper script for running Playwright with system Chromium

  qmk # Keyboard firmware toolkit

  screenkey # Display pressed keys on screen
  simplescreenrecorder # Screen recording tool

  unixtools.ifconfig # Network interface configuration
  unixtools.netstat # Network statistics
  glances # System monitoring tool with style
  
  # Graphics utilities (keep useful ones for system maintenance)
  pciutils # lspci command for hardware identification
  mesa-demos # glxinfo and other OpenGL utilities (useful for troubleshooting)

  vlc # Media player

  # Wayland-specific tools for Niri
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
  rofi-launcher
  cheatsheet-viewer
  
  # Additional KDE tools and notifications
  libnotify # Notification library and notify-send command
  kdePackages.kdialog # KDE dialog boxes and notifications
  
  # Application launcher
  rofi # Application launcher and window switcher (includes Wayland support)
  linuxKernel.packages.linux_zen.xone

]
