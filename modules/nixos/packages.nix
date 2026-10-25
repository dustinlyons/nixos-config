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

  # Screenshots: drag a region and it is captured on mouse release (-k).
  # shot-copy puts the image on the clipboard; shot-save also asks where to save it.
  shot-copy = pkgs.writeShellScriptBin "shot-copy" ''
    tmp=$(mktemp --suffix=.png -p "''${XDG_RUNTIME_DIR:-/tmp}")
    trap 'rm -f "$tmp"' EXIT
    ${pkgs.kdePackages.spectacle}/bin/spectacle -b -n -r -k -o "$tmp"
    [ -s "$tmp" ] || exit 0  # selection cancelled
    ${pkgs.wl-clipboard}/bin/wl-copy --type image/png < "$tmp"
  '';

  shot-save = pkgs.writeShellScriptBin "shot-save" ''
    # Saved locations; any other directory can be typed into the menu
    locations="$HOME/src/conductly/screenshots
    $HOME/src/atlas-2.0
    $HOME/Pictures/Screenshots
    $HOME/Downloads"

    tmp=$(mktemp --suffix=.png -p "''${XDG_RUNTIME_DIR:-/tmp}")
    trap 'rm -f "$tmp"' EXIT
    ${pkgs.kdePackages.spectacle}/bin/spectacle -b -n -r -k -o "$tmp"
    [ -s "$tmp" ] || exit 0  # selection cancelled

    dir=$(printf '%s\n' "$locations" | sed 's/^ *//' | \
        ${pkgs.rofi}/bin/rofi -dmenu -i -p "Save to")
    [ -n "$dir" ] || exit 0
    dir="''${dir/#\~/$HOME}"

    name=$(${pkgs.rofi}/bin/rofi -dmenu -p "Name (Enter = timestamp)" < /dev/null)
    name="''${name%.png}"
    [ -n "$name" ] || name="screenshot-$(date +'%Y%m%d-%H%M%S')"

    mkdir -p "$dir"
    cp "$tmp" "$dir/$name.png"
    ${pkgs.wl-clipboard}/bin/wl-copy --type image/png < "$tmp"
    ${pkgs.libnotify}/bin/notify-send -a Screenshot "Saved and copied" "$dir/$name.png"
  '';
in
shared-packages ++ [

  _1password-gui # Password manager

] ++ lib.optionals (pkgs ? cider-appimage) [
  obsidian-appimage # Note taking application
  cider-appimage # Apple Music client
] ++ [
  
  cliphist # Clipboard history manager for Wayland

  shot-copy # Region screenshot straight to clipboard
  shot-save # Region screenshot saved to a chosen location

  docker-compose # Docker Compose tool

  ninja # Fast build system

  tableplus-appimage # Database management tool

  bluez # Bluetooth

  brlaser # Printer driver
  
  chromium # Open-source web browser (for Playwright)

  # chromedriver # Chrome webdriver for testing (commented out due to nodejs version conflict)

  #inputs.claude-desktop.packages."${pkgs.system}".claude-desktop-with-fhs

  discord # Voice and text chat

  xclip # Manage clipboard from command line

  wine # Windows compatibility layer
  winetricks # Wine configuration helper
  vulkan-tools # Vulkan utilities
  gamemode # Optimize system performance for games
] ++ lib.optionals (pkgs ? curseforge-appimage) [
  curseforge-appimage # Game addon manager
] ++ [
  gimp # Image editor
  glow # Terminal markdown viewer
  google-chrome # Web browser
  gws # Google Workspace CLI
  firefox # Web browser
  
  hyprpicker # Wayland color picker

  imv # Lightweight Wayland image viewer
  
  keepassxc # Password manager

  qalculate-qt # Powerful calculator with unit conversions and symbolic math

  pavucontrol # Pulse audio controls
  playerctl # Control media players from command line
  playwright-deps # Playwright browser dependencies (from overlay)
  playwright-wrapper # Wrapper script for running Playwright with system Chromium

  qmk # Keyboard firmware toolkit

  showmethekey # Display pressed keys on screen; reads libinput, so it works
               # under Wayland. screenkey, which this replaces, is X11-only and
               # could never have run in this Plasma session.
  simplescreenrecorder # Screen recording tool

  unixtools.ifconfig # Network interface configuration
  unixtools.netstat # Network statistics
  glances # System monitoring tool with style
  
  # Graphics utilities (keep useful ones for system maintenance)
  pciutils # lspci command for hardware identification
  mesa-demos # glxinfo and other OpenGL utilities (useful for troubleshooting)

  vlc # Media player

  # Left over from a Niri setup that never landed -- this machine boots Plasma.
  # swaybg, kanshi and wdisplays speak wlroots-only protocols that KWin does not
  # implement, and Plasma handles wallpaper, idle and display arrangement
  # itself, so swayidle was redundant too. wev is compositor-agnostic and stays.
  wev # Wayland event viewer (useful for debugging keyboard input)
  
  yubikey-agent # Yubikey SSH agent
  pinentry-qt # GPG pinentry

  zathura # PDF viewer
  
  xwayland # X11 compatibility layer for Wayland

  mariadb # mysql client
  newrelic-cli # New Relic CLI
  sentry-cli # Sentry CLI
  
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

]
