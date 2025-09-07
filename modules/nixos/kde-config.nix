{
  programs.plasma = {
    enable = true;
    shortcuts = {
      # Audio Controls
      "kmix"."decrease_microphone_volume" = "Microphone Volume Down";
      "kmix"."decrease_volume" = ["Meta+Shift+Down" "Volume Down"];
      "kmix"."decrease_volume_small" = "Shift+Volume Down";
      "kmix"."increase_microphone_volume" = "Microphone Volume Up";
      "kmix"."increase_volume" = ["Volume Up" "Meta+Shift+Up"];
      "kmix"."increase_volume_small" = "Shift+Volume Up";
      "kmix"."mic_mute" = ["Microphone Mute" "Meta+Volume Mute"];
      "kmix"."mute" = "Volume Mute";

      # Session Management
      "ksmserver"."Lock Session" = ["Meta+L" "Screensaver"];
      "ksmserver"."Log Out" = "Ctrl+Alt+Del";

      # Window Management (KWin)
      "kwin"."Activate Window Demanding Attention" = "Meta+Ctrl+A";
      "kwin"."Edit Tiles" = "Meta+T";
      "kwin"."Expose" = "Ctrl+F9";
      "kwin"."ExposeAll" = ["Ctrl+F10" "Launch (C)"];
      "kwin"."ExposeClass" = "Ctrl+F7";
      "kwin"."Grid View" = "Meta+G";
      "kwin"."Kill Window" = "Meta+Ctrl+Esc";
      "kwin"."MoveMouseToCenter" = "Meta+F6";
      "kwin"."MoveMouseToFocus" = "Meta+F5";
      "kwin"."Overview" = "Meta+W";

      # Desktop Navigation
      "kwin"."Switch One Desktop Down" = "Meta+Ctrl+Down";
      "kwin"."Switch One Desktop Up" = "Meta+Ctrl+Up";
      "kwin"."Switch One Desktop to the Left" = ["Ctrl+Alt+Left" "Meta+Left"];
      "kwin"."Switch One Desktop to the Right" = ["Meta+Ctrl+Right" "Meta+Right" "Ctrl+Alt+Right"];

      # Window Navigation
      "kwin"."Switch Window Down" = "Meta+Alt+Down";
      "kwin"."Switch Window Left" = "Meta+Alt+Left";
      "kwin"."Switch Window Right" = "Meta+Alt+Right";
      "kwin"."Switch Window Up" = "Meta+Alt+Up";

      # Desktop Switching
      "kwin"."Switch to Desktop 1" = "Ctrl+F1";
      "kwin"."Switch to Desktop 2" = "Ctrl+F2";
      "kwin"."Switch to Desktop 3" = "Ctrl+F3";
      "kwin"."Switch to Desktop 4" = "Ctrl+F4";

      # Window Switching
      "kwin"."Walk Through Windows" = ["Alt+Tab" "Meta+Tab"];
      "kwin"."Walk Through Windows (Reverse)" = ["Alt+Shift+Tab" "Meta+Shift+Tab"];
      "kwin"."Walk Through Windows of Current Application" = ["Alt+`" "Meta+`"];
      "kwin"."Walk Through Windows of Current Application (Reverse)" = ["Alt+~" "Meta+~"];

      # Window Actions
      "kwin"."Window Close" = ["Meta+Backspace" "Alt+F4"];
      "kwin"."Window Maximize" = "Meta+PgUp";
      "kwin"."Window Minimize" = ["Meta+H" "Meta+PgDown"];
      "kwin"."Window Operations Menu" = "Alt+F3";

      # Window Movement Between Desktops
      "kwin"."Window One Desktop Down" = "Meta+Ctrl+Shift+Down";
      "kwin"."Window One Desktop Up" = "Meta+Ctrl+Shift+Up";
      "kwin"."Window One Desktop to the Left" = "Meta+Ctrl+Shift+Left";
      "kwin"."Window One Desktop to the Right" = "Meta+Ctrl+Shift+Right";

      # Window Quick Tiling (disabled by default - uncomment to enable)
      # "kwin"."Window Quick Tile Bottom" = "Meta+Down";
      # "kwin"."Window Quick Tile Left" = "Meta+Left";
      # "kwin"."Window Quick Tile Right" = "Meta+Right";
      # "kwin"."Window Quick Tile Top" = "Meta+Up";

      # Window Movement Between Screens
      "kwin"."Window to Next Desktop" = "Meta+Shift+Right";
      "kwin"."Window to Previous Desktop" = "Meta+Shift+Left";

      # Zoom Controls
      "kwin"."view_actual_size" = "Meta+0";
      "kwin"."view_zoom_in" = ["Meta++" "Meta+="];
      "kwin"."view_zoom_out" = "Meta+-";

      # Media Controls
      "mediacontrol"."nextmedia" = "Media Next";
      "mediacontrol"."pausemedia" = "Media Pause";
      "mediacontrol"."playpausemedia" = "Media Play";
      "mediacontrol"."previousmedia" = "Media Previous";
      "mediacontrol"."stopmedia" = "Media Stop";

      # Power Management
      "org_kde_powerdevil"."Decrease Keyboard Brightness" = "Keyboard Brightness Down";
      "org_kde_powerdevil"."Decrease Screen Brightness" = "Monitor Brightness Down";
      "org_kde_powerdevil"."Decrease Screen Brightness Small" = "Shift+Monitor Brightness Down";
      "org_kde_powerdevil"."Hibernate" = "Hibernate";
      "org_kde_powerdevil"."Increase Keyboard Brightness" = "Keyboard Brightness Up";
      "org_kde_powerdevil"."Increase Screen Brightness" = "Monitor Brightness Up";
      "org_kde_powerdevil"."Increase Screen Brightness Small" = "Shift+Monitor Brightness Up";
      "org_kde_powerdevil"."PowerDown" = "Power Down";
      "org_kde_powerdevil"."PowerOff" = "Power Off";
      "org_kde_powerdevil"."Sleep" = "Sleep";
      "org_kde_powerdevil"."Toggle Keyboard Backlight" = "Keyboard Light On/Off";
      "org_kde_powerdevil"."powerProfile" = "Battery";

      # Plasma Shell
      "plasmashell"."activate application launcher" = ["Meta" "Alt+F1"];
      "plasmashell"."activate task manager entry 1" = "Meta+1";
      "plasmashell"."activate task manager entry 2" = "Meta+2";
      "plasmashell"."activate task manager entry 3" = "Meta+3";
      "plasmashell"."activate task manager entry 4" = "Meta+4";
      "plasmashell"."activate task manager entry 5" = "Meta+5";
      "plasmashell"."activate task manager entry 6" = "Meta+6";
      "plasmashell"."activate task manager entry 7" = "Meta+7";
      "plasmashell"."activate task manager entry 8" = "Meta+8";
      "plasmashell"."activate task manager entry 9" = "Meta+9";

      # Clipboard
      "plasmashell"."clipboard_action" = "Meta+Ctrl+X";
      "plasmashell"."cycle-panels" = "Meta+Alt+P";
      "plasmashell"."show-on-mouse-pos" = "Meta+V";

      # Activities
      "plasmashell"."manage activities" = "Meta+Q";
      "plasmashell"."next activity" = "Meta+A";
      "plasmashell"."previous activity" = "Meta+Shift+A";
      "plasmashell"."stop current activity" = "Meta+S";
      "plasmashell"."show dashboard" = "Ctrl+F12";

      # Application Launchers
      "services/Alacritty.desktop"."New" = "Meta+Return";
      "services/Alacritty.desktop"."_launch" = "Ctrl+Alt+T";
      "services/cider.desktop"."PlayPause" = "Meta+Del";
      "services/cider.desktop"."_launch" = "Meta+M";
      "services/emacsclient.desktop"."new-window" = "Meta+E";
      "services/google-chrome.desktop"."_launch" = "Meta+B";
      "services/org.kde.dolphin.desktop"."_launch" = "Meta+D";
      "services/org.kde.krunner.desktop"."_launch" = "Meta+Space";
      "services/plasma-manager-commands.desktop"."view-cheatsheets" = "Meta+C";
      "services/plasma-manager-commands.desktop"."zeditor" = "Meta+Z";
      
      # Disable conflicting zed.desktop shortcut
      "services/dev.zed.Zed.desktop"."_launch" = "none";
    };

    configFile = {
      # Global settings
      "kdeglobals"."General"."TerminalApplication" = "alacritty";
      "kdeglobals"."General"."TerminalService" = "Alacritty.desktop";
      "kdeglobals"."General"."ColorScheme" = "BreezeDark";
      "kdeglobals"."KDE"."SingleClick" = false;
      "kdeglobals"."KDE"."LookAndFeelPackage" = "org.kde.breezedark.desktop";

      # File dialog settings
      "kdeglobals"."KFileDialog Settings"."Breadcrumb Navigation" = true;
      "kdeglobals"."KFileDialog Settings"."Show Inline Previews" = true;
      "kdeglobals"."KFileDialog Settings"."Show Speedbar" = true;
      "kdeglobals"."KFileDialog Settings"."Sort directories first" = true;
      "kdeglobals"."KFileDialog Settings"."View Style" = "DetailTree";

      # Custom close shortcut
      "kdeglobals"."Shortcuts"."Close" = "Ctrl+Esc";

      # Window manager colors (dark theme)
      "kdeglobals"."WM"."activeBackground" = "39,44,49";
      "kdeglobals"."WM"."activeBlend" = "252,252,252";
      "kdeglobals"."WM"."activeForeground" = "252,252,252";
      "kdeglobals"."WM"."inactiveBackground" = "32,36,40";
      "kdeglobals"."WM"."inactiveBlend" = "161,169,177";
      "kdeglobals"."WM"."inactiveForeground" = "161,169,177";

      # KRunner settings
      "krunnerrc"."General"."FreeFloating" = true;
      "krunnerrc"."General"."historyBehavior" = "Disabled";
      "krunnerrc"."Plugins"."krunner_katesessionsEnabled" = false;
      "krunnerrc"."Plugins"."krunner_konsoleprofilesEnabled" = false;
      "krunnerrc"."Plugins/Favorites"."plugins" = "krunner_sessions,krunner_powerdevil,krunner_services,krunner_systemsettings";

      # Screen lock settings
      "kscreenlockerrc"."Daemon"."Timeout" = 15;
      "kscreenlockerrc"."Greeter"."WallpaperPlugin" = "org.kde.color";
      "kscreenlockerrc"."Greeter/Wallpaper/org.kde.color/General"."Color" = "18,29,37";

      # Session management
      "ksmserverrc"."General"."loginMode" = "emptySession";

      # KWallet settings
      "kwalletrc"."Wallet"."Close When Idle" = false;
      "kwalletrc"."Wallet"."Close on Screensaver" = false;
      "kwalletrc"."Wallet"."Enabled" = true;
      "kwalletrc"."Wallet"."Leave Open" = true;
      "kwalletrc"."Wallet"."Prompt on Open" = false;
      "kwalletrc"."org.freedesktop.secrets"."apiEnabled" = true;

      # Desktop configuration
      "kwinrc"."Desktops"."Number" = 3;
      "kwinrc"."Desktops"."Rows" = 1;

      # Window effects
      "kwinrc"."Effect-diminactive"."Strength" = 5;
      "kwinrc"."Effect-wobblywindows"."ResizeWobble" = false;
      "kwinrc"."NightColor"."Active" = true;
      "kwinrc"."Plugins"."diminactiveEnabled" = true;
      "kwinrc"."Plugins"."sheetEnabled" = true;

      # Desktop change OSD
      "kwinrc"."Script-desktopchangeosd"."TextOnly" = true;

      # Window tiling configuration
      "kwinrc"."Tiling"."padding" = 4;
      "kwinrc"."Tiling"."tiles" = "{\"layoutDirection\":\"horizontal\",\"tiles\":[{\"width\":0.25},{\"width\":0.5},{\"width\":0.25}]}";

      # Window behavior
      "kwinrc"."Windows"."RollOverDesktops" = true;
      "kwinrc"."Xwayland"."Scale" = 1.15;

      # Keyboard layout
      "kxkbrc"."Layout"."Options" = "ctrl:nocaps";
      "kxkbrc"."Layout"."ResetOldOptions" = true;

      # Notifications
      "plasmanotifyrc"."Notifications"."PopupPosition" = "TopCenter";

      # Disable notifications for specific apps
      "plasmanotifyrc"."Applications/discord"."ShowPopups" = false;

      # Input device settings (Logitech MX Master 3)
      "kcminputrc"."Libinput/1133/16514/Logitech MX Master 3"."NaturalScroll" = false;
      "kcminputrc"."Libinput/1133/16514/Logitech MX Master 3"."ScrollFactor" = 3;

      # KDE service settings
      "kded5rc"."Module-browserintegrationreminder"."autoload" = false;
      "kded5rc"."Module-device_automounter"."autoload" = false;

      # Locale settings
      "plasma-localerc"."Formats"."LANG" = "en_US.UTF-8";

      # Trash confirmation
      "kiorc"."Confirmations"."ConfirmEmptyTrash" = false;
    };

    # Panel configuration (felix-style two-panel setup)
    panels = [
      # Main taskbar panel (center bottom) - matches felix alignment=132
      {
        floating = true;
        location = "bottom";
        alignment = "center";
        lengthMode = "fit";
        height = 50;
        hiding = "dodgewindows";
        widgets = [
          {
            kickoff = {
              sortAlphabetically = true;
              icon = "utilities-terminal";
            };
          }
          "org.kde.plasma.marginsseparator"
          {
            iconTasks = {
              appearance = {
                showTooltips = true;
                highlightWindows = true;
                indicateAudioStreams = true;
                fill = true;
              };
              launchers = [
                "preferred://filemanager"
                "preferred://browser"
                "applications:Alacritty.desktop"
                "applications:emacsclient.desktop"
              ];
            };
          }
        ];
      }
      # System tray panel (right side bottom) - matches felix alignment=2
      {
        floating = true;
        location = "bottom";
        alignment = "right";
        lengthMode = "custom";
        minLength = 155;
        maxLength = 429;
        height = 22;
        hiding = "dodgewindows";
        widgets = [
          {
            systemTray.items = {
              shown = [
                "org.kde.plasma.clipboard"
                "org.kde.plasma.volume"
                "org.kde.plasma.brightness"
                "org.kde.plasma.battery"
                "org.kde.plasma.networkmanagement"
                "org.kde.plasma.bluetooth"
              ];
            };
          }
          "org.kde.plasma.digitalclock"
        ];
      }
    ];
  };
}
