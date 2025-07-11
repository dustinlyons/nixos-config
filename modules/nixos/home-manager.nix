{ config, pkgs, lib, inputs, ... }:

let
  user = "dustin";
  xdg_configHome  = "/home/${user}/.config";
  shared-programs = import ../shared/home-manager.nix { inherit config pkgs lib; };
  shared-files = import ../shared/files.nix { inherit config pkgs; };

  # These files are generated when secrets are decrypted at build time
  gpgKeys = [
    "/home/${user}/.ssh/pgp_github.key"
    "/home/${user}/.ssh/pgp_github.pub"
  ];
in
{
  gtk = {
    enable = true;
    cursorTheme = {
      package = pkgs.apple-cursor;
      name = "macOS-Monterey";
      size = 12;
    };
  };

  home = {
    enableNixpkgsReleaseCheck = false;
    username = "${user}";
    homeDirectory = "/home/${user}";
    packages = pkgs.callPackage ./packages.nix { inherit inputs; };
    file = shared-files // import ./files.nix { inherit user pkgs; };
    stateVersion = "25.05";
    
    sessionVariables = {
      XCURSOR_SIZE = "12";
      XCURSOR_THEME = "macOS-Monterey";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };
  };

  programs = shared-programs // { 
    gpg.enable = true;
    
    # Niri Wayland compositor configuration
    niri.config = ''
      cursor {
        xcursor-theme "macOS-Monterey"
        xcursor-size 12
        hide-when-typing
      }
      
      input {
        keyboard {
          xkb {
            layout "us"
            options "ctrl:nocaps"
          }
        }
        
        touchpad {
          tap
          natural-scroll
        }
      }
      
      binds {
        // Custom keybindings
        Ctrl+Alt+T { spawn "${pkgs.alacritty}/bin/alacritty"; }
        Mod+B { spawn "google-chrome-stable" "--ozone-platform=wayland"; }
        Mod+E { spawn "/home/dustin/.local/bin/emacsclient-gui"; }
        
        // Essential bindings
        Mod+Return { spawn "${pkgs.alacritty}/bin/alacritty" "--class" "floating-terminal"; }
        Mod+T { spawn "${pkgs.alacritty}/bin/alacritty"; }
        Mod+Space { spawn "${pkgs.fuzzel}/bin/fuzzel"; }
        
        // Clipboard history
        Mod+V { spawn "bash" "-c" "${pkgs.cliphist}/bin/cliphist list | ${pkgs.fuzzel}/bin/fuzzel --dmenu -l 10 | ${pkgs.cliphist}/bin/cliphist decode | ${pkgs.wl-clipboard}/bin/wl-copy"; }
        
        // Color picker
        Mod+C { spawn "bash" "-c" "${pkgs.hyprpicker}/bin/hyprpicker -a"; }
        
        Mod+Shift+E { quit; }
        Mod+Q { close-window; }
        Alt+F4 { close-window; }
        
        // Toggle floating
        Mod+W { toggle-window-floating; }
        
        // Screenshots
        Print { spawn "bash" "-c" "${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\" - | ${pkgs.swappy}/bin/swappy -f -"; }
        Ctrl+Print { spawn "bash" "-c" "${pkgs.grim}/bin/grim - | ${pkgs.swappy}/bin/swappy -f -"; }
        Alt+Print { screenshot-window; }
        
        // Focus movement (vim-like)
        Mod+H { focus-column-left; }
        Mod+L { focus-column-right; }
        Mod+K { focus-window-up; }
        Mod+J { focus-window-down; }
        
        // Additional focus movement bindings
        Ctrl+Alt+Left { focus-column-left; }
        Ctrl+Alt+Right { focus-column-right; }
        Mod+Left { focus-column-left; }
        Mod+Right { focus-column-right; }
        
        // Window movement
        Mod+Shift+H { move-column-left; }
        Mod+Shift+L { move-column-right; }
        Mod+Shift+K { move-window-up; }
        Mod+Shift+J { move-window-down; }
        
        // Workspace management
        Mod+1 { focus-workspace 1; }
        Mod+2 { focus-workspace 2; }
        Mod+3 { focus-workspace 3; }
        Mod+4 { focus-workspace 4; }
        Mod+5 { focus-workspace 5; }
        Mod+6 { focus-workspace 6; }
        
        // Navigate workspaces with arrow keys
        Mod+Up { focus-workspace-up; }
        Mod+Down { focus-workspace-down; }
        Ctrl+Alt+Up { focus-workspace-up; }
        Ctrl+Alt+Down { focus-workspace-down; }
        
        // Move windows between workspaces with arrow keys
        Mod+Shift+Up { move-column-to-workspace-up; }
        Mod+Shift+Down { move-column-to-workspace-down; }
        
        Mod+Shift+1 { move-column-to-workspace 1; }
        Mod+Shift+2 { move-column-to-workspace 2; }
        Mod+Shift+3 { move-column-to-workspace 3; }
        Mod+Shift+4 { move-column-to-workspace 4; }
        Mod+Shift+5 { move-column-to-workspace 5; }
        Mod+Shift+6 { move-column-to-workspace 6; }
        
        // Window controls
        Mod+F { maximize-column; }
        Mod+Shift+F { fullscreen-window; }
        Mod+R { switch-preset-column-width; }
        Mod+Shift+R { reset-window-height; }
        
        // Floating window toggle
        Mod+Shift+W { toggle-window-floating; }
        
        // Volume controls
        XF86AudioRaiseVolume { spawn "${pkgs.wireplumber}/bin/wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+"; }
        XF86AudioLowerVolume { spawn "${pkgs.wireplumber}/bin/wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"; }
        XF86AudioMute { spawn "${pkgs.wireplumber}/bin/wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
        
        // Media controls
        Mod+Shift+P { spawn "${pkgs.playerctl}/bin/playerctl" "play-pause"; }
        Mod+Shift+N { spawn "${pkgs.playerctl}/bin/playerctl" "next"; }
        Mod+Shift+B { spawn "${pkgs.playerctl}/bin/playerctl" "previous"; }
        
        // Enhanced navigation and window management
        Mod+Tab { focus-column-right; }
        Mod+Shift+Tab { focus-column-left; }
        
        // Window sizing presets (cycle through widths)
        Mod+Ctrl+R { switch-preset-column-width; }
        
        
        // Terminal animations
        Mod+F1 { spawn "${pkgs.alacritty}/bin/alacritty" "--class" "fullscreen-animation" "-e" "${pkgs.asciiquarium}/bin/asciiquarium"; }
        Mod+F2 { spawn "${pkgs.alacritty}/bin/alacritty" "--class" "fullscreen-animation" "-e" "${pkgs.tty-clock}/bin/tty-clock" "-C" "6" "-c"; }
        Mod+F3 { spawn "${pkgs.alacritty}/bin/alacritty" "--class" "fullscreen-animation" "-e" "${pkgs.cava}/bin/cava"; }
        
        // Show keybindings help
        Mod+Shift+Slash { show-hotkey-overlay; }
        
        // Focus all windows including floating
        Mod+I { focus-window-or-workspace-up; }
        Mod+O { focus-window-or-workspace-down; }
        
        // Launch Cider
        Mod+P { spawn "cider"; }
        
        // Column width management
        Mod+Minus { set-column-width "-10%"; }
        Mod+Equal { set-column-width "+10%"; }
        
        // Window height management
        Mod+Shift+Minus { set-window-height "-10%"; }
        Mod+Shift+Equal { set-window-height "+10%"; }
        
        // Monitor focus
        Mod+Shift+Left { focus-monitor-left; }
        Mod+Shift+Right { focus-monitor-right; }
        
        // Move to monitor
        Mod+Ctrl+Shift+Left { move-column-to-monitor-left; }
        Mod+Ctrl+Shift+Right { move-column-to-monitor-right; }
        
        // Consume window into column
        Mod+Comma { consume-window-into-column; }
        
        // Expel window from column
        Mod+Period { expel-window-from-column; }
        
        // Center column
        Mod+Shift+C { center-column; }
        
        // Focus specific column
        Mod+Ctrl+H { focus-column-first; }
        Mod+Ctrl+L { focus-column-last; }
      }
      
      layout {
        gaps 16
        center-focused-column "never"
        
        struts {
          left 8
          right 8
          top 8
          bottom 8
        }
        
        preset-column-widths {
          proportion 0.33333
          proportion 0.5
          proportion 0.66667
        }
        
        default-column-width { proportion 0.5; }
        
        border { off; }
        
        focus-ring { off; }
        
        shadow {
          on
          softness 20
          spread 0
          offset x=0 y=10
          draw-behind-window true
          color "#000000CC"  // CC = 80% opacity in hex
        }
        
      }
      
      window-rule {
        match app-id="google-chrome"
        default-column-width { proportion 0.66667; }
      }
      
      window-rule {
        match app-id="Alacritty"
        default-column-width { proportion 0.5; }
      }
      
      window-rule {
        match app-id="floating-terminal"
        open-floating true
      }
      
      window-rule {
        match app-id="fullscreen-animation"
        open-fullscreen true
        open-on-workspace "next"
      }
      
      spawn-at-startup "${pkgs.waybar}/bin/waybar"
      spawn-at-startup "${pkgs.mako}/bin/mako"
      spawn-at-startup "${pkgs.swaybg}/bin/swaybg" "-i" "/home/${user}/Pictures/space-goose.jpg" "-m" "fill"
      spawn-at-startup "bash" "-c" "${pkgs.wl-clipboard}/bin/wl-paste --watch ${pkgs.cliphist}/bin/cliphist store"
    '';
    
    # Waybar status bar
    waybar = {
      enable = true;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 30;
          spacing = 4;
          
          modules-left = ["custom/niri_workspaces"];
          modules-center = [];
          modules-right = ["custom/media" "pulseaudio" "custom/weather" "clock"];

          "custom/niri_workspaces" = {
            format = "{}";
            interval =  2;
            return-type = "json";
            exec = "$HOME/.config/waybar/modules/workspaces.sh \"$WAYBAR_OUTPUT_NAME\"";
            on-click = "fuzzel";
            on-scroll-up = "$HOME/.config/waybar/modules/workspaces.sh up";
            on-scroll-down = "$HOME/.config/waybar/modules/workspaces.sh down";
            signal = 8;
          };
          
          "custom/weather" = {
            format = "{}";
            exec = "curl -s 'wttr.in/Louisville?format=1' | sed 's/+//g'";
            interval = 1800;
            tooltip = false;
          };
          
          "custom/music" = {
            format = " {}";
            exec = "${pkgs.playerctl}/bin/playerctl metadata --format '{{ artist }} - {{ title }}' 2>/dev/null || echo 'No music playing'";
            interval = 5;
            max-length = 50;
            on-click = "${pkgs.playerctl}/bin/playerctl play-pause";
          };
          
          "custom/notifications" = {
            format = " {}";
            exec = "echo $(${pkgs.mako}/bin/makoctl list | wc -l)";
            interval = 5;
            on-click = "${pkgs.mako}/bin/makoctl dismiss-all";
          };
          
          "custom/power" = {
            format = "⏻";
            tooltip = false;
            on-click = "${pkgs.fuzzel}/bin/fuzzel -d";
          };
          
          "wlr/workspaces" = {
            format = "{icon}";
            on-click = "activate";
            format-icons = {
              active = "";
              default = "";
            };
          };
          
          clock = {
            format = "{:%I:%M %p}";
            timezone = "America/Kentucky/Louisville";
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
            format-alt = "{:%A, %B %d, %Y}";
          };
          
          cpu = {
            format = " {usage}%";
            tooltip = false;
            states = {
              warning = 80;
            };
          };
          
          memory = {
            format = " {}%";
            states = {
              warning = 80;
            };
          };
          
          battery = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = "{icon} {capacity}%";
            format-charging = " {capacity}%";
            format-plugged = " {capacity}%";
            format-icons = ["" "" "" "" ""];
          };
          
          network = {
            format-wifi = " {signalStrength}%";
            format-ethernet = " {ipaddr}";
            tooltip-format = "{ifname} via {gwaddr}";
            format-linked = " {ifname} (No IP)";
            format-disconnected = "Disconnected ⚠";
          };
          
          pulseaudio = {
            format = "{icon} {volume}%";
            format-muted = " {volume}%";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = ["" "" ""];
            };
            on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
          };
          
          tray = {
            spacing = 10;
          };
        };
      };
      
      style = ''
        * {
          font-family: "Inter", "SF Pro Display", -apple-system, BlinkMacSystemFont, sans-serif;
          font-size: 14px;
          font-weight: 500;
          border: none;
          border-radius: 0;
        }
        
        window#waybar {
          background: rgba(30, 30, 30, 0.85);
          color: #ffffff;
          border-bottom: 1px solid rgba(255, 255, 255, 0.1);
        }
        
        #custom-niri_workspaces {
          padding: 0 16px;
          font-weight: 600;
          font-family: "JetBrainsMono", monospace;
        }
        
        #clock {
          padding: 0 16px;
          font-weight: 600;
        }
        
        #custom-weather {
          padding: 0 16px;
          margin-right: 8px;
          opacity: 0.8;
        }
        
        #custom-media {
          padding: 0 16px;
          color: #6699cc;
          font-style: italic;
        }
        
        #tray {
          padding: 0 12px;
        }
        
        #tray > .passive {
          -gtk-icon-effect: dim;
        }
        
        #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: #eb4d4b;
        }
        
        #cpu,
        #memory {
          padding: 0 16px;
          color: #ffffff;
        }
        
        #cpu.warning,
        #memory.warning {
          color: #ff6666;
        }
      '';
    };
    
    # Application launcher
    fuzzel = {
      enable = true;
      settings = {
        main = {
          font = "Inter:size=14";
          dpi-aware = "yes";
          width = 40;
          lines = 12;
          tabs = 4;
          inner-pad = 16;
          horizontal-pad = 16;
          vertical-pad = 16;
        };
        colors = {
          background = "2a2a2aee";
          text = "ffffffff";
          match = "888888ff";
          selection = "404040ff";
          selection-text = "ffffffff";
          border = "00000033";
        };
        border = {
          width = 1;
          radius = 8;
        };
      };
    };
  };
  
  # Notification daemon
  services.mako = {
    enable = true;
    settings = {
      background-color = "#2a2a2a";
      text-color = "#ffffff";
      border-color = "#00000033";
      border-size = 1;
      border-radius = 8;
      default-timeout = 5000;
      font = "Inter 12";
      padding = "16";
      margin = "16";
      layer = "overlay";
    };
  };

  # Idle management for monitor sleep
  services.swayidle = {
    enable = true;
    events = [];
    timeouts = [
      {
        timeout = 300;  # 5 minutes
        command = "${pkgs.niri}/bin/niri msg action power-off-monitors";
        resumeCommand = "${pkgs.niri}/bin/niri msg action power-on-monitors";
      }
    ];
  };

  # This installs my GPG signing keys for Github
  systemd.user.services.gpg-import-keys = {
    Unit = {
      Description = "Import gpg keys";
      After = [ "gpg-agent.socket" ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = toString (pkgs.writeScript "gpg-import-keys" ''
        #! ${pkgs.runtimeShell} -el
        ${lib.optionalString (gpgKeys != []) ''
        ${pkgs.gnupg}/bin/gpg --import ${lib.concatStringsSep " " gpgKeys}
        ''}
      '');
    };

    Install = { WantedBy = [ "default.target" ]; };
  };
}
