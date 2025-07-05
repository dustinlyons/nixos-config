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
      size = 24;
    };
  };

  home = {
    enableNixpkgsReleaseCheck = false;
    username = "${user}";
    homeDirectory = "/home/${user}";
    packages = pkgs.callPackage ./packages.nix { inherit inputs; };
    file = shared-files // import ./files.nix { inherit user pkgs; };
    stateVersion = "25.05";
    
  };

  programs = shared-programs // { 
    gpg.enable = true;
    
    # Niri Wayland compositor configuration
    niri.config = ''
      cursor {
        xcursor-theme "macOS-Monterey"
        xcursor-size 24
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
        Ctrl+Alt+B { spawn "google-chrome-stable" "--ozone-platform=wayland"; }
        Ctrl+Alt+E { spawn "/home/dustin/.local/bin/emacsclient-gui"; }
        
        // Essential bindings
        Mod+Return { spawn "${pkgs.alacritty}/bin/alacritty"; }
        Mod+Space { spawn "${pkgs.bemenu}/bin/bemenu-run" "--fn" "MesloLGS NF 12" "--tb" "#1f2528" "--tf" "#c0c5ce" "--fb" "#1f2528" "--ff" "#c0c5ce" "--nb" "#1f2528" "--nf" "#65737e" "--hb" "#6699cc" "--hf" "#1f2528"; }
        
        // Clipboard history
        Mod+V { spawn "bash" "-c" "${pkgs.cliphist}/bin/cliphist list | ${pkgs.bemenu}/bin/bemenu -l 10 | ${pkgs.cliphist}/bin/cliphist decode | ${pkgs.wl-clipboard}/bin/wl-copy"; }
        
        // Screen lock
        Ctrl+Alt+L { spawn "${pkgs.swaylock}/bin/swaylock" "-c" "000000"; }
        Mod+Shift+E { quit; }
        Mod+Q { close-window; }
        Alt+F4 { close-window; }
        
        // Screenshots
        Print { spawn "bash" "-c" "${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\" - | ${pkgs.swappy}/bin/swappy -f -"; }
        Ctrl+Print { spawn "bash" "-c" "${pkgs.grim}/bin/grim - | ${pkgs.swappy}/bin/swappy -f -"; }
        Alt+Print { screenshot-window; }
        
        // Focus movement (vim-like)
        Mod+H { focus-column-left; }
        Mod+L { focus-column-right; }
        Mod+K { focus-window-up; }
        Mod+J { focus-window-down; }
        
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
        
        // Volume controls
        XF86AudioRaiseVolume { spawn "${pkgs.wireplumber}/bin/wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+"; }
        XF86AudioLowerVolume { spawn "${pkgs.wireplumber}/bin/wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"; }
        XF86AudioMute { spawn "${pkgs.wireplumber}/bin/wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"; }
        
        // Enhanced navigation and window management
        Mod+Tab { focus-column-right; }
        Mod+Shift+Tab { focus-column-left; }
        
        // Window sizing presets (cycle through widths)
        Mod+Ctrl+R { switch-preset-column-width; }
        
        // Lock screen
        Mod+Ctrl+Q { spawn "${pkgs.swaylock}/bin/swaylock"; }
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
          
          modules-left = ["custom/niri_workspaces" "custom/weather"];
          modules-center = ["clock"];
          modules-right = ["custom/notifications" "pulseaudio" "network" "cpu" "memory"];

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
            timezone = "America/Kentucky/Louisville";
            tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
            format-alt = "{:%Y-%m-%d}";
          };
          
          cpu = {
            format = " {usage}%";
            tooltip = false;
          };
          
          memory = {
            format = " {}%";
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
          font-family: MesloLGS NF, FontAwesome, sans-serif;
          font-size: 13px;
          border: none;
          border-radius: 0;
        }
        
        window#waybar {
          background: linear-gradient(135deg, rgba(40, 42, 54, 0.7) 0%, rgba(68, 71, 90, 0.7) 100%);
          color: #f8f8f2;
          border-bottom: 1px solid rgba(189, 147, 249, 0.3);
          box-shadow: 0 2px 20px rgba(0, 0, 0, 0.3);
        }
        
        #workspaces button {
          padding: 0 12px;
          background-color: transparent;
          color: #f8f8f2;
          margin: 0 2px;
          border-radius: 6px;
          transition: all 0.3s ease;
        }
        
        #workspaces button:hover {
          background: rgba(189, 147, 249, 0.2);
          color: #bd93f9;
        }
        
        #workspaces button.active {
          background: linear-gradient(135deg, #bd93f9 0%, #ff79c6 100%);
          color: #282a36;
          font-weight: bold;
        }
        
        #clock,
        #battery,
        #cpu,
        #memory,
        #network,
        #pulseaudio,
        #tray,
        #custom-niri,
        #custom-weather,
        #custom-music,
        #custom-notifications,
        #custom-power {
          padding: 0 12px;
          margin: 0;
          color: #f8f8f2;
          transition: all 0.3s ease;
        }
        
        #custom-niri,
        #custom-power {
          font-size: 16px;
        }
        
        #custom-music {
          color: #50fa7b;
        }
        
        #custom-weather {
          color: #8be9fd;
        }
        
        #custom-notifications {
          color: #ffb86c;
        }
        
        #battery.charging {
          color: #99c794;
        }
        
        #battery.critical:not(.charging) {
          color: #ec5f67;
        }
        
        #network.disconnected {
          color: #ec5f67;
        }
        
        #pulseaudio.muted {
          color: #65737e;
        }
      '';
    };
    
    # Application launcher
    fuzzel = {
      enable = true;
      settings = {
        main = {
          font = "MesloLGS NF:size=14";
          dpi-aware = "yes";
          width = 35;
          lines = 10;
          tabs = 4;
        };
        colors = {
          background = "1f2528dd";
          text = "c0c5ceff";
          match = "6699ccff";
          selection = "65737eff";
          selection-text = "ffffffff";
          border = "6699ccff";
        };
      };
    };
  };
  
  # Notification daemon
  services.mako = {
    enable = true;
    settings = {
      background-color = "#1f2528";
      text-color = "#c0c5ce";
      border-color = "#6699cc";
      border-size = 2;
      border-radius = 5;
      default-timeout = 5000;
      font = "MesloLGS NF 10";
      padding = "10";
      margin = "10";
      layer = "overlay";
    };
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
