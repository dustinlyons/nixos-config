{ config, pkgs, lib, ... }:

let
  # === World Buff Fetcher Wrapper ===
  # Handles retries and success checking for the Playwright script
  fetcherWrapper = pkgs.writeShellScriptBin "world-buff-fetcher-wrapper" ''
    #!/bin/bash
    set -e
    
    TIMER_FILE="/home/dustin/.local/share/src/restxp/world-buff-fetcher/wow-buff-timers.json"
    MAX_RETRIES=3
    RETRY_DELAY=300  # 5 minutes in seconds
    
    # Function to check if fetch was successful
    check_success() {
      if [ ! -f "$TIMER_FILE" ]; then
        return 1
      fi
      
      # Check if file has valid buff times (not "Error fetching")
      if grep -q '"Error fetching"' "$TIMER_FILE" 2>/dev/null; then
        return 1
      fi
      
      # Check if file was updated in the last minute
      if [ $(find "$TIMER_FILE" -mmin -1 2>/dev/null | wc -l) -eq 0 ]; then
        return 1
      fi
      
      return 0
    }
    
    # Try to fetch buffs with retries
    for attempt in $(seq 1 $MAX_RETRIES); do
      echo "=== Attempt $attempt of $MAX_RETRIES ==="
      
      # Run the playwright script
      if ${pkgs.playwright-wrapper}/bin/playwright-wrapper ./world-buff-fetcher/run-world-buff-fetcher; then
        # Check if the fetch was actually successful
        if check_success; then
          echo "✓ Successfully fetched buff timers on attempt $attempt"
          exit 0
        else
          echo "⚠ Script ran but fetch failed (attempt $attempt)"
        fi
      else
        echo "⚠ Script failed to run (attempt $attempt)"
      fi
      
      # Wait before retry (except on last attempt)
      if [ $attempt -lt $MAX_RETRIES ]; then
        echo "Waiting $RETRY_DELAY seconds before retry..."
        sleep $RETRY_DELAY
      fi
    done
    
    echo "✗ Failed to fetch buff timers after $MAX_RETRIES attempts"
    exit 1
  '';

  # === Buff Reminder Script ===
  # Checks buff timers and character status, sends notifications
  buffReminderScript = pkgs.writeShellScriptBin "buff-reminder" ''
    #!/bin/bash
    
    # Configuration
    TIMER_FILE="/home/dustin/.local/share/src/restxp/world-buff-fetcher/wow-buff-timers.json"
    SAVED_VARS="/home/dustin/.local/share/Steam/steamapps/compatdata/3212376421/pfx/drive_c/Program Files (x86)/World of Warcraft/_classic_era_/WTF/Account/DEBEO/SavedVariables/RestXPExporter.lua"
    BUFF_WARNING_MINUTES=15
    
    # Check if files exist
    if [ ! -f "$TIMER_FILE" ] || [ ! -f "$SAVED_VARS" ]; then
      exit 0
    fi
    
    # Parse buff timers and check if any are within 15 minutes
    current_time=$(date +%s)
    upcoming_buff=""
    
    # Read buff times from JSON
    while IFS= read -r line; do
      # Extract time strings like "6:00 PM"
      if echo "$line" | grep -qE '"[^"]+": *"[0-9]+:[0-9]+ [AP]M"'; then
        buff_name=$(echo "$line" | sed 's/.*"\([^"]*\)": *".*/\1/')
        time_str=$(echo "$line" | sed 's/.*": *"\([^"]*\)".*/\1/')
        
        # Skip error entries
        if [ "$time_str" = "Error fetching" ]; then
          continue
        fi
        
        # Parse time (e.g., "6:00 PM")
        hour=$(echo "$time_str" | cut -d: -f1)
        minute=$(echo "$time_str" | cut -d: -f2 | cut -d' ' -f1)
        ampm=$(echo "$time_str" | grep -oE '[AP]M')
        
        # Convert to 24-hour format
        if [ "$ampm" = "PM" ] && [ "$hour" != "12" ]; then
          hour=$((hour + 12))
        elif [ "$ampm" = "AM" ] && [ "$hour" = "12" ]; then
          hour=0
        fi
        
        # Get today's date at the buff time
        buff_timestamp=$(date -d "today $hour:$minute" +%s)
        
        # If the time is in the past today, assume it's tomorrow
        if [ $buff_timestamp -lt $current_time ]; then
          buff_timestamp=$(date -d "tomorrow $hour:$minute" +%s)
        fi
        
        # Calculate minutes until buff
        minutes_until=$(( (buff_timestamp - current_time) / 60 ))
        
        # Check if within warning window
        if [ $minutes_until -le $BUFF_WARNING_MINUTES ] && [ $minutes_until -ge 0 ]; then
          if [ -z "$upcoming_buff" ]; then
            upcoming_buff="$buff_name in ''${minutes_until}m"
          else
            upcoming_buff="$upcoming_buff, $buff_name in ''${minutes_until}m"
          fi
        fi
      fi
    done < "$TIMER_FILE"
    
    # Exit if no buffs are coming up
    if [ -z "$upcoming_buff" ]; then
      exit 0
    fi
    
    # Check if any character needs buffs using Python to parse the Lua file
    eval "$(python3 -c "
import re
import sys

try:
    with open('$SAVED_VARS', 'r', encoding='utf-8') as f:
        content = f.read()
    
    match = re.search(r'RestXP_GlobalDataStore\s*=\s*(\{[\s\S]*?\})\s*(?:RestXP_LastChar\s*=|\Z)', content, re.MULTILINE)
    if not match:
        sys.exit(0)
    
    char_blocks = re.finditer(r'\[\"([^\"]+)\"\]\s*=\s*\{([^}]+)\}', match.group(1))
    
    needs_buffs = False
    char_names = []
    
    for block in char_blocks:
        char_name = block.group(1)
        char_data = block.group(2)
        
        level_match = re.search(r'\[\"level\"\]\s*=\s*(\d+)', char_data)
        if not level_match or int(level_match.group(1)) < 60:
            continue
        
        wcb = re.search(r'\[\"wcb_remaining_sec\"\]\s*=\s*(\d+)', char_data)
        rally = re.search(r'\[\"rally_remaining_sec\"\]\s*=\s*(\d+)', char_data)
        soz = re.search(r'\[\"soz_remaining_sec\"\]\s*=\s*(\d+)', char_data)
        
        wcb_val = int(wcb.group(1)) if wcb else 0
        rally_val = int(rally.group(1)) if rally else 0
        soz_val = int(soz.group(1)) if soz else 0
        
        if wcb_val == 0 and rally_val == 0 and soz_val == 0:
            char_names.append(char_name.split('-')[0])
            needs_buffs = True
    
    if needs_buffs:
        print('needs_buffs=true')
        if char_names:
            print(f'char_list=\"{char_names[0]}\"') if len(char_names) == 1 else print(f'char_list=\"{\" and \".join(char_names[:3])}\"')
    
except Exception:
    sys.exit(0)
")" || exit 0
    
    # Show notification if characters need buffs
    if [ "$needs_buffs" = "true" ]; then
      if [ -n "$char_list" ]; then
        ${pkgs.kdePackages.kdialog}/bin/kdialog \
          --title "World Buffs" \
          --passivepopup "World buffs in 15 minutes ($char_list)" 10
      else
        ${pkgs.kdePackages.kdialog}/bin/kdialog \
          --title "World Buffs" \
          --passivepopup "World buffs in 15 minutes" 10
      fi
    fi
  '';
in
{
  # ========================================
  # Development Environment Services
  # ========================================
  # This module manages development environment services and
  # automated WoW-related tasks:
  # 1. Development environments (conductly, river)
  # 2. Fetching world buff timers from hcbuffs.com
  # 3. Sending desktop notifications for upcoming buffs
  
  systemd = {
    # === Development Environment Services ===
    # Automatically start development environments in tmux on login
    
    user.services.conductly-devenv = {
      description = "Start conductly development environment in tmux";
      wantedBy = [ "default.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.writeShellScript "start-conductly" ''
          cd /home/dustin/.local/share/src/conductly
          export TMUX_TMPDIR=/run/user/1000
          ${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-conductly new-session -d -s conductly "${pkgs.nix}/bin/nix develop --impure -c bash -c \"devenv up; exec bash\""
        ''}";
        ExecStop = "${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-conductly kill-session -t conductly";
        RemainAfterExit = "no";
        Environment = [
          "PATH=/run/current-system/sw/bin:/home/dustin/.nix-profile/bin:/etc/profiles/per-user/dustin/bin:/nix/var/nix/profiles/default/bin:/run/wrappers/bin:/usr/bin:/bin"
        ];
      };
    };

    user.services.river-devenv = {
      description = "Start river development environment in tmux";
      wantedBy = [ "default.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "forking";
        ExecStart = "${pkgs.writeShellScript "start-river" ''
          cd /home/dustin/.local/share/src/river
          export TMUX_TMPDIR=/run/user/1000
          ${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-river new-session -d -s river "${pkgs.nix}/bin/nix develop --impure -c bash -c \"devenv up; exec bash\""
        ''}";
        ExecStop = "${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-river kill-session -t river";
        RemainAfterExit = "no";
        Environment = [
          "PATH=/run/current-system/sw/bin:/home/dustin/.nix-profile/bin:/etc/profiles/per-user/dustin/bin:/nix/var/nix/profiles/default/bin:/run/wrappers/bin:/usr/bin:/bin"
        ];
      };
    };

    # === World Buff Services & Bitcoin Noobs Article Generation ===
    services = {
      # Fetches buff timers using Playwright, with retry logic
      world-buff-fetcher = {
        description = "World Buff Fetcher";
        serviceConfig = {
          Type = "oneshot";
          User = "dustin";
          WorkingDirectory = "/home/dustin/.local/share/src/restxp";
          ExecStart = "${fetcherWrapper}/bin/world-buff-fetcher-wrapper";

          # Environment for Node.js and Playwright
          Environment = [
            "HOME=/home/dustin"
            "PATH=${pkgs.nodejs_20}/bin:/run/current-system/sw/bin"
            "NODE_PATH=/home/dustin/.local/share/src/restxp/node_modules"
          ];

          # No automatic restart - wrapper handles retries
          Restart = "no";

          # Timeout accounts for retries (3 attempts * 5 min = 15 min + script time)
          TimeoutStartSec = "20min";

          # Logging
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      # Checks for upcoming buffs and notifies if characters need them
      buff-reminder = {
        description = "World Buff Reminder";
        serviceConfig = {
          Type = "oneshot";
          User = "dustin";
          ExecStart = "${buffReminderScript}/bin/buff-reminder";

          # Environment for GUI notifications
          Environment = [
            "HOME=/home/dustin"
            "DISPLAY=:0"
            "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus"
            "XDG_RUNTIME_DIR=/run/user/1000"
          ];

          # Logging
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      # Automated article generation for crypto content
      bitcoin-noobs-crypto = {
        description = "Bitcoin Noobs Crypto Article Generator";
        after = [ "network.target" ];
        serviceConfig = {
          Type = "oneshot";
          User = "dustin";
          Group = "users";
          WorkingDirectory = "/home/dustin/src/bitcoin-noobs";
          ExecStart = "/usr/bin/python3 /home/dustin/src/bitcoin-noobs/article_generator.py crypto --count 10 --skip-review --skip-integration";
          Environment = [
            "HOME=/home/dustin"
            "USER=dustin"
          ];
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      # Automated article generation for news content
      bitcoin-noobs-news = {
        description = "Bitcoin Noobs News Article Generator";
        after = [ "network.target" ];
        serviceConfig = {
          Type = "oneshot";
          User = "dustin";
          Group = "users";
          WorkingDirectory = "/home/dustin/src/bitcoin-noobs";
          ExecStart = "/usr/bin/python3 /home/dustin/src/bitcoin-noobs/article_generator.py news --auto-discover --skip-review";
          Environment = [
            "HOME=/home/dustin"
            "USER=dustin"
          ];
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };
    };

    timers = {
      # Run fetcher hourly from 6am to 10pm
      world-buff-fetcher = {
        description = "Run Playwright script hourly from 6am to 10pm";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-* 06..22:00:00";  # Every hour from 6am to 10pm
          Persistent = true;                   # Run if missed
          RandomizedDelaySec = "30s";          # Small randomization
        };
      };

      # Check every 15 minutes for upcoming buffs
      buff-reminder = {
        description = "Check for upcoming buffs every 15 minutes";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-* *:00,15,30,45:00";  # Every 15 minutes
          Persistent = true;                        # Run if missed
        };
      };

      # Run crypto article generator daily
      bitcoin-noobs-crypto = {
        description = "Run Bitcoin Noobs Crypto Article Generator Daily";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "daily";
          RandomizedDelaySec = "3600";  # Random delay 0-60 minutes
          Persistent = true;
        };
      };

      # Run news article generator weekly
      bitcoin-noobs-news = {
        description = "Run Bitcoin Noobs News Article Generator Weekly";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "weekly";
          RandomizedDelaySec = "3600";  # Random delay 0-60 minutes
          Persistent = true;
        };
      };
    };
  };
}
