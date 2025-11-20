{ config, pkgs, lib, ... }:
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
