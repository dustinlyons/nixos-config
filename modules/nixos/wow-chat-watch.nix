{ pkgs, lib, user, ... }:

# ========================================
# WoW chat-log watcher
# ========================================
# Notifies on LFG / recruitment messages while WoW is running, and keeps a
# history of everything that matched.
#
# Addons cannot write files, but the game client can: with /chatlog on it
# appends every chat line to Logs/WoWChatLog.txt. The AutoChatLog addon
# (deployed below) turns /chatlog on at every login. wow-chat-watch tails that
# file, runs each message through the regex rules in wow-chat-watch/rules.toml,
# pops a notify-send popup the first time a sender trips a rule (then stays
# quiet for that sender for the rule's cooldown), and appends every match to
# ~/.local/state/wow-chat-watch/matches.jsonl.
#
#   wow-chat-watch show [-f]     past matches (the "app"); -f keeps following
#   wow-chat-watch replay [-v]   dry-run the rules over the whole existing log
#   systemctl --user status wow-chat-watch
#
# Edit rules.toml and `nix run .#build-switch`; the unit references the rules
# file by store path, so home-manager restarts the watcher when they change.

let
  wow-chat-watch = pkgs.writers.writePython3Bin "wow-chat-watch"
    { flakeIgnore = [ "E501" "W503" ]; }  # long lines; W503 contradicts W504
    (builtins.readFile ./wow-chat-watch/wow_chat_watch.py);

  rules = ./wow-chat-watch/rules.toml;

  # Proton prefix for the Steam non-Steam shortcut that launches Battle.net.
  wowDir = ".local/share/Steam/steamapps/compatdata/3212376421/pfx/drive_c/Program Files (x86)/World of Warcraft";
in
{
  home-manager.users.${user} = {
    home.packages = [ wow-chat-watch ];

    home.file = {
      # Two static files; recursive = true makes the AddOns/AutoChatLog directory
      # real and symlinks the .toc and .lua into it, which Wine reads normally.
      "${wowDir}/_anniversary_/Interface/AddOns/AutoChatLog" = {
        source = ./wow-chat-watch/AutoChatLog;
        recursive = true;
      };

      # Same rules the service uses, for manual `replay` / `show` runs.
      ".config/wow-chat-watch/rules.toml".source = rules;

      # Launcher (rofi / app menu) that opens the live match list in a terminal.
      ".local/share/applications/wow-chat-viewer.desktop".text = ''
        [Desktop Entry]
        Name=WoW Chat Viewer
        Comment=Live list of chat messages that matched wow-chat-watch rules
        Exec=${pkgs.alacritty}/bin/alacritty --class wow-chat-viewer --title "WoW Chat" -e ${wow-chat-watch}/bin/wow-chat-watch show --follow
        Type=Application
        Icon=dialog-information
        Categories=Game;Utility;
      '';
    };

    systemd.user.services.wow-chat-watch = {
      Unit = {
        Description = "Watch WoW's chat log and notify on matching messages";
        After = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${wow-chat-watch}/bin/wow-chat-watch --config ${rules} watch";
        Restart = "on-failure";
        RestartSec = 5;
        # notify-send and pw-play
        Environment = [ "PATH=${lib.makeBinPath [ pkgs.libnotify pkgs.pipewire ]}" ];
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
