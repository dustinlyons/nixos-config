{ pkgs, user, ... }:

# ========================================
# World of Warcraft launcher (no Steam)
# ========================================
# Runs Battle.net through umu-launcher with GE-Proton, reusing the prefix of
# the Steam non-Steam shortcut, and tells Battle.net to start WoW right away.
# Battle.net still has to run: WoW logs in through it. Keep the Proton here
# matching the shortcut's; switching versions on one prefix upgrades or
# downgrades it each time.
#
# If Battle.net is already running, the launcher does not start a second
# copy: two Wine sessions in one prefix can corrupt it. It sends a desktop
# notification instead. For the same reason, don't also launch the Steam
# shortcut while this is running. If neither is running but an earlier
# session left its Wine server or wrappers behind, the launcher shuts those
# down first.
#
# The icon is the WoW Forever icon, extracted from _forever_/Wow.exe with
# icoutils (wrestool -x -t 14, then icotool -x -i 6 for the 256px image).

let
  steamDir = "$HOME/.local/share/Steam";
  prefix = "${steamDir}/steamapps/compatdata/3212376421/pfx";

  wow = pkgs.writeShellScriptBin "wow" ''
    notify() {
      ${pkgs.libnotify}/bin/notify-send --app-name="World of Warcraft" \
        --icon=wow-forever "World of Warcraft" "$1"
    }

    # Match the process name, not the full command line (-f): the umu/proton
    # wrappers carry "Battle.net.exe --exec=launch WoW" in their arguments and
    # can outlive Battle.net, which made this report a game that wasn't running.
    # Wine sets each process name to its .exe basename. pgrep rejects name
    # patterns over 15 characters, so the dots are left unescaped.
    if ${pkgs.procps}/bin/pgrep -xi 'wowb?\.exe' >/dev/null; then
      notify "WoW is already running."
      exit 0
    fi
    if ${pkgs.procps}/bin/pgrep -xi 'battle.net.exe' >/dev/null; then
      notify "Battle.net is already running. Press Play in its window."
      exit 0
    fi

    export WINEPREFIX="${prefix}"
    export PROTONPATH="${pkgs.proton-ge-bin.steamcompattool}"
    export GAMEID=0

    # Neither game nor launcher is running, but an earlier session can leave
    # its Wine server and umu/proton wrappers behind with the prefix still
    # open. Shut them down so the new launch doesn't share the prefix.
    wrappers='Battle\.net/Battle\.net\.exe'
    leftover=0
    # Exits 0 if it stopped a server for this prefix, 1 if none was running.
    if "$PROTONPATH/files/bin/wineserver" -k 2>/dev/null; then
      leftover=1
    fi
    if ${pkgs.procps}/bin/pkill -f "$wrappers"; then
      leftover=1
      for _ in $(seq 20); do
        ${pkgs.procps}/bin/pgrep -f "$wrappers" >/dev/null || break
        sleep 0.5
      done
      ${pkgs.procps}/bin/pkill -KILL -f "$wrappers" || true
    fi
    if [ "$leftover" = 1 ]; then
      notify "Closed a leftover Battle.net session. Starting WoW."
    fi
    exec ${pkgs.umu-launcher}/bin/umu-run \
      "$WINEPREFIX/drive_c/Program Files (x86)/Battle.net/Battle.net.exe" \
      --exec="launch WoW" "$@"
  '';
in
{
  home-manager.users.${user} = {
    home.packages = [ wow ];

    home.file.".local/share/icons/hicolor/256x256/apps/wow-forever.png".source =
      ./wow-forever.png;

    home.file.".local/share/applications/wow.desktop".text = ''
      [Desktop Entry]
      Name=World of Warcraft
      Comment=Battle.net via umu-launcher, starts WoW directly
      Exec=${wow}/bin/wow
      Type=Application
      Icon=wow-forever
      Categories=Game;
    '';
  };
}
