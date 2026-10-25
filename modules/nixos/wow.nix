{ pkgs, user, ... }:

# ========================================
# World of Warcraft launcher (no Steam)
# ========================================
# Runs Battle.net through umu-launcher with Steam's Proton 10.0, reusing the
# prefix of the old Steam non-Steam shortcut, and tells Battle.net to start
# WoW right away. Battle.net still has to run: WoW logs in through it.
#
# Don't also launch the Steam shortcut while this is running; two Wine
# sessions in one prefix can corrupt it.

let
  steamDir = "$HOME/.local/share/Steam";
  prefix = "${steamDir}/steamapps/compatdata/3212376421/pfx";

  wow = pkgs.writeShellScriptBin "wow" ''
    export WINEPREFIX="${prefix}"
    export PROTONPATH="${steamDir}/steamapps/common/Proton 10.0"
    export GAMEID=0
    exec ${pkgs.umu-launcher}/bin/umu-run \
      "$WINEPREFIX/drive_c/Program Files (x86)/Battle.net/Battle.net.exe" \
      --exec="launch WoW" "$@"
  '';
in
{
  home-manager.users.${user} = {
    home.packages = [ wow ];

    home.file.".local/share/applications/wow.desktop".text = ''
      [Desktop Entry]
      Name=World of Warcraft
      Comment=Battle.net via umu-launcher, starts WoW directly
      Exec=${wow}/bin/wow
      Type=Application
      Icon=applications-games
      Categories=Game;
    '';
  };
}
