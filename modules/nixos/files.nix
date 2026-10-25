{ user, ... }:

let
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
  xdg_dataHome   = "${home}/.local/share";
  xdg_stateHome  = "${home}/.local/state"; in
{
  "${home}/.npmrc" = {
    text = ''
      prefix=/home/dustin/.npm-packages
    '';
  };

  "${xdg_dataHome}/applications/cheatsheet-viewer.desktop" = {
    text = ''
      [Desktop Entry]
      Name=Cheatsheet Viewer
      Comment=View programming cheatsheets
      Exec=cheatsheet-viewer
      Type=Application
      Icon=accessories-text-editor
      Categories=Utility;Documentation;
    '';
  };

  "${xdg_configHome}/kwinrulesrc" = {
    text = ''
[General]
count=2
rules=fa8dd962-e5d7-4a39-9bea-190a62f25ce2,7c1f0a94-3d6b-4e52-9a08-2b5c41e7d310

[fa8dd962-e5d7-4a39-9bea-190a62f25ce2]
Description=Cheatsheet Viewer Position and Size
position=2351,0
positionrule=3
size=988,1100
sizerule=2
types=1
wmclass=alacritty cheatsheet-viewer
wmclasscomplete=true
wmclassmatch=1

[7c1f0a94-3d6b-4e52-9a08-2b5c41e7d310]
Description=Steam game on ultrawide (DP-3)
acceptfocus=true
acceptfocusrule=2
fsplevel=0
fsplevelrule=2
screen=1
screenrule=2
types=1
wmclass=steam_app_3212376421
wmclasscomplete=false
wmclassmatch=1
    '';
  };
  
}
