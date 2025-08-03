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

  "${xdg_configHome}/swappy/config" = {
    text = ''
      [Default]
      save_dir=$HOME/Pictures/Screenshots
      save_filename_format=screenshot-%Y%m%d-%H%M%S.png
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
  
}
