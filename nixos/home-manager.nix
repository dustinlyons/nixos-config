{ pkgs, lib, ... }:

let
  home = builtins.getEnv "HOME";
  common-programs = import ../common/home-manager.nix { pkgs = pkgs; lib = lib; };
  polybar-bars = builtins.readFile ./config/polybar/bars.ini;
  polybar-colors = builtins.readFile ./config/polybar/colors.ini;
  polybar-modules = builtins.readFile ./config/polybar/modules.ini;
  polybar-user_modules = builtins.readFile ./config/polybar/user_modules.ini; in
{
  home = {
    enableNixpkgsReleaseCheck = false;
    packages = pkgs.callPackage ./packages.nix {};
    username = "dustin";
    homeDirectory = "/home/dustin";
    stateVersion = "21.05";
  };

  # Use a dark theme
  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
  };

  # Screen lock
  services.screen-locker = {
    enable = true;
    inactiveInterval = 30;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dim";
    xautolock.extraOptions = [
      "Xautolock.killer: systemctl suspend"
    ];
  };

  # Auto mount devices
  services.udiskie.enable = true;

  services.polybar = {
    enable = true;
    config = ./config/polybar/config.ini;
    extraConfig = polybar-bars + polybar-colors + polybar-modules + polybar-user_modules;
    package = pkgs.polybarFull;
    script = "polybar main &";
  };

  services.dunst = {
    enable = true;
    package = pkgs.dunst;
    settings = {
      global = {
      monitor = 0;
      follow = "mouse";
      border = 0;
      height = 400;
      width = 320;
      offset = "33x65";
      indicate_hidden = "yes";
      shrink = "no";
      separator_height = 0;
      padding = 32;
      horizontal_padding = 32;
      frame_width = 0;
      sort = "no";
      idle_threshold = 120;
      font = "Noto Sans";
      line_height = 4;
      markup = "full";
      format = "<b>%s</b>\n%b";
      alignment = "left";
      transparency = 10;
      show_age_threshold = 60;
      word_wrap = "yes";
      ignore_newline = "no";
      stack_duplicates = false;
      hide_duplicate_count = "yes";
      show_indicators = "no";
      icon_position = "left";
      icon_theme = "Adwaita-dark";
      sticky_history = "yes";
      history_length = 20;
      history = "ctrl+grave";
      browser = "google-chrome-stable";
      always_run_script = true;
      title = "Dunst";
      class = "Dunst";
      max_icon_size = 64;
    };
    };
  };

  programs = common-programs // {};

}
