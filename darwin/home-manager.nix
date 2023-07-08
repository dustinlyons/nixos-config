{ config, pkgs, lib, ... }:

let
  common-programs = import ../common/home-manager.nix { config = config; pkgs = pkgs; lib = lib; };
  common-files = import ../common/files.nix {};
  user = "dustin"; in
{
  imports = [
    <home-manager/nix-darwin>
   ./dock
  ];

  # It me
  users.users.${user} = {
    name = "${user}";
    home = "/Users/${user}";
    isHidden = false;
    shell = pkgs.zsh;
  };

  # Fully declarative dock using the latest from Nix Store
  local.dock.enable = true;
  local.dock.entries = [
    { path = "/Applications/Slack.app/"; }
    { path = "/System/Applications/Messages.app/"; }
    { path = "/System/Applications/Facetime.app/"; }
    { path = "/Applications/Telegram.app/"; }
    { path = "${pkgs.alacritty}/Applications/Alacritty.app/"; }
    { path = "/Applications/Discord.app/"; }
    { path = "/System/Applications/Music.app/"; }
    { path = "/System/Applications/News.app/"; }
    { path = "/System/Applications/Photos.app/"; }
    { path = "/System/Applications/Photo Booth.app/"; }
    { path = "/Applications/Drafts.app/"; }
    { path = "/System/Applications/Home.app/"; }
    {
      path = "${config.users.users.${user}.home}/.local/share/bin/emacs-launcher.command";
      section = "others";
    }
    {
      path = "${config.users.users.${user}.home}/.local/share/";
      section = "others";
      options = "--sort name --view grid --display folder";
    }
    {
      path = "${config.users.users.${user}.home}/.local/share/downloads";
      section = "others";
      options = "--sort name --view grid --display stack";
    }
  ];

  # We use Homebrew to install impure software only (Mac Apps)
  homebrew.enable = true;
  homebrew.onActivation = {
    autoUpdate = true;
    cleanup = "zap";
    upgrade = true;
  };
  homebrew.brewPrefix = "/opt/homebrew/bin";

  # These app IDs are from using the mas CLI app
  # mas = mac app store
  # https://github.com/mas-cli/mas
  #
  # $ mas search <app name>
  #
  homebrew.casks = pkgs.callPackage ./casks.nix {};
  homebrew.masApps = {
    "1password" = 1333542190;
    "drafts" = 1435957248;
    "hidden-bar" = 1452453066;
    "yoink" = 457622435;
  };

  # Enable home-manager to manage the XDG standard
  home-manager = {
    useGlobalPkgs = true;
    users.${user} = {
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix {};
      home.file = common-files // import ./files.nix { config = config; pkgs = pkgs; };
      programs = common-programs // {};

      # https://github.com/nix-community/home-manager/issues/3344
      # Marked broken Oct 20, 2022 check later to remove this
      # Confirmed still broken, Mar 5, 2023
      manual.manpages.enable = false;
    };
  };
}
