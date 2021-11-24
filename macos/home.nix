{ config, pkgs, lib, ... }:

let 
  common-programs = import ../common/programs.nix { pkgs = pkgs; }; in
{
  imports = [ 
    <home-manager/nix-darwin> 
    ./dock
  ];

  local.dock.enable = true;
  local.dock.entries = [
    { path = "/Applications/Slack.app/"; }
    { path = "/Applications/Discord.app/"; }
    { path = "/Applications/Telegram.app/"; }
    { path = "/System/Applications/Messages.app/"; }
    { path = "/System/Applications/Facetime.app/"; }
    { path = "/Applications/zoom.us.app/"; }
    { path = "/Applications/Notion.app/"; }
    { path = "/Applications/Brave Browser.app/"; }
    { path = "/Applications/iTerm.app/"; }
    { path = "/System/Applications/Music.app/"; }
    { path = "/System/Applications/Podcasts.app/"; }
    { path = "/System/Applications/Photos.app/"; }
    { path = "/System/Applications/Photo Booth.app/"; }
    { path = "/System/Applications/News.app/"; }
    { path = "/Applications/Harvest.app/"; }
    { path = "/Applications/Drafts.app/"; }
    { path = "/System/Applications/Home.app/"; }
    {
      path = "/Users/dustin/State/";
      section = "others";
      options = "--sort name --view grid --display folder";
    }
    {
      path = "/Users/dustin/State/Inbox/Downloads";
      section = "others";
      options = "--sort name --view grid --display stack";
    }
  ];

  # It me
  users.users.dustin = {
    name = "dustin";
    home = "/Users/dustin";
    isHidden = false;
    shell = pkgs.zsh;
  };

  # We use Homebrew to install impure software only (Mac Apps)
  homebrew.enable = true;
  homebrew.cleanup = "uninstall";
  homebrew.brewPrefix = "/opt/homebrew/bin";
  homebrew.casks = pkgs.callPackage ./casks.nix {};
  homebrew.masApps = {
    "1password" = 1333542190;
    "drafts" = 1435957248;
    "harvest" = 506189836;
    "hidden-bar" = 1452453066;
    "instapaper" = 288545208;
    "yoink" = 457622435; 
  };

  home-manager = {
    useGlobalPkgs = true;
    users.dustin = { pkgs, lib, ... }: {
      home.packages = pkgs.callPackage ./packages.nix {};
      programs = common-programs // { };
      home.activation = {
        aliasApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          rm -rf /Applications/Home\ Manager\ Apps
          mkdir -p /Applications/Home\ Manager\ Apps
          for i in $(ls ~/Applications); do app=$i; ln -s ~/Applications/$app /Applications/Home\ Manager\ Apps/$app; done
        '';
      };
    };
  };
}
