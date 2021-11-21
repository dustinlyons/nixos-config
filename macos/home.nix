{ config, pkgs, ... }:

let 
  common-programs = import ../common/programs.nix { pkgs = pkgs; }; in
{
  imports = [ 
    <home-manager/nix-darwin> 
    ./dock
  ];

  local.dock.enable = true;
  local.dock.entries = [
    { path = "/Applications/Brave Browser.app/"; }
    { path = "/Applications/Microsoft Teams.app/"; }
    { path = "/Applications/Slack.app/"; }
    { path = "/System/Applications/Messages.app/"; }
    { path = "/Applications/iTerm.app/"; }
    { path = "/System/Applications/Music.app/"; }
    { path = "/System/Applications/Home.app/"; }
    {
      path = "/Users/dustin/State/";
      section = "others";
      options = "--sort name --view grid --display folder";
    }
  ];

  # It me
  users.users.dustin = {
    name = "dustin";
    home = "/Users/dustin";
    isHidden = false;
  };

  # We use Homebrew to install impure software only (Mac Apps)
  homebrew.enable = true;
  homebrew.cleanup = "uninstall";
  homebrew.brews = [ ];
  homebrew.brewPrefix = "/opt/homebrew/bin";
  homebrew.casks = pkgs.callPackage ./casks.nix {};

  # Link Applications from home-manager so Raycast can pick them up
  # Note, Spotlight ignores symlinks in Applications; only Raycast works here
  home-manager.users.dustin = { pkgs, ... }: {
    home.packages = pkgs.callPackage ./packages.nix {};
    home.file."Applications/From Nix".source = let
      apps = pkgs.buildEnv {
        name = "from-nix-applications";
        paths = pkgs.callPackage ./packages.nix {};
        pathsToLink = "/Applications";
      }; in "${apps}/Applications";
    programs = common-programs // { };
  };
}
