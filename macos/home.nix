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

  # Link Applications from home-manager so Raycast can pick them up
  # Note, Spotlight ignores symlinks in Applications; only Raycast works here
  #system.build.applications = pkgs.lib.mkForce (pkgs.buildEnv {
  #  name = "applications";
  #  paths = config.environment.systemPackages ++ config.home-manager.users.dustin.home.packages;
  #  pathsToLink = "/Applications";
  #});

  system.activationScripts.applications.text = let
    env = pkgs.buildEnv {
      name = "system-applications";
      paths = config.environment.systemPackages ++ config.home-manager.users.dustin.home.packages;
      pathsToLink = "/Applications";
    };
  in pkgs.lib.mkForce ''
    echo "setting up ~/Applications..." >&2
    rm -rf ~/Applications/Nix\ Apps
    mkdir -p ~/Applications/Nix\ Apps
    find ${env}/Applications -maxdepth 1 -type l -exec readlink '{}' + |
        while read src; do
          /bin/cp -cr "$src" ~/Applications/Nix\ Apps
        done
  '';

  home-manager.users.dustin = { pkgs, ... }: {
    home.packages = pkgs.callPackage ./packages.nix {};
    programs = common-programs // { };
  };
}
