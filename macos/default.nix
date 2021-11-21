{ config, pkgs, nixpkgs, ... }:

let
  common-programs = import ../common/programs.nix { pkgs = nixpkgs; }; in
{

  imports = [ ./home.nix ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowInsecure = false;
      allowUnsupportedSystem = true;
    };
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # Setup user, packages, programs
  nix.package = pkgs.nixUnstable;
  nix.trustedUsers = [ "@admin" "dustin" ];
  nix.gc.user = "root";
  
  # Turn off NIX_PATH warnings now that we're using flakes
  system.checks.verifyNixPath = false;

  environment.systemPackages = [
    #(pkgs.emacsWithPackagesFromUsePackage {
    #  config = ../common/config/emacs/Emacs.org;
    #  package = pkgs.emacsGit;
    #  alwaysEnsure = true;
    #})
  ];

  # Enable fonts dir
  fonts.enableFontDir = true;

  programs = common-programs // {
    zsh = {
      enable = true;
    };
  };

  system = {
    stateVersion = 4;

    defaults = {
      LaunchServices = {
        LSQuarantine = false;
      };

      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        ApplePressAndHoldEnabled = false;

        # 120, 90, 60, 30, 12, 6, 2
        KeyRepeat = 2;

        # 120, 94, 68, 35, 25, 15
        InitialKeyRepeat = 15;

        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = "0.0";
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = false;
        launchanim = true;
        orientation = "bottom";
        tilesize = 64;
      };

      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };
}
