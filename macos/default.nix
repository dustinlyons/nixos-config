{ config, pkgs, nixpkgs, ... }:
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
  services.activate-system.enable = true;

  # Setup user, packages, programs
  nix = {
    trustedUsers = [ "@admin" "dustin" ];
    package = pkgs.nixUnstable;
    gc.user = "root";
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };

  # Turn off NIX_PATH warnings now that we're using flakes
  system.checks.verifyNixPath = false;

  environment.systemPackages = pkgs.callPackage ./packages.nix {};

  # Enable fonts dir
  fonts.enableFontDir = true;

  programs = { };

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
        tilesize = 48;
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
