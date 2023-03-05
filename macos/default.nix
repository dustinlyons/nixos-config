{ config, pkgs, nixpkgs, ... }:
{

  imports = [
    ../common
    ./cachix
    ./home-manager.nix
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # Setup user, packages, programs
  nix = {
    package = pkgs.nixUnstable;
    settings.trusted-users = [ "@admin" "dustin" ];
    gc.user = "root";

    # Turn this on to make command line easier
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Turn off NIX_PATH warnings now that we're using flakes
  system.checks.verifyNixPath = false;

  services.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
     config = ../common/config/emacs/config.org;
     package = pkgs.emacsNativeComp;
     alwaysEnsure = true;
   };
  };

  # Load configuration that is shared across systems
  environment.systemPackages = with pkgs; [] ++ (import ../common/packages.nix { pkgs = pkgs; });

  # Enable fonts dir
  fonts.fontDir.enable = true;

  # Confusing, I know. Nix has too many ways to get stuff done.
  # This is nix-darwin's programs attrset. We don't use it.
  # Instead, we use home-manager to manage program settings.
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
        "com.apple.sound.beep.volume" = 0.0;
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = false;
        show-recents = false;
        launchanim = true;
        orientation = "bottom";
        tilesize = 48;
      };

      finder = {
        _FXShowPosixPathInTitle = false;
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
