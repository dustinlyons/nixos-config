{ config, pkgs, nixpkgs, ... }:
{

  imports = [
    ../common
    ./cachix
    ./home-manager.nix
  ];

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  services.activate-system.enable = true;

  # Setup user, packages, programs
  nix = {
    trustedUsers = [ "@admin" "dustin" ];
    package = pkgs.nixUnstable;
    gc.user = "root";
    # Highly recommend adding these to save keystrokes
    # at the command line
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # GTK Native Comp Emacs
  services.emacs.package = pkgs.emacsGcc;

  # Turn off NIX_PATH warnings now that we're using flakes
  system.checks.verifyNixPath = false;

  environment.systemPackages = with pkgs; [
    (emacsWithPackagesFromUsePackage {
      config = ../common/config/emacs/Emacs.org;
      package = emacsGcc;
      alwaysEnsure = true;
    })
  ] ++ (import ../common/packages.nix { pkgs = pkgs; });

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
