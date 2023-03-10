{ config, pkgs, nixpkgs, ... }:

let user = "dustin"; in
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
    settings.trusted-users = [ "@admin" "${user}" ];

    gc = {
      user = "root";
      automatic = true;
      interval = { Weekday = 0; Hour = 2; Minute = 0; };
      options = "--delete-older-than 30d";
    };

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

  # WIP
  launchd.user.agents.emacs.serviceConfig = {
    KeepAlive = true;
    ProgramArguments = [
      "/bin/sh"
      "-c"
      ''
        /bin/wait4path ${pkgs.emacs}/bin/emacs && \
          exec ${pkgs.emacs}/bin/emacs --fg-daemon
      ''
    ];
    StandardErrorPath = "/tmp/emacs.err.log";
    StandardOutPath = "/tmp/emacs.out.log";
  };

  # WIP
  launchd.daemons."nix-store-optimise".serviceConfig = {
    ProgramArguments = [
      "/bin/sh"
      "-c"
      ''
        /bin/wait4path ${config.nix.package}/bin/nix && \
          exec ${config.nix.package}/bin/nix store optimise
      ''
    ];
    StartCalendarInterval = [
      {
        Hour = 2;
        Minute = 30;
      }
    ];
    StandardErrorPath = "/tmp/nix-store.err.log";
    StandardOutPath = "/tmp/nix-store.out.log";
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
