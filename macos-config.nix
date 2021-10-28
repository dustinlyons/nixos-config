{ config, pkgs, ... }:

{
  environment.systemPackages = [ ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowInsecure = false;
      allowUnsupportedSystem = false;
    };
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  # Setup user, packages, programs
  nix.package = pkgs.nixUnstable;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina

  imports = [ <home-manager/nix-darwin> ];

  users.users.dustin = {
    name = "dustin";
    home = "/Users/dustin";
  };

  home-manager.users.dustin = { pkgs, ... }: {
    home.packages = pkgs.callPackage ./macos-packages.nix {};
  };

  system = {
    stateVersion = 4;

    defaults = {
      NSGlobalDomain = {
        ApplePressAndHoldEnabled = false;
        _HIHideMenuBar = true;
        "com.apple.keyboard.fnState" = true;
        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = "0.0";
        "com.apple.sound.beep.feedback" = 0;
      };

      dock = {
        autohide = false;
        launchanim = true;
        orientation = "bottom";
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
