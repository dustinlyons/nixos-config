# Dustin's NixOS configuration
{ config, inputs, pkgs, ... }:
{
  imports = [
    ./cachix
    ../common
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.hostName = "felix"; # Define your hostname.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  # Turn on flag for proprietary software
  nix = {
    settings.allowed-users = [ "dustin" ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };

  # Manages keys and such
  programs.gnupg.agent.enable = true;

  # Needed for anything GTK related
  programs.dconf.enable = true;

  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  # Sometimes I need to SSH into this machine, usually to reboot into Windows
  # from my couch for better SteamOS compatibility
  services.openssh = {
    enable = true;
    settings.passwordAuthentication = false;
  };

  # This helps fix tearing of windows
  services.xserver.screenSection = ''
    Option       "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
    Option       "AllowIndirectGLXProtocol" "off"
    Option       "TripleBuffer" "on"
  '';

  # LightDM Display Manager
  services.xserver.displayManager.defaultSession = "none+bspwm";
  services.xserver.displayManager.lightdm = {
    enable = true;
    greeters.slick.enable = true;
    background = ./config/login-wallpaper.png;
  };

  # My tiling window manager
  services.xserver.windowManager.bspwm = {
    enable = true;
    configFile = ./config/bspwmrc;
    sxhkd.configFile = ./config/sxhkdrc;
  };

  # Turn Caps Lock into Ctrl
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Enable CUPS to print documents
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.brlaser ];

  # Enable sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Video support
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.driSupport = true;
  hardware.nvidia.modesetting.enable = true;

  # Enable Xbox support
  # hardware.xone.enable = true;

  # Crypto wallet support
  hardware.ledger.enable = true;

  # Better support for general peripherals
  services.xserver.libinput.enable = true;
  boot.kernelModules = [ "uinput" ];

  # Sync state between machines
  services.syncthing = {
    enable = true;
    user = "dustin";
    dataDir = "/home/dustin/.config/syncthing";
    configDir = "/home/dustin/.config/syncthing";
  };

  # Add docker daemon
  virtualisation.docker.enable = true;
  virtualisation.docker.logDriver = "json-file";

  # Picom, my window compositor with fancy effects
  #
  # Notes on writing exclude rules:
  #
  #   class_g looks up index 1 in WM_CLASS value for an application
  #   class_i looks up index 0
  #
  #   To find the value for a specific application, use `xprop` at the
  #   terminal and then click on a window of the application in question
  #
  services.picom = {
    enable = true;
    settings = {
      animations = true;
      animation-stiffness = 300.0;
      animation-dampening = 35.0;
      animation-clamping = false;
      animation-mass = 1;
      animation-for-workspace-switch-in = "auto";
      animation-for-workspace-switch-out = "auto";
      animation-for-open-window = "slide-down";
      animation-for-menu-window = "none";
      animation-for-transient-window = "slide-down";
      corner-radius = 12;
      rounded-corners-exclude = [
        "class_i = 'polybar'"
        "class_g = 'i3lock'"
      ];
      round-borders = 3;
      round-borders-exclude = [];
      round-borders-rule = [];
      shadow = true;
      shadow-radius = 8;
      shadow-opacity = 0.4;
      shadow-offset-x = -8;
      shadow-offset-y = -8;
      fading = false;
      inactive-opacity = 0.8;
      frame-opacity = 0.7;
      inactive-opacity-override = false;
      active-opacity = 1.0;
      focus-exclude = [
      ];

      opacity-rule = [
        "100:class_g = 'i3lock'"
        "60:class_g = 'Dunst'"
        "100:class_g = 'Alacritty' && focused"
        "90:class_g = 'Alacritty' && !focused"
      ];

      blur-kern = "3x3box";
      blur = {
        method = "kernel";
        strength = 8;
        background = false;
        background-frame = false;
        background-fixed = false;
        kern = "3x3box";
      };

      shadow-exclude = [
        "class_g = 'Dunst'"
      ];

      blur-background-exclude = [
        "class_g = 'Dunst'"
      ];

      backend = "glx";
      vsync = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = false;
      detect-transient = true;
      detect-client-leader = true;
      use-damage = true;
      log-level = "info";

      wintypes = {
        normal = { fade = true; shadow = false; };
        tooltip = { fade = true; shadow = false; opacity = 0.75; focus = true; full-shadow = false; };
        dock = { shadow = false; };
        dnd = { shadow = false; };
        popup_menu = { opacity = 1.0; };
        dropdown_menu = { opacity = 1.0; };
      };
    };
  };

  # It's me
  users.users.dustin = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "docker"
    ];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOoC9CTKaguJf4cktkbVfU4+KdVL/kTg1XqIIwxwh/85"
    ];
  };

  # My editor runs as a daemon
  services.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ../common/config/emacs/config.org;
      package = pkgs.emacsUnstable;
      alwaysEnsure = true;
    };
  };

  environment.systemPackages = with pkgs; [
    gitAndTools.gitFull
    inetutils
  ];

  services.gvfs.enable = true; # Mount, trash, and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images

  system.stateVersion = "21.05"; # Don't change this

}
