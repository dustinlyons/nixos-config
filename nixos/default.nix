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
  networking.extraHosts =
  ''
    192.168.2.67 BRN008077D92A06.local # Printer
  '';

  # Turn on flag for proprietary software
  nix = {
    # allowedUsers = [ "dustin" ];
    settings.allowed-users = [ "dustin" ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };

  # Video games, patch libusb1 so Xbox controller works
  programs.steam.enable = true;
  programs.gnupg.agent.enable = true;

  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  # Sometimes I need to SSH into this machine
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
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
    greeters.enso.enable = true;
  };

  services.xserver.windowManager.bspwm = {
    enable = true;
    configFile = ./bspwmrc;
    sxhkd.configFile = ./sxhkdrc;
  };

  # Turn Caps Lock into Ctrl
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Enable CUPS to print documents
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.brlaser ];

  # Enable sound
  sound.enable = true;

  # Video support
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.driSupport = true;
  hardware.nvidia.modesetting.enable = true;

  # Enable Xbox support
  hardware.xone.enable = true;

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

  services.picom = {
    enable = true;
    settings = {
      animations = true;
      animation-stiffness = 300.0;
      animation-dampening = 35.0;
      animation-clamping = false;
      animation-mass = 1;
      animation-for-open-window = "zoom";
      animation-for-menu-window = "slide-down";
      animation-for-transient-window = "slide-down";
      corner-radius = 13;
      rounded-corners-exclude = [
      ];
      round-borders = 3;
      round-borders-exclude = [
      ];

      round-borders-rule = [
      ];

      shadow = false;
      shadow-radius = 44;
      shadow-opacity = .75;
      shadow-offset-x = -15;
      shadow-offset-y = -15;
      shadow-exclude = [
      ];

      fading = false;
      fade-in-step = 0.09;
      fade-out-step = 0.09;
      inactive-opacity = 0.8;
      frame-opacity = 0.7;
      inactive-opacity-override = false;
      active-opacity = 1.0;
      focus-exclude = [
      ];

      opacity-rule = [
      ];

      blur-kern = "3x3box";
      blur = {
        method = "kawase";
        strength = 8;
        background = true;
        background-frame = false;
        background-fixed = false;
        kern = "3x3box";
      };

      blur-background-exclude = [
      ];

      experimental-backends = true;
      backend = "glx";
      vsync = false;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = false;
      refresh-rate = 60;
      detect-transient = true;
      detect-client-leader = true;
      use-damage = true;
      log-level = "info";

      wintypes = {
        normal = { fade = true; shadow = false; };
        tooltip = { fade = true; shadow = true; opacity = 0.75; focus = true; full-shadow = false; };
        dock = { shadow = false; };
        dnd = { shadow = false; };
        popup_menu = { opacity = 0.8; };
        dropdown_menu = { opacity = 0.8; };
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

  environment.systemPackages = with pkgs; [
    gitAndTools.gitFull
    xfce.thunar
    inetutils
    (emacsWithPackagesFromUsePackage {
      config = ../common/config/emacs/Emacs.org;
      package = emacsPgtkNativeComp;
      alwaysEnsure = true;
    })
  ];

  services.gvfs.enable = true; # Mount, trash, and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images

  system.stateVersion = "21.05"; # Don't change this

}
