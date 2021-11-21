# Dustin's NixOS configuration
{ config, pkgs, ... }:
{
  imports = [
    ../common
  ];

  # Import shared configuration

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
  #networking.hostId = "18675309";
  #boot.initrd.supportedFilesystems = ["zfs"]; # boot from zfs
  #boot.supportedFilesystems = [ "zfs" ];
  #services.udev.extraRules = ''
  #  ACTION=="add|change",
  #  KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*",
  #  ENV{ID_FS_TYPE}=="zfs_member",
  #  ATTR{../queue/scheduler}="none"
  #'';

  # Turn on flag for proprietary software
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  nix = {
    allowedUsers = [ "dustin" ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };

  services.emacs.package = pkgs.emacsPgtkGcc;

  nixpkgs.overlays = [
    (import (builtins.fetchGit {
      url = "https://github.com/nix-community/emacs-overlay.git";
      ref = "master";
      rev = "278ab5def18f7c3edd2dc65a64994e8b3aa63390";
    }))
  ];

  # Enable the X11 windowing system
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.screenSection = ''
    Option       "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
    Option       "AllowIndirectGLXProtocol" "off"
    Option       "TripleBuffer" "on"
  '';

  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Turn Caps Lock into Ctrl
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Enable CUPS to print documents
  services.printing.enable = true;

  # Enable sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # libinput provides better support for our stuff
  services.xserver.libinput.enable = true;

  # Sync state between machines
  services.syncthing = {
    enable = true;
    user = "dustin";
    dataDir = "/home/dustin/.config/syncthing";
    configDir = "/home/dustin/.config/syncthing";
  };

  # It's me
  users.users.dustin = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.systemPackages = [
    (pkgs.emacsWithPackagesFromUsePackage {
      config = ../common/config/emacs/Emacs.org;
      package = pkgs.emacsPgtkGcc;
      alwaysEnsure = true;
    })
  ];

  system.stateVersion = "21.05"; # Don't change this

}
