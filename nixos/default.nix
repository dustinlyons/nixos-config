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

  # This helps fix tearing of windows
  services.xserver.screenSection = ''
    Option       "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
    Option       "AllowIndirectGLXProtocol" "off"
    Option       "TripleBuffer" "on"
  '';

  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.displayManager.gdm.wayland = false;
  services.xserver.desktopManager.gnome.enable = true;

  # Turn Caps Lock into Ctrl
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Enable CUPS to print documents
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.brlaser ];

  # Enable sound
  sound.enable = true;
  # Wireplumber broke my audio, patch is incoming but need this
  # until then. See: https://github.com/NixOS/nixpkgs/issues/163066
  services.pipewire.media-session.enable = true;
  services.pipewire.wireplumber.enable = false;

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
  
  # It's me
  users.users.dustin = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "docker"
    ];
    shell = pkgs.zsh;
  };

  environment.systemPackages = with pkgs; [
    gitAndTools.gitFull
    discord
    (emacsWithPackagesFromUsePackage {
      config = ../common/config/emacs/Emacs.org;
      package = emacsPgtkNativeComp;
      alwaysEnsure = true;
    })
  ];

  system.stateVersion = "21.05"; # Don't change this

}
