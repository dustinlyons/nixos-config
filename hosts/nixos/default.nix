{ config, lib, pkgs, modulesPath, user, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")

    # Import shared configuration (tmux, zsh, packages, etc.)
    # Comment these out initially if you want to start completely minimal
    ../../modules/shared

    # Agenix for secrets management - temporarily disabled
    # inputs.agenix.nixosModules.default
  ];

  # Hardware Configuration (merged from hardware-configuration.nix)
  boot = {
    loader.systemd-boot = {
      enable             = true;
      configurationLimit = 42;  # Limit number of generations in boot menu
    };
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules        = [];
    kernelModules               = [ "kvm-amd" "uinput" ];
    #kernelPackages              = pkgs.linuxPackages_latest;
    #kernelModules               = [ "kvm-amd" "uinput" "v4l2loopback" ];
    #extraModulePackages         = [ pkgs.linuxPackages.v4l2loopback ];
  };

  # Filesystems
  fileSystems."/" =
    { device = "/dev/disk/by-uuid/27bb6e75-80f8-4072-8974-83f5a45cbe48";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/8AC5-E75B";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  swapDevices = [ ];

  # Hardware platform
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Networking
  networking = {
    hostName        = "felix";
    useDHCP         = lib.mkDefault true;
    # networking.interfaces.eno1.useDHCP = lib.mkDefault true;
    networkmanager.enable = true;
    firewall.enable       = false;
    
    # Custom hosts entries
    extraHosts = ''
      10.0.10.2 lab-1
      10.0.10.3 lab-2
    '';
  };

  # Set your time zone.
  time.timeZone = "America/Kentucky/Louisville";

  # Select internationalisation properties.
  i18n.defaultLocale      = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS        = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT    = "en_US.UTF-8";
    LC_MONETARY       = "en_US.UTF-8";
    LC_NAME           = "en_US.UTF-8";
    LC_NUMERIC        = "en_US.UTF-8";
    LC_PAPER          = "en_US.UTF-8";
    LC_TELEPHONE      = "en_US.UTF-8";
    LC_TIME           = "en_US.UTF-8";
  };

  # Programs configuration
  programs = {
    zsh.enable = true;
  };

  # Services configuration
  services = {
    #emacs = {
    #  enable = true;
    #  package = pkgs.emacs-unstable-pgtk;  # Wayland-native Emacs with pgtk
    #};

    xserver = {
     enable = true;
     videoDrivers = ["amdgpu"];
    };

    displayManager.sddm.enable = true;
    desktopManager.plasma6.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    # Enable sound with PipeWire (PulseAudio disabled in favor of PipeWire).
    pulseaudio.enable = false;

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };

    # Enable the OpenSSH daemon.
    openssh.enable = true;
  };

  # Define a user account. Don't forget to set a password with 'passwd'.
  users.users.${user} = {
    isNormalUser = true;
    description  = "Dustin Lyons";
    extraGroups  = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
  };

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "dustin";

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  #   $ nix search <pkg>
  environment.systemPackages = with pkgs; [
    vim
    git
    #emacs-unstable-pgtk
    wl-clipboard     # Wayland clipboard utilities (replaces xclip)
    wayland-utils    # Wayland utilities
  ];

  # Don't require password for users in `wheel` group for these commands
  security.sudo = {
    enable     = true;
    extraRules = [
      {
        commands = [
          {
            command = "${pkgs.systemd}/bin/reboot";
            options = [ "NOPASSWD" ];
          }
          {
            command = "/run/current-system/sw/bin/nixos-rebuild";
            options = [ "NOPASSWD" ];
          }
        ];
        groups = [ "wheel" ];
      }
    ];
  };

  # Fonts
  fonts.packages = import ../../modules/shared/fonts.nix { inherit pkgs; };

  # Configure Nix settings for flakes and Cachix
  nix = {
    nixPath = [
      "nixos-config=/home/${user}/.local/share/src/nixos-config:/etc/nixos"
    ];
    settings = {
      allowed-users       = [ "${user}" ];
      trusted-users       = [ "@admin" "${user}" "root" ];
      substituters        = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      ];
      experimental-features = [ "nix-command" "flakes" ];
    };
    package      = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # This value determines the NixOS release from which default
  # settings for stateful data were taken. Leave it at your first
  # install's release unless you know what you're doing.
  system.stateVersion = "25.05";
}
