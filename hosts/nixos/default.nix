{ config, lib, pkgs, modulesPath, inputs, user, ... }:

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

    initrd.availableKernelModules = [
      "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" "v4l2loopback"
    ];
    initrd.kernelModules        = [];
    kernelModules               = [ "uinput" "v4l2loopback" ];  # uinput for input devices, v4l2loopback for virtual cameras
    extraModulePackages         = [ pkgs.linuxPackages.v4l2loopback ];
  };

  # Filesystems
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3b81b6bc-b655-4985-b7dc-108ffa292c63";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device  = "/dev/disk/by-uuid/D302-2157";
    fsType  = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/d2b78e71-7ea1-472d-864a-64072cfa4978"; }
  ];

  # Hardware platform
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Networking
  networking = {
    hostName        = "felix";
    useDHCP         = lib.mkDefault true;
    # networking.interfaces.eno1.useDHCP = lib.mkDefault true;
    networkmanager.enable = true;
    firewall.enable       = false;
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

  # Enable the X11 windowing system (still needed for compatibility).
  # Enable the KDE Plasma Desktop Environment with Wayland.
  services = {
    xserver = {
      enable = true;

      # Configure keymap
      xkb = {
        layout  = "us";
        variant = "";
      };
    };

    displayManager = {
      sddm = {
        enable = true;
        wayland.enable = true;  # Enable Wayland support in SDDM
      };

      # Enable automatic login for the user.
      autoLogin = {
        enable = true;
        inherit user;
      };

      # Set default session to Wayland
      defaultSession = "plasma";  # This will use Plasma Wayland by default
    };

    # KDE Plasma 6 desktop
    desktopManager.plasma6.enable = true;

    emacs = {
      enable = true;
      package = pkgs.emacs;
    };

    # Enable CUPS to print documents.
    printing.enable = true;

    # Enable sound with PipeWire (PulseAudio disabled in favor of PipeWire).
    pulseaudio.enable = false;

    pipewire = {
      enable           = true;
      alsa.enable      = true;
      alsa.support32Bit = true;
      pulse.enable     = true;
      # If you want to use JACK applications, uncomment:
      # jack.enable = true;
      # use the example session manager:
      # media-session.enable = true;
    };

    # Enable touchpad support (enabled by default in most desktopManager).
    # xserver.libinput.enable = true;

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

  # Install firefox.
  programs.firefox.enable = true;

  # My shell
  programs.zsh.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  #   $ nix search <pkg>
  environment.systemPackages = with pkgs; [
    vim
    git
    emacs
    # Wayland-specific utilities
    wl-clipboard     # Wayland clipboard utilities (replaces xclip)
    wayland-utils    # Wayland utilities
    # inputs.agenix.packages."${pkgs.system}".default  # agenix CLI (temporarily disabled)
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
