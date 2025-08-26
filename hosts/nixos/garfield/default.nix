{ config, lib, pkgs, modulesPath, user, inputs, ... }:

let
  myEmacs = import ../../../modules/shared/emacs.nix { inherit pkgs; };
in

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./hardware-configuration.nix

    # Import shared configuration (tmux, zsh, home-manager, etc.)
    ../../../modules/shared
    
    # Import garfield-specific packages
    {
      environment.systemPackages = import ../../../modules/nixos/garfield-packages.nix { inherit config pkgs inputs; };
    }

    # Note: systemd.nix module excluded for this host
    # Note: agenix disabled for this host
  ];

  # Hardware Configuration - use dedicated hardware-configuration.nix
  # (imported above)

  # Networking
  networking = {
    hostName        = "garfield";
    useDHCP         = lib.mkDefault true;
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

  # Programs configuration - Steam disabled for this host
  programs = {
    zsh.enable = true;
    firefox.enable = true;
  };

  # Console configuration for virtual terminals
  console.useXkbConfig = true;

  # Services configuration
  services = {
    emacs = {
      enable = true;
      package = myEmacs;
    };

    # X11 configuration - Nvidia graphics
    xserver = {
     enable = true;
     videoDrivers = ["nvidia"];
     xkb = {
       layout = "us";
       options = "ctrl:nocaps";
     };
    };

    displayManager.sddm.enable = true;
    desktopManager.plasma6.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    # Enable sound with PipeWire
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

    # Bluetooth
    blueman.enable = true;
  };

  # Define a user account
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
  
  # Disable overlays for garfield
  nixpkgs.overlays = [
    (final: prev: {
      cider-appimage = null;  # Disable cider-appimage for garfield
    })
  ];

  # Note: chaotic.mesa-git.enable disabled for this host

  # Environment variables for Nvidia/Wayland
  environment.sessionVariables = {
    # Enable Nvidia Wayland support
    NIXOS_OZONE_WL = "1";
    # Nvidia Wayland environment variables
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    WLR_NO_HARDWARE_CURSORS = "1";
    # Electron apps (like VS Code) on Wayland
    ELECTRON_OZONE_PLATFORM_HINT = "wayland";
  };

  # List packages installed in system profile
  environment.systemPackages = with pkgs; [
    vim
    git
    myEmacs
    wl-clipboard     # Wayland clipboard utilities
    wayland-utils    # Wayland utilities
    lm_sensors       # Hardware monitoring sensors
    btop             # Modern resource monitor
    
    # Nvidia utilities
    nvidia-container-toolkit  # For containerized GPU workloads
  ];

  # Hardware platform
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  # Hardware support
  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    graphics = {
      enable = true;
      enable32Bit = true;
    };

    nvidia = {
      # Use the latest production driver
      modesetting.enable = true;
      
      # Power management (experimental)
      powerManagement.enable = false;
      powerManagement.finegrained = false;
      
      # Use the open source version of the kernel module (for RTX 20 series and newer)
      # Only available from driver 515.43.04+
      open = false;
      
      # Enable the Nvidia settings menu
      nvidiaSettings = true;
      
      # Use the latest stable driver package
      package = config.boot.kernelPackages.nvidiaPackages.stable;
    };

    # Intel CPU microcode updates
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };

  # Bootloader
  boot = {
    loader.systemd-boot = {
      enable             = true;
      configurationLimit = 42;
    };
    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;
  };

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
  fonts.packages = import ../../../modules/shared/fonts.nix { inherit pkgs; };

  # Configure Nix settings for flakes
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

  system.stateVersion = "25.05";
}
