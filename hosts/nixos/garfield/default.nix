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
      environment.systemPackages = import ../../../modules/nixos/garfield-packages.nix { inherit pkgs; };
    }

    # GitHub Runner module for lab CI
    ../../../modules/nixos/github-runner.nix

    # Note: systemd.nix module excluded for this host
    # Note: agenix disabled for this host
  ];

  # Hardware Configuration - use dedicated hardware-configuration.nix
  # (imported above)

  # Networking with VLAN support for GitHub runners
  networking = {
    hostName = "garfield";
    networkmanager.enable = false;  # Disabled for manual VLAN control
    useNetworkd = true;  # Use systemd-networkd for VLAN support
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];  # SSH
    };

    # VLAN interfaces on eno0 (or your main interface)
    # NOTE: Adjust interface name if not eno0 - check with `ip link`
    vlans = {
      "eno0.10" = {
        id = 10;
        interface = "eno0";
      };
      "eno0.20" = {
        id = 20;
        interface = "eno0";
      };
    };

    interfaces = {
      eno0 = {};
      "eno0.10" = {
        ipv4.addresses = [
          { address = "10.0.10.2"; prefixLength = 24; }
        ];
      };
      "eno0.20" = {
        ipv4.addresses = [
          { address = "10.0.20.2"; prefixLength = 24; }
        ];
      };
    };

    # Gateway on VLAN 10
    defaultGateway = {
      address = "10.0.10.1";
      interface = "eno0.10";
    };

    nameservers = [ "10.0.10.1" "1.1.1.1" ];
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
    # GitHub Runners Configuration
    github-runners-lab = {
      enable = true;
      runnerCount = 4;
      organization = "conductly";
    };

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

    displayManager = {
      sddm.enable = true;
      autoLogin = {
        enable = true;
        user = "dustin";
      };
    };

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

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

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
