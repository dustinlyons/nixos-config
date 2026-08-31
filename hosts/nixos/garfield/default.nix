{ config, lib, pkgs, modulesPath, user, agenix, ... }:

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

    # Home Assistant for camera monitoring and automation
    ../../../modules/nixos/home-assistant.nix

    # n8n workflow automation for GTM pipeline
    ../../../modules/nixos/n8n.nix

    # Jenkins CI (LAN-only vhost; nginx base is n8n.nix)
    ../../../modules/nixos/jenkins.nix

    # Atlas webhook ingress: TLS front door for hooks.dlyons.dev, proxied to
    # Atlas on felix (garfield owns the WAN 443 forward; nginx base is n8n.nix)
    ../../../modules/nixos/hooks-proxy.nix

    # LAN-only HTTP host for AppImage binaries (fetched by other hosts at build time)
    ../../../modules/nixos/appimage-host.nix

    # Nightly off-box backup of the state nixos-rebuild cannot recreate
    # (n8n workflows + database, Home Assistant .storage).
    ../../../modules/nixos/backups.nix

    # agenix for garfield's non-reproducible system secrets
    # (n8n encryption key, GitHub runner registration).
    agenix.nixosModules.default
    ../../../modules/nixos/garfield-secrets.nix

    # Note: atlas devenv migrated to felix (2026-06-27); import removed so a
    # rebuild won't restart it here. It was also masked at runtime on garfield.
    # Note: systemd.nix module excluded for this host
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
      allowedTCPPorts = []; # Per-service ports managed below with source restrictions
      # Allow SSH and Home Assistant only from LAN subnets.
      # Without this, ports 22/8123 are open to any source IP — reachable
      # from the Internet if the UDM forwards them.
      extraCommands = ''
        iptables -A nixos-fw -p tcp --dport 22 -s 10.0.10.0/24 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 22 -s 192.168.0.0/24 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 22 -s 127.0.0.0/8 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 8123 -s 10.0.10.0/24 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 8123 -s 192.168.0.0/24 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 8123 -s 127.0.0.0/8 -j nixos-fw-accept
      '';
      extraStopCommands = ''
        iptables -D nixos-fw -p tcp --dport 22 -s 10.0.10.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 22 -s 192.168.0.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 22 -s 127.0.0.0/8 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 8123 -s 10.0.10.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 8123 -s 192.168.0.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 8123 -s 127.0.0.0/8 -j nixos-fw-accept 2>/dev/null || true
      '';
    };

    # ADDRESSING — this host is on DHCP, and that is deliberate now.
    #
    # This block used to declare VLAN sub-interfaces on `eno0` (10.0.10.2 on
    # VLAN 10, 10.0.20.2 on VLAN 20) with a matching static default gateway.
    # None of it ever took effect: there is no `eno0` on this machine. The
    # single NIC enumerates as `eno1`, systemd-networkd had nothing to attach
    # the VLANs to, and the box has been reachable this whole time only
    # because hardware-configuration.nix leaves `networking.useDHCP` on.
    #
    # Removed rather than corrected (2026-08-31). The declared addresses were
    # never the ones in use, so "fixing" them to eno0.10 would have moved a
    # working host onto an untested static configuration during the one
    # exercise — disaster recovery — where that is least welcome. What runs
    # today is one DHCP interface at 10.0.10.134, a lease from the UDM.
    #
    # Consequence worth knowing before a rebuild on new hardware: the address
    # comes from the UDM, not from this file. New hardware means a new MAC and
    # a new lease, so either give the replacement a reservation for
    # 10.0.10.134 or expect to re-point the UDM's forwards and its static DNS
    # record. Nothing in this repo binds to that address any more — see the
    # listen/hosts notes in appimage-host.nix and jenkins.nix — so services
    # will start regardless; they just won't be where the network expects.
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
      # Cap concurrent CI jobs at 2 box-wide (pest / playwright / preview
      # deploys, all selecting `[self-hosted, ci]`). 2 keeps the box from being
      # overwhelmed.
      runnerCount = 2;
      # Plus one deploy-only runner (`[self-hosted, deploy]`) that CI can never
      # occupy, so staging/production deploys start immediately instead of
      # queueing behind a multi-hour playwright run. Cheap to co-locate: deploys
      # are SSH/rsync-bound and finish in under 15 minutes.
      deployRunnerCount = 1;
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
    # openFirewall disabled — port 22 is opened with LAN source restrictions
    # in the firewall.extraCommands above instead of globally.
    openssh = {
      enable = true;
      openFirewall = false;
    };

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
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
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
