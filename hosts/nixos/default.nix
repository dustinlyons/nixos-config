{ config, lib, pkgs, modulesPath, user, ... }:

let
  myEmacs = import ../../modules/shared/emacs.nix { inherit pkgs; };
in

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")

    # Import shared configuration (tmux, zsh, packages, etc.)
    # Comment these out initially if you want to start completely minimal
    ../../modules/shared
    
    # Systemd services and timers
    ../../modules/nixos/systemd.nix

    # Agenix for secrets management - temporarily disabled
    # inputs.agenix.nixosModules.default
  ];

  # Hardware Configuration (merged from hardware-configuration.nix)
  boot = {
    loader.systemd-boot = {
      enable             = true;
      configurationLimit = 5;   # Keep 5 generations for rollback capability
    };
    loader.efi.canTouchEfiVariables = true;

    initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
    initrd.kernelModules        = [];
    kernelModules               = [ "kvm-amd" "uinput" ];
    kernelParams = [
      # Essential parameters for ASUS PG278Q monitor with RX 9070 GPU
      "amdgpu.dc=1"              # Force display core (required for RDNA 4 GPUs)
      "drm.edid_firmware=DP-2:edid/PG278Q.bin" # Force EDID for ASUS PG278Q monitor
      "video=DP-2:2560x1440@60e" # Force CVT timing to ensure proper display
      # GPU stability parameters
      "amdgpu.gpu_recovery=1"    # Enable GPU recovery after timeouts
      "amdgpu.runpm=0"          # Disable runtime power management for stability
    ];
    kernelPackages              = pkgs.linuxPackages_latest;
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

  # Windows partition mount
  fileSystems."/mnt/windows" = {
    device = "/dev/nvme0n1p3";
    fsType = "ntfs-3g";
    options = [
      "defaults"
      "uid=1000"
      "gid=100"
      "umask=0022"
      "nofail"
    ];
  };

  swapDevices = [ ];

  # Hardware platform
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  
  # Hardware support for gaming
  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };

    graphics = {
      enable = true;
      enable32Bit = true;
    };
    
    # Custom EDID firmware for ASUS PG278Q ROG Swift
    firmware = with pkgs; [ 
      (runCommand "pg278q-edid" {} ''
        mkdir -p $out/lib/firmware/edid
        cp ${./firmware/edid/PG278Q.bin} $out/lib/firmware/edid/PG278Q.bin
        cp ${./firmware/edid/PG278Q.bin} $out/lib/firmware/edid/DP-2.bin
      '')
    ];

  };

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
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
    };
  };

  # Console configuration for virtual terminals
  console.useXkbConfig = true;

  # Services configuration
  services = {
    emacs = {
      enable = true;
      package = myEmacs;
    };


    xserver = {
     enable = true;
     videoDrivers = ["amdgpu"];
     xkb = {
       layout = "us";
       options = "ctrl:nocaps";
     };
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

    # Bluetooth
    blueman.enable = true;

    # Key remapping service
    keyd = {
      enable = true;
      keyboards.default.settings.main = {
        end = "sysrq";  # Map End key to Print Screen
      };
    };

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

  # Enable mesa-git from Chaotic Nyx - disabled for GPU stability
  # chaotic.mesa-git.enable = true;

  # List packages installed in system profile. To search, run:
  #   $ nix search <pkg>
  environment.systemPackages = with pkgs; [
    vim
    git
    myEmacs
    wl-clipboard     # Wayland clipboard utilities (replaces xclip)
    wayland-utils    # Wayland utilities
    lm_sensors       # Hardware monitoring sensors
    btop             # Modern resource monitor with temp display
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

  # Increase inotify watch limit to prevent warnings
  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 1048576;
  };

  # Create symlink for easier Windows partition access
  systemd.tmpfiles.rules = [
    "L+ /home/dustin/windows - - - - /mnt/windows"
  ];

  # This value determines the NixOS release from which default
  # settings for stateful data were taken. Leave it at your first
  # install's release unless you know what you're doing.
  system.stateVersion = "25.05";
}
