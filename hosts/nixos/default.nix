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

    # WoW chat-log watcher: popups + history for LFG / recruitment messages
    ../../modules/nixos/wow-chat-watch.nix

    # Agenix for secrets management - temporarily disabled
    # inputs.agenix.nixosModules.default
  ];

  # Hardware Configuration (merged from hardware-configuration.nix)
  boot = {
    loader.systemd-boot = {
      enable             = true;
      configurationLimit = 1;   # ESP is only 96M — can't fit more than one generation (~41M each)
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
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/27bb6e75-80f8-4072-8974-83f5a45cbe48";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/8AC5-E75B";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

    # Windows partition mount
    "/mnt/windows" = {
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

    # Xbox One / Series X|S controllers, incl. the Xbox Wireless Adapter dongle.
    # Pulls in the xone modules for the running kernel + the dongle firmware, and
    # blacklists mt76x2u, which otherwise claims the dongle (045e:02fe is an
    # MT7612U) and exposes it as a useless wifi interface.
    xone.enable = true;

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

    # Firewall on, nothing opened globally. Before 2026-09-11 this was
    # `firewall.enable = false`, which left the Conductly dev database
    # (mariadb :3306), `php artisan serve` (:8000), Vite (:5173), Storybook
    # (:6006) and the Atlas webhook listener (:8788) reachable from every phone,
    # TV and guest device on the client VLAN. Those dev servers still bind
    # 0.0.0.0 (as of 2026-09-16: mariadb :3306, PHP :8000, Next :3000,
    # Storybook :6006, epmd :4369, and Vite on the LAN address :5173) — the
    # firewall is what keeps them off the LAN, not their bind address. The two
    # things other machines legitimately reach get source-restricted rules
    # below, in the same style garfield uses.
    #
    # programs.steam's remotePlay/dedicatedServer openFirewall options (above)
    # declare their own ports and start taking effect now that this is on.
    # IPv4 only, like garfield: the LAN does not route IPv6.
    firewall = {
      enable = true;
      allowedTCPPorts = [];
      extraCommands = ''
        # SSH from the two LAN VLANs and the UDM's remote-access VPN.
        iptables -A nixos-fw -p tcp --dport 22 -s 10.0.10.0/24 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 22 -s 192.168.0.0/24 -j nixos-fw-accept
        iptables -A nixos-fw -p tcp --dport 22 -s 192.168.1.0/24 -j nixos-fw-accept

        # Atlas webhook receiver (systemd.nix binds it to 0.0.0.0:8788).
        # garfield's nginx is the only legitimate caller (hooks-proxy.nix) and
        # it lives on the server VLAN; nothing on the client VLAN needs this.
        iptables -A nixos-fw -p tcp --dport 8788 -s 10.0.10.0/24 -j nixos-fw-accept

        # mDNS from the client VLAN, so Chrome casting and Steam's LAN
        # discovery keep working (both were seen bound to 5353 when this was
        # written). Multicast queries are unsolicited, so the default
        # established/related rule does not cover them.
        iptables -A nixos-fw -p udp --dport 5353 -s 192.168.0.0/24 -j nixos-fw-accept

        # Opt-in: testing the Conductly dev build from a phone on the client
        # VLAN. Vite was deliberately bound to this box's LAN address, so this
        # was probably in use; uncomment if it is. Leave 3306 closed regardless.
        # iptables -A nixos-fw -p tcp --dport 5173 -s 192.168.0.0/24 -j nixos-fw-accept
        # iptables -A nixos-fw -p tcp --dport 8000 -s 192.168.0.0/24 -j nixos-fw-accept
      '';
      extraStopCommands = ''
        iptables -D nixos-fw -p tcp --dport 22 -s 10.0.10.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 22 -s 192.168.0.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 22 -s 192.168.1.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p tcp --dport 8788 -s 10.0.10.0/24 -j nixos-fw-accept 2>/dev/null || true
        iptables -D nixos-fw -p udp --dport 5353 -s 192.168.0.0/24 -j nixos-fw-accept 2>/dev/null || true
        # iptables -D nixos-fw -p tcp --dport 5173 -s 192.168.0.0/24 -j nixos-fw-accept 2>/dev/null || true
        # iptables -D nixos-fw -p tcp --dport 8000 -s 192.168.0.0/24 -j nixos-fw-accept 2>/dev/null || true
      '';
    };

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

    # Enable the OpenSSH daemon. Keys only: this desktop has no firewall and
    # shares the client VLAN with every phone and IoT device in the house.
    openssh = {
      enable = true;
      # Port 22 is opened per-source in networking.firewall.extraCommands
      # above; the module's default would open it to every address first and
      # make those rules meaningless.
      openFirewall = false;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
    };

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
    extraGroups  = [ "networkmanager" "wheel" "docker" ];
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
    bun              # JS runtime for the atlas devenv service (migrated from garfield)
    bubblewrap       # Unprivileged sandboxing tool
  ];

  # Don't require a password for `reboot`. nixos-rebuild used to be listed
  # here too; it was removed 2026-09-11 because it evaluates and activates
  # arbitrary Nix, so a passwordless entry for it is a passwordless root
  # shell for anyone holding a wheel session (this desktop auto-logs in).
  # `nix run .#build-switch` now prompts once per switch.
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
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      experimental-features = [ "nix-command" "flakes" ];
    };
    package      = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # Collect garbage weekly. This trims what the Nix store holds; the boot
    # menu is a separate limit (configurationLimit above, capped by the 96M
    # ESP), so this governs how far back `nixos-rebuild --rollback` can reach,
    # not how many entries appear at boot. The running generation is never
    # deleted.
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 30d";
    };

    # Hard-link identical files across generations. Run as a weekly job rather
    # than settings.auto-optimise-store, which hashes every path as it is
    # written and would slow down every build.
    optimise = {
      automatic = true;
      dates     = [ "03:45" ];
    };
  };

  # Increase inotify watch limit to prevent warnings
  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 1048576;
  };

  # Docker
  virtualisation.docker.enable = true;

  # Create symlink for easier Windows partition access
  systemd.tmpfiles.rules = [
    "L+ /home/dustin/windows - - - - /mnt/windows"
  ];

  # This value determines the NixOS release from which default
  # settings for stateful data were taken. Leave it at your first
  # install's release unless you know what you're doing.
  system.stateVersion = "25.05";
}
