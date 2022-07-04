{ nixpkgs ? <nixpkgs>, system ? "x86_64-linux" }:

let
  iso-config = { pkgs, ... }: {
    imports = [
      "${nixpkgs}/nixos/modules/installer/cd-dvd/iso-image.nix"
      "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
    ];

    time.timeZone = "America/New_York";
    networking.hostName = "compute";
    networking.hostId = "67518725";

    boot.initrd.supportedFilesystems = ["zfs"]; # boot from zfs
    boot.supportedFilesystems = [ "zfs" ];
    services.udev.extraRules = ''
      ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
'';

    nixpkgs.config.allowUnfree = true;
    environment.systemPackages = with pkgs; [ zfs vim wget htop inetutils ];

    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    networking.firewall.enable = false;

    users.extraUsers.root.password = "password"; # Change after first login
    users.users.dustin = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };

    virtualisation.docker.enable = true;
    services.openssh.enable = true;
  };

  evalNixos = configuration: import "${nixpkgs}/nixos" {
    inherit system configuration;
  };

in { iso = (evalNixos iso-config).config.system.build.isoImage; }
