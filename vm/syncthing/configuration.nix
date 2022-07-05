{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs.autoSnapshot = {
    enable = true;
    frequent = 30; # Keep last thirty 15-minute snapshots (instead of four)
  };
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "state"; # Define your hostname.

  # Set your time zone.
  time.timeZone = "America/New_York";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.hostId = "e583cd01";
  networking.interfaces.ens18.ipv4.addresses = [ {
    address = "192.168.0.215";
    prefixLength = 24;
  } ];

  networking.defaultGateway = "192.168.0.1";
  networking.nameservers = [ "192.168.0.223" ];

  users.users.dustin = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # 12:00 - Snapshot data with current date
  # 2:00 - Destroy anything older than 30 days
  # 2:00 - Incrementally send today's changes to mirrored pool
  # 2:00 - Destroy anything older than 30 days on mirrored pool
  services.cron = {
    enable = true;
    systemCronJobs = [
      "1  0 * * *      root    zfs snapshot rpool/data@`date +\%Y-\%m-\%d` 2>> /var/log/zfs.snapshot"
      "0  2 * * *      root    zfs destroy rpool/data@`date -d -30days +\%Y-\%m-\%d` 2>> /var/log/zfs.destroy"
      "0  2 * * *      root    zfs send -i rpool/data@`date -d -1days +\%Y-\%m-\%d` rpool/data@`date +\%Y-\%m-\%d` | ssh dustin@192.168.0.223 zfs recv rpool/backups 2>> /var/log/zfs.send"
      "0  2 * * *      root    ssh dustin@192.168.0.223 zfs destroy rpool/data@`date -d -30days +\%Y-\%m-\%d` 2>> /var/log/zfs.destroy"
    ];

  };

  services.syncthing = {
    enable = true;
    user = "dustin";
    dataDir = "/home/dustin";
    configDir = "/data/config";
  };

  environment.systemPackages = with pkgs; [
    git
    rsync
    vim
    wget
    htop
    inetutils
  ];

  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

