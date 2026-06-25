{ config, pkgs, lib, ... }:

let
  # LAN-only access — same subnets the n8n module trusts.
  lanCidrs = [
    "10.0.10.0/24"    # Server VLAN
    "192.168.0.0/24"  # Client VLAN
    "127.0.0.1/32"    # Localhost
  ];
  lanAllowRules = lib.concatMapStringsSep "\n" (cidr: "allow ${cidr};") lanCidrs;

  # Where the AppImages live on garfield. Uploaded out-of-band (scp), not via
  # Nix and not tracked in git — see the overlays/*-appimage.nix fetchurl URLs.
  serveRoot = "/var/lib/appimages";

  # eno1 carries 10.0.10.134, the address `garfield` resolves to on the LAN.
  serveAddr = "10.0.10.134";
  servePort = 8088;
in
{
  # ========================================
  # AppImage file host
  # ========================================
  # Serves large AppImage binaries (Cider, Obsidian, CurseForge) to other LAN
  # hosts (e.g. felix) so they can be pulled at build time via pkgs.fetchurl
  # instead of living in the config repo. Plain HTTP is fine here: integrity is
  # guaranteed by the Nix hash on each fetch, and access is LAN-only.

  systemd.tmpfiles.rules = [
    "d ${serveRoot} 0755 root root - -"
  ];

  services.nginx = {
    enable = true;

    virtualHosts."garfield-appimages" = {
      default = true;
      listen = [
        { addr = serveAddr; port = servePort; }
      ];
      root = serveRoot;
      extraConfig = ''
        autoindex on;
        ${lanAllowRules}
        deny all;
      '';
    };
  };

  # Open the file-host port on the LAN interface only (same interface n8n uses).
  networking.firewall.interfaces."eno1".allowedTCPPorts = [ servePort ];
}
