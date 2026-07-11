{ config, pkgs, lib, ... }:
{
  # ========================================
  # Atlas webhook ingress (garfield = TLS front door; Atlas lives on FELIX)
  # ========================================
  # Public HTTPS endpoint that lets external providers (GitHub today) PUSH
  # events to Atlas instead of Atlas polling. Atlas's receiver
  # (atlas/core/webhooks.ts) runs on felix and verifies the per-request HMAC
  # signature; a verified event is a doorbell, not data — it just kicks the
  # relevant beat, whose own free precheck re-reads authoritative state.
  #
  #   WAN :443 → UDM → garfield :443 → nginx (SNI: hooks.dlyons.dev)
  #                                      → felix 192.168.0.169:8788
  #
  # History: the original vhost lived in atlas.nix and proxied to 127.0.0.1 —
  # correct when Atlas ran on garfield. Atlas migrated to felix (2026-06-27)
  # and atlas.nix was un-imported, which silently dropped the vhost: SNI for
  # hooks.dlyons.dev then fell through to the default (dlyons.dev) cert and
  # every GitHub delivery failed the TLS handshake. This module restores the
  # vhost on garfield (which still owns the WAN 443 forward) and points it at
  # felix instead.
  #
  # NOTE: felix's LAN address is a DHCP lease — if it changes, update the
  # proxyPass here (better: give felix a DHCP reservation on the UDM).
  # Felix binds the listener beyond localhost via ATLAS_WEBHOOK_HOST=0.0.0.0
  # (set in systemd.nix's start-atlas env line).
  #
  # nginx itself (enable, ACME terms, the shared "webhooks" rate-limit zone,
  # the access log, and the fail2ban jails) is configured alongside n8n in
  # n8n.nix; this adds one more virtualHost, which the NixOS module system
  # merges in. Ports 80/443 are already open/forwarded (n8n.nix), so no
  # firewall change is needed; port 80 also serves the ACME http-01 challenge
  # for this vhost's own hooks.dlyons.dev certificate.
  services.nginx.virtualHosts."hooks.dlyons.dev" = {
    forceSSL = true;
    enableACME = true;

    extraConfig = ''
      add_header X-Frame-Options "DENY" always;
      add_header X-Content-Type-Options "nosniff" always;
      add_header Referrer-Policy "strict-origin-when-cross-origin" always;
    '';

    locations = {
      # The only public surface. Atlas verifies the HMAC signature on the body;
      # nginx adds rate limiting (the shared "webhooks" zone from n8n.nix) as a
      # cheap DoS guard in front of it.
      "/hook/" = {
        proxyPass = "http://192.168.0.169:8788";
        proxyWebsockets = false;
        extraConfig = ''
          limit_req zone=webhooks burst=10 nodelay;
          limit_req_status 429;
        '';
      };

      # Anything else is a scanner — 404 (fail2ban's nginx-deny jail in n8n.nix
      # bans repeat offenders on 403/404).
      "/" = {
        return = "404";
      };
    };
  };
}
