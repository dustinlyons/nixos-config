{ config, pkgs, lib, ... }:
{
  # ========================================
  # Atlas webhook ingress (push, not poll)
  # ========================================
  # Public HTTPS endpoint that lets external providers (GitHub today; Linear/
  # Attio/n8n next) PUSH events to Atlas instead of Atlas polling for them.
  # Atlas's receiver (core/webhooks.ts) binds 127.0.0.1:8788 and verifies the
  # per-request HMAC signature; a verified event is a doorbell, not data — it
  # just kicks the relevant beat, whose own precheck re-reads authoritative
  # state. nginx terminates TLS and forwards only the /hook/ path.
  #
  #   WAN :443 → UDM → garfield :443 → nginx (SNI: hooks.dlyons.dev) → 127.0.0.1:8788
  #
  # nginx itself (enable, ACME terms, the shared "webhooks" rate-limit zone, the
  # access log, and the fail2ban jails that ban repeat 403/404 scanners) is
  # configured alongside n8n in n8n.nix; this adds one more virtualHost, which
  # the NixOS module system merges in. Ports 80/443 are already open on the lab
  # interface (n8n.nix), so no firewall change is needed.
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
        proxyPass = "http://127.0.0.1:8788";
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

  systemd.user.services.atlas-devenv = {
    description = "Start atlas server in tmux";
    wantedBy = [ "default.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.writeShellScript "start-atlas" ''
        ATLAS_DIR=/home/dustin/.local/share/src/atlas

        # Install dependencies if needed
        if [ ! -d "$ATLAS_DIR/node_modules" ]; then
          cd "$ATLAS_DIR" && ${pkgs.nix}/bin/nix develop /home/dustin/.local/share/src/conductly --impure -c bash -c "bun install"
        fi

        cd /home/dustin/.local/share/src/conductly
        export TMUX_TMPDIR=/run/user/1000
        # App env vars must be set on the innermost shell that execs bun (NOT just
        # exported here) so they survive the tmux + `nix develop` layers — this is
        # the same `env VAR=val` form the repo's start.sh uses. Keep in sync with
        # start.sh. ATLAS_AUTO_MERGE=1 lets the merge-prs beat auto-merge Atlas-
        # labeled PRs into develop once CI + bot review settle (release PRs to main
        # are never auto-merged). ATLAS_STUDIO=1 enables the 3D studio front door
        # (port 8787); SLACK_ALLOWED_SENDERS is a comma-separated allowlist of Slack
        # user IDs that REPLACES the default (must include Dustin's own ID). Budget
        # caps are OFF by default in code (core/config.ts) — set ATLAS_ENABLE_QUOTA=1
        # to enforce.
        ${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-atlas new-session -d -s atlas "${pkgs.nix}/bin/nix develop --impure -c bash -c \"env ATLAS_AUTO_MERGE=1 ATLAS_STUDIO=1 SLACK_ALLOWED_SENDERS=U06EL4RDNSH,U072SUTF8NB bun run $ATLAS_DIR/server.ts\""
      ''}";
      ExecStop = "${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-atlas kill-session -t atlas";
      RemainAfterExit = "no";
      Environment = [
        "PATH=/run/current-system/sw/bin:/home/dustin/.nix-profile/bin:/etc/profiles/per-user/dustin/bin:/nix/var/nix/profiles/default/bin:/run/wrappers/bin:/usr/bin:/bin"
      ];
    };
  };
}
