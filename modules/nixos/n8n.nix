{ config, pkgs, lib, ... }:

let
  domain = "dlyons.dev";
  lanCidrs = [
    "10.0.10.0/24"    # Server VLAN
    "192.168.0.0/24"  # Client VLAN
    "127.0.0.1/32"    # Localhost
  ];
  lanAllowRules = lib.concatMapStringsSep "\n" (cidr: "allow ${cidr};") lanCidrs;
in
{
  # ========================================
  # n8n Workflow Automation
  # ========================================
  # Self-hosted workflow automation for the GTM pipeline.
  # Web UI: https://dlyons.dev (LAN only — blocked from internet)
  # Webhooks: https://dlyons.dev/webhook/* (public — rate limited)
  #
  # Data stored in PostgreSQL (n8n database).
  # Encryption key auto-generated on first boot at /var/lib/n8n-secrets/encryption.key
  #
  # Network topology:
  #   WAN :443 → UDM port forward → 10.0.10.134:443 (eno1) → nginx → localhost:5678 (n8n)
  #   Lab clients → 10.0.10.134:443 → nginx → localhost:5678 (n8n)
  #
  # Security:
  #   - Only /webhook/* is reachable from the internet
  #   - n8n UI, API, and all other paths are LAN-only
  #   - Webhook endpoints are rate-limited (5 req/sec burst 10)
  #   - Security headers set on all responses

  services = {
    n8n = {
      enable = true;
      openFirewall = false;

      environment = {
        # Bind to localhost only — nginx handles external traffic
        N8N_HOST = "127.0.0.1";

        # Public webhook URL
        WEBHOOK_URL = "https://${domain}";

        # PostgreSQL connection
        DB_TYPE = "postgresdb";
        DB_POSTGRESDB_HOST = "/run/postgresql";
        DB_POSTGRESDB_DATABASE = "n8n";
        DB_POSTGRESDB_USER = "n8n";

        # Encryption key for stored credentials
        N8N_ENCRYPTION_KEY_FILE = "/var/lib/n8n-secrets/encryption.key";
      };
    };
  };

  # Nginx reverse proxy with TLS termination
  services.nginx = {
    enable = true;

    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;

    # Access logging for monitoring and fail2ban
    appendHttpConfig = ''
      log_format gtm '$remote_addr - [$time_local] "$request" $status $body_bytes_sent "$http_referer" "$http_user_agent" rt=$request_time';
      map $request_uri $loggable {
        ~^/healthz 0;
        default    1;
      }
      access_log /var/log/nginx/access.log gtm if=$loggable;
      limit_req_zone $binary_remote_addr zone=webhooks:10m rate=5r/s;
    '';

    virtualHosts.${domain} = {
      forceSSL = true;
      enableACME = true;

      # Security headers
      extraConfig = ''
        add_header X-Frame-Options "DENY" always;
        add_header X-Content-Type-Options "nosniff" always;
        add_header X-XSS-Protection "1; mode=block" always;
        add_header Referrer-Policy "strict-origin-when-cross-origin" always;
      '';

      locations = {
        # Webhooks — public, rate-limited
        "/webhook/" = {
          proxyPass = "http://127.0.0.1:5678";
          proxyWebsockets = false;
          extraConfig = ''
            limit_req zone=webhooks burst=10 nodelay;
            limit_req_status 429;
          '';
        };

        # Webhook test paths (n8n uses /webhook-test/ for manual testing)
        "/webhook-test/" = {
          proxyPass = "http://127.0.0.1:5678";
          proxyWebsockets = false;
          extraConfig = ''
            ${lanAllowRules}
            deny all;
          '';
        };

        # n8n REST API — LAN only
        "/api/" = {
          proxyPass = "http://127.0.0.1:5678";
          proxyWebsockets = false;
          extraConfig = ''
            ${lanAllowRules}
            deny all;
          '';
        };

        # n8n UI and everything else — LAN only
        "/" = {
          proxyPass = "http://127.0.0.1:5678";
          proxyWebsockets = true;
          extraConfig = ''
            ${lanAllowRules}
            deny all;
          '';
        };
      };
    };
  };

  # Let's Encrypt
  security.acme = {
    acceptTerms = true;
    defaults.email = "dustin@dlyons.dev";
  };

  # Firewall — allow HTTPS + ACME on the Lab interface
  networking.firewall.interfaces."eno1".allowedTCPPorts = [ 80 443 ];

  # Monitoring tools
  environment.systemPackages = [ pkgs.goaccess ];

  # fail2ban — auto-ban abusive IPs
  services.fail2ban = {
    enable = true;
    bantime = "1h";
    bantime-increment = {
      enable = true;
      maxtime = "168h"; # Cap repeat offender bans at 1 week
    };
    jails = {
      # Ban external IPs that get denied/not-found (scanners, exploit probes).
      # Uses a custom filter because the stock nginx-botsearch doesn't work:
      #   - Its journalmatch reads systemd journal, but access logs go to file
      #   - Its regex expects combined log format (with username field)
      #   - It only matches 404s, but deny rules return 403s
      nginx-deny = ''
        enabled = true
        filter = nginx-deny-gtm
        logpath = /var/log/nginx/access.log
        maxretry = 5
        findtime = 60
        ignoreip = 127.0.0.0/8 ::1 10.0.10.0/24 192.168.0.0/24
      '';
      # Ban IPs that hit webhook rate limits (reads nginx error log from journal)
      nginx-ratelimit = ''
        enabled = true
        filter = nginx-limit-req
        backend = systemd
        maxretry = 10
        findtime = 60
        ignoreip = 127.0.0.0/8 ::1 10.0.10.0/24 192.168.0.0/24
      '';
      # SSH brute force protection (uses built-in NixOS sshd jail)
      sshd.settings = {
        enabled = true;
        maxretry = 5;
        findtime = 300;
        ignoreip = "127.0.0.0/8 ::1 10.0.10.0/24 192.168.0.0/24";
      };
    };
  };

  # Custom fail2ban filter for the GTM access log format.
  # Matches 400 (malformed/binary requests), 403 (denied by ACL), 404 (not found).
  # No journalmatch — forces file-based reading from the access log.
  environment.etc."fail2ban/filter.d/nginx-deny-gtm.conf".text = ''
    [Definition]
    failregex = ^<HOST> - \[.*?\] "[^"]*" (?:400|403|404) \d+
    ignoreregex =
    datepattern = ^[^\[]*\[({DATE})
  '';

  # PostgreSQL for n8n data storage
  services.postgresql = {
    enable = true;
    ensureDatabases = [ "n8n" ];
    ensureUsers = [
      {
        name = "n8n";
        ensureDBOwnership = true;
      }
    ];
  };

  # Ensure n8n starts after PostgreSQL
  systemd.services.n8n = {
    after = [ "postgresql.service" "n8n-encryption-key.service" ];
    requires = [ "postgresql.service" "n8n-encryption-key.service" ];

    # n8n 2.x task runner spawns `node` as a child process.
    # Without PATH to nodejs, it fails with "spawn node ENOENT".
    path = [ pkgs.nodejs ];
  };

  # Generate encryption key on first boot if it doesn't exist
  systemd.services.n8n-encryption-key = {
    description = "Generate n8n encryption key";
    wantedBy = [ "multi-user.target" ];
    unitConfig.ConditionPathExists = "!/var/lib/n8n-secrets/encryption.key";
    serviceConfig = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /var/lib/n8n-secrets";
      ExecStart = "${pkgs.bash}/bin/bash -c '${pkgs.openssl}/bin/openssl rand -hex 32 > /var/lib/n8n-secrets/encryption.key'";
      ExecStartPost = "${pkgs.coreutils}/bin/chmod 600 /var/lib/n8n-secrets/encryption.key";
      RemainAfterExit = true;
    };
  };
}
