{ config, pkgs, lib, ... }:

let
  domain = "n8n.dlyons.dev";
in
{
  # ========================================
  # n8n Workflow Automation
  # ========================================
  # Self-hosted workflow automation for the GTM pipeline.
  # Web UI: https://n8n.dlyons.dev (internal via Lab VLAN)
  # Webhooks: https://n8n.dlyons.dev/webhook/* (external via DMZ VLAN)
  #
  # Data stored in PostgreSQL (n8n database).
  # Encryption key auto-generated on first boot at /var/lib/n8n-secrets/encryption.key
  #
  # Network topology:
  #   WAN :443 → UDM port forward → 10.0.10.134:443 (eno1) → nginx → localhost:5678 (n8n)
  #   Lab clients → 10.0.10.134:443 → nginx → localhost:5678 (n8n)
  #
  # UDM Pro requirements:
  #   1. Port forward: WAN 443 → 10.0.10.134:443
  #   2. Port forward: WAN 80  → 10.0.10.134:80  (ACME HTTP-01 challenge)
  #   3. Firewall rule: Allow TCP, External → Internal, dst 10.0.10.134, ports 80,443

  services.n8n = {
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

  # Nginx reverse proxy with TLS termination
  services.nginx = {
    enable = true;

    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;

    virtualHosts.${domain} = {
      forceSSL = true;
      enableACME = true;

      locations."/" = {
        proxyPass = "http://127.0.0.1:5678";
        proxyWebsockets = true;
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
