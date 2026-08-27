{ config, pkgs, lib, ... }:

let
  domain = "jenkins.dlyons.dev";

  # Same LAN allowlist n8n.nix uses. Kept local rather than shared because the
  # two modules are independently importable; if a third service needs it,
  # promote it to a shared module then.
  lanCidrs = [
    "10.0.10.0/24"    # Server VLAN
    "192.168.0.0/24"  # Client VLAN
    "127.0.0.1/32"    # Localhost
  ];
  lanAllowRules = lib.concatMapStringsSep "\n" (cidr: "allow ${cidr};") lanCidrs;
in
{
  # ========================================
  # Jenkins CI
  # ========================================
  # Web UI: https://jenkins.dlyons.dev (LAN only — deny all from the internet)
  #
  # Network topology:
  #   Lab clients -> garfield :443 -> nginx (SNI: jenkins.dlyons.dev)
  #                                     -> 127.0.0.1:8080 (jenkins)
  #
  # Jenkins itself binds localhost only, so the nginx ACL is the sole access
  # path — there is no port to reach around it even from the LAN.
  #
  # PREREQUISITE — public DNS: `jenkins.dlyons.dev` must have an A record
  # pointing at the WAN address (70.228.88.181, same as dlyons.dev and
  # hooks.dlyons.dev) before Let's Encrypt can issue via http-01. Until that
  # record exists the ACME order fails and nginx serves the self-signed
  # fallback cert — the vhost still works, browsers just warn. The record is
  # only needed for issuance; the vhost is still LAN-only by ACL.
  #
  # nginx itself (enable, ACME terms, the access log, fail2ban jails) is
  # configured alongside n8n in n8n.nix; this adds one more virtualHost, which
  # the module system merges in. Ports 80/443 are already open (n8n.nix), so
  # no firewall change is needed here.
  #
  # State lives in /var/lib/jenkins and is NOT reproducible by nixos-rebuild —
  # jobs, plugins, credentials, and build history all live there. Back it up.

  services.jenkins = {
    enable = true;

    # Localhost only — nginx terminates TLS and enforces the LAN ACL.
    listenAddress = "127.0.0.1";
    port = 8080;

    # Docker builds. Docker is already enabled on this host for the GitHub
    # runners; this makes the socket usable from `agent { docker { ... } }`
    # pipelines. Note this is root-equivalent on the box — the same tradeoff
    # the lab-ci user already takes in github-runner.nix.
    extraGroups = [ "docker" ];

    # Toolchain on the jenkins process PATH. Jobs inherit this, so freestyle
    # steps and `sh` pipeline steps can call these without a tool installer.
    packages = [
      # Shell + archive basics. The jenkins module sets PATH to exactly this
      # list, so without coreutils et al. even `sh 'ls'` fails.
      pkgs.bashInteractive
      pkgs.coreutils
      pkgs.findutils
      pkgs.diffutils
      pkgs.gnugrep
      pkgs.gnused
      pkgs.gawk
      pkgs.gnutar
      pkgs.gzip
      pkgs.bzip2
      pkgs.xz
      pkgs.zip
      pkgs.unzip
      pkgs.which

      # SCM + network
      pkgs.git
      pkgs.openssh
      pkgs.curl
      pkgs.wget

      # Same JDK Jenkins itself runs on, so agents and JVM build tools don't
      # pull a second jdk closure into the system.
      config.services.jenkins.javaPackage

      # Language toolchains
      pkgs.nodejs
      pkgs.php82
      pkgs.php82Packages.composer

      # Flake builds (`nix build`, `nix run`). Requires the allowed-users entry
      # below or the daemon refuses the connection.
      pkgs.nix

      # The Docker Pipeline plugin shells out to the `docker` CLI.
      pkgs.docker
    ];

    # Cap the heap. Garfield is a shared box — 3 GitHub runners, n8n,
    # PostgreSQL, and Home Assistant all live here — and the JVM's default
    # MaxRAMPercentage would let Jenkins claim a quarter of system memory it
    # will never need for a lab-scale controller.
    extraJavaOptions = [ "-Xmx2g" ];

    # Puts `jenkins-cli` on PATH with JENKINS_URL preset to the local listener.
    withCLI = true;
  };

  # Docker is already enabled by github-runner.nix on this host; mkDefault so
  # this module is also correct standalone and never conflicts with that
  # module's explicit `enable = true`.
  virtualisation.docker.enable = lib.mkDefault true;

  # garfield restricts nix-daemon access to a single user (see
  # hosts/nixos/garfield/default.nix). Without this, any pipeline calling
  # `nix build` dies with "user is not allowed to connect to the Nix daemon".
  # List options merge across modules, so this appends rather than replaces.
  nix.settings.allowed-users = [ "jenkins" ];

  # The upstream module hardens the unit with PrivateUsers=true, which drops
  # the process into a user namespace where unmapped supplementary groups
  # collapse to nobody — taking the `docker` group membership with them. The
  # socket happens to be chmod 666 on this host (github-runner.nix's
  # docker-permissions.service), so access would survive by accident; disable
  # the namespace so it survives on purpose and doesn't break if that service
  # ever goes away. Every other hardening knob upstream sets is left alone.
  systemd.services.jenkins.serviceConfig.PrivateUsers = lib.mkForce false;

  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;

    extraConfig = ''
      add_header X-Content-Type-Options "nosniff" always;
      add_header Referrer-Policy "strict-origin-when-cross-origin" always;

      # Plugin .hpi uploads and build artifacts exceed nginx's 1m default.
      client_max_body_size 100m;
    '';

    locations."/" = {
      proxyPass = "http://127.0.0.1:8080";
      # Agent connections and the live console log both use WebSockets.
      proxyWebsockets = true;
      extraConfig = ''
        ${lanAllowRules}
        deny all;

        # Jenkins streams request bodies (CLI, file parameters, agent
        # traffic); buffering them in nginx stalls those endpoints.
        proxy_request_buffering off;
        proxy_read_timeout 90s;
      '';
    };
  };
}
