{ config, pkgs, lib, ... }:

let
  domain = "jenkins.dlyons.dev";

  # garfield's address on the server VLAN, and the port the web UI is served
  # on. Deliberately NOT 443 — see the topology note below.
  lanIp = "10.0.10.134";
  lanPort = 8443;

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
  # Web UI: https://jenkins.dlyons.dev:8443 (LAN only)
  #
  # Network topology:
  #   Lab clients -> garfield :8443 -> nginx (SNI: jenkins.dlyons.dev)
  #                                      -> 127.0.0.1:8080 (jenkins)
  #   Let's Encrypt -> UDM :80 -> garfield :80 -> ACME challenge only
  #
  # WHY 8443 AND NOT 443 — this is the whole security model, do not "simplify"
  # it back to 443. The UDM forwards WAN 80/443 to this host for
  # hooks.dlyons.dev, and it source-NATs what it forwards: a request off the
  # internet arrives at nginx with a *LAN* source address. nginx's allow/deny
  # matches on that address, so an `allow 192.168.0.0/24; deny all;` rule
  # accepts the entire internet. This was live for ~10 minutes on 2026-08-27 —
  # the unlocked setup wizard was publicly reachable and served its robots.txt
  # to an off-network fetch — before the vhost moved here.
  #
  # 8443 is not forwarded by the UDM, so the NAT boundary is what keeps this
  # off the internet. That is the same protection Home Assistant on :8123
  # already relies on (see the firewall block in garfield/default.nix). The
  # allow/deny rules below are kept as a second layer: for traffic that
  # reaches this port directly over the LAN the source address is genuine, so
  # they do constrain which VLANs can connect.
  #
  # LAN NAME RESOLUTION: `jenkins.dlyons.dev` resolves publicly to the WAN
  # address, which does not forward 8443 — so a LAN client must resolve it to
  # ${lanIp} instead, or the browser will hang. garfield gets that via the
  # networking.hosts entry below; other machines need a local DNS record on
  # the UDM (Settings -> Routing & Firewall -> DNS) pointing the name at
  # ${lanIp}. Without it, use https://${lanIp}:8443 and accept the name
  # mismatch, or tunnel: ssh -L 8080:127.0.0.1:8080 garfield
  #
  # PREREQUISITE — public DNS: `jenkins.dlyons.dev` must keep an A record
  # pointing at the WAN address (70.228.88.181, same as dlyons.dev and
  # hooks.dlyons.dev). Let's Encrypt reaches the :80 vhost below over that
  # record to issue and renew via http-01. Note this also publishes the
  # hostname to Certificate Transparency logs, so treat the name as public
  # knowledge — obscurity is not part of the model above.
  #
  # nginx itself (enable, ACME terms, the access log, fail2ban jails) is
  # configured alongside n8n in n8n.nix; this adds two more virtualHosts,
  # which the module system merges in. Ports 80/443 are already open
  # (n8n.nix); 8443 is opened per-CIDR below.
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

  # Public :80 — ACME http-01 challenge only. No TLS, no proxy: any path other
  # than /.well-known/acme-challenge/ falls through to nginx's 404. This is
  # the one part of this service the internet is meant to reach.
  services.nginx.virtualHosts = {
    ${domain} = {
      enableACME = true;
    };

    # The actual web UI, on the non-forwarded LAN port. Separate attribute name
    # because two vhosts share one server_name on different ports.
    "${domain}-lan" = {
      serverName = domain;
      onlySSL = true;
      # Reuse the cert the :80 vhost above obtains; do not order a second one.
      useACMEHost = domain;
      listen = [
        { addr = "0.0.0.0"; port = lanPort; ssl = true; }
      ];

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
  };

  # Open 8443 to the LAN VLANs only, matching the per-service source-restricted
  # style garfield already uses for 22 and 8123. extraCommands is types.lines,
  # so this appends to the rules in garfield/default.nix rather than replacing
  # them.
  networking = {
    firewall.extraCommands = lib.concatMapStringsSep "\n"
      (cidr: "iptables -A nixos-fw -p tcp --dport ${toString lanPort} -s ${cidr} -j nixos-fw-accept")
      lanCidrs;

    firewall.extraStopCommands = lib.concatMapStringsSep "\n"
      (cidr: "iptables -D nixos-fw -p tcp --dport ${toString lanPort} -s ${cidr} -j nixos-fw-accept 2>/dev/null || true")
      lanCidrs;

    # So the host serving the cert can also resolve the name it is served
    # under; without this, curl/jenkins-cli on garfield follow public DNS out
    # to the WAN address and hang on the unforwarded port.
    hosts.${lanIp} = [ domain ];
  };
}
