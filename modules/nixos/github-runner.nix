{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.github-runners-lab;

  # Service user for CI processes
  ciUser = "lab-ci";
  ciGroup = "lab-ci";

  # Custom GitHub runner image with PHP 8.2
  runnerImage = "github-runner-php82:latest";

  # The runner fleet is split into two label-partitioned pools so that a deploy
  # never waits behind a long pest/playwright job. GitHub Actions has no
  # priority queue and cannot preempt a running job, so reserved capacity is
  # the only way to make deploys start immediately:
  #
  #   `ci`     -> PR test workloads (pest, playwright, preview deploys)
  #   `deploy` -> staging/production deploys ONLY, so a slot is always idle
  #
  # The pools are exclusive: a ci runner carries no `deploy` label and vice
  # versa. Every runner also carries the implicit `self-hosted` label, so a
  # workflow using a bare `runs-on: self-hosted` can STILL land on the deploy
  # runner — conductly's CI workflows must target `[self-hosted, ci]` for the
  # reservation to hold.
  ciRunners = map (n: {
    id = toString n;
    name = "lab-runner-${toString n}";
    labels = "self-hosted,docker,lab,runner-${toString n},php,nodejs,playwright,php82,ci";
  }) (lib.range 1 cfg.runnerCount);

  # Deploy runners share the same image (composer/php/bin maestro all work) but
  # advertise a deliberately narrow label set, so nothing except the deploy
  # workflows can select them.
  deployRunners = map (n: {
    id = "deploy-${toString n}";
    name = "lab-deploy-runner-${toString n}";
    labels = "self-hosted,docker,lab,deploy";
  }) (lib.range 1 cfg.deployRunnerCount);

  allRunners = ciRunners ++ deployRunners;

  # Generate systemd services for each runner
  generateRunnerService = runner: {
    "github-runner-${runner.id}" = {
      description = "GitHub Action Runner ${runner.id}";
      after = [ "docker-networks.service" "network.target" "docker-permissions.service" ];
      wants = [ "docker-networks.service" "docker-permissions.service" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "30";
        User = ciUser;
        Group = "docker";

        # Set environment variables for the runner
        Environment = [
          "RUNNER_NAME=${runner.name}"
          "RUNNER_WORKDIR=/tmp/runner-work-${runner.id}"
          "DOCKER_HOST=unix:///var/run/docker.sock"
        ];

        ExecStartPre = pkgs.writeShellScript "github-runner-${runner.id}-pre" ''
          # Stop and remove any existing runner container
          ${pkgs.docker}/bin/docker stop github-runner-${runner.id} 2>/dev/null || true
          ${pkgs.docker}/bin/docker rm github-runner-${runner.id} 2>/dev/null || true
        '';

        ExecStart = pkgs.writeShellScript "github-runner-${runner.id}-start" ''
          # Load GitHub token from file
          if [ ! -f /etc/github-runner/token ]; then
            echo "ERROR: GitHub token not found at /etc/github-runner/token"
            exit 1
          fi

          if [ ! -f /etc/github-runner/config ]; then
            echo "ERROR: GitHub config not found at /etc/github-runner/config"
            exit 1
          fi

          # Source the configuration
          source /etc/github-runner/config

          ACCESS_TOKEN=$(cat /etc/github-runner/token)

          # Override the runner name for this instance
          RUNNER_NAME="${runner.name}"

          # Build docker run command based on whether GITHUB_REPO is set
          if [ -z "$GITHUB_REPO" ]; then
            echo "Starting organization runner $RUNNER_NAME for $GITHUB_OWNER"
            ${pkgs.docker}/bin/docker run --rm \
              --name github-runner-${runner.id} \
              --network lab-net \
              --privileged \
              -v /var/run/docker.sock:/var/run/docker.sock \
              -v /tmp/runner-work-${runner.id}:/tmp/runner-work-${runner.id} \
              -e RUNNER_NAME="$RUNNER_NAME" \
              -e ORG_NAME="$GITHUB_OWNER" \
              -e ACCESS_TOKEN="$ACCESS_TOKEN" \
              -e RUNNER_WORKDIR="/tmp/runner-work-${runner.id}" \
              -e LABELS="${runner.labels}" \
              -e RUNNER_SCOPE="org" \
              ${runnerImage}
          else
            echo "Starting repository runner $RUNNER_NAME for $GITHUB_OWNER/$GITHUB_REPOSITORY"
            ${pkgs.docker}/bin/docker run --rm \
              --name github-runner-${runner.id} \
              --network lab-net \
              --privileged \
              -v /var/run/docker.sock:/var/run/docker.sock \
              -v /tmp/runner-work-${runner.id}:/tmp/runner-work-${runner.id} \
              -e RUNNER_NAME="$RUNNER_NAME" \
              -e REPO_URL="https://github.com/$GITHUB_OWNER/$GITHUB_REPOSITORY" \
              -e ACCESS_TOKEN="$ACCESS_TOKEN" \
              -e RUNNER_WORKDIR="/tmp/runner-work-${runner.id}" \
              -e LABELS="${runner.labels}" \
              -e RUNNER_SCOPE="repo" \
              ${runnerImage}
          fi
        '';

        ExecStop = pkgs.writeShellScript "github-runner-${runner.id}-stop" ''
          ${pkgs.docker}/bin/docker stop github-runner-${runner.id} || true
          ${pkgs.docker}/bin/docker rm github-runner-${runner.id} || true
        '';
      };
    };
  };

  # Generate all runner services
  runnerServices = lib.foldr (a: b: a // b) {} (map generateRunnerService allRunners);

in
{
  options.services.github-runners-lab = {
    enable = mkEnableOption "GitHub Action Runners for lab environment";

    runnerCount = mkOption {
      type = types.int;
      default = 4;
      description = "Number of general-purpose CI runners to create (label: ci)";
    };

    deployRunnerCount = mkOption {
      type = types.int;
      default = 1;
      description = ''
        Number of deploy-only runners to create (label: deploy). These are
        reserved for the staging/production deploy workflows so a deploy never
        queues behind a long CI job. Deploys are SSH/rsync-bound and short, so
        one is normally enough.
      '';
    };

    organization = mkOption {
      type = types.str;
      default = "conductly";
      description = "GitHub organization name";
    };
  };

  config = mkIf cfg.enable {
    # Enable Docker
    virtualisation.docker = {
      enable = true;
    };

    # Dedicated CI service user for GitHub runners
    users.users.${ciUser} = {
      isSystemUser = true;
      description = "Lab CI Service Account";
      group = ciGroup;
      extraGroups = [ "docker" ];
      home = "/var/lib/lab-ci";
      createHome = true;
    };

    # Create a group for the CI user
    users.groups.${ciGroup} = {};

    # Add all the systemd services (docker-networks + all runners + permissions)
    systemd.services = runnerServices // {
      # Docker networks service
      docker-networks = {
        description = "Create Docker networks";
        after = [ "docker.service" ];
        wants = [ "docker.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = pkgs.writeShellScript "create-docker-networks" ''
            # Create lab-net network if it doesn't exist
            if ! ${pkgs.docker}/bin/docker network inspect lab-net >/dev/null 2>&1; then
              ${pkgs.docker}/bin/docker network create \
                --driver bridge \
                --subnet 172.18.0.0/24 \
                lab-net
            fi
          '';
          ExecStop = pkgs.writeShellScript "remove-docker-networks" ''
            # Remove lab-net network if it exists
            if ${pkgs.docker}/bin/docker network inspect lab-net >/dev/null 2>&1; then
              ${pkgs.docker}/bin/docker network rm lab-net || true
            fi
          '';
        };
      };

      # Docker permissions service (runs as root)
      docker-permissions = {
        description = "Fix Docker socket permissions for GitHub runners";
        after = [ "docker.service" ];
        wants = [ "docker.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = pkgs.writeShellScript "fix-docker-permissions" ''
            # Make docker socket accessible to docker group
            chmod 666 /var/run/docker.sock
            echo "Docker socket permissions fixed"

            # Create and set ownership for CI user home directory
            mkdir -p /var/lib/lab-ci
            chown ${ciUser}:${ciGroup} /var/lib/lab-ci
            chmod 750 /var/lib/lab-ci

            # Create and fix permissions for work directories (ci + deploy pools)
            ${lib.concatMapStringsSep "\n" (runner: ''
              mkdir -p /tmp/runner-work-${runner.id}
              chmod 777 /tmp/runner-work-${runner.id}
              echo "Created/fixed permissions for /tmp/runner-work-${runner.id}"
            '') allRunners}

            # Ensure github-runner config directory has correct permissions
            if [ -d /etc/github-runner ]; then
              # Make token readable only by root and lab-ci group
              if [ -f /etc/github-runner/token ]; then
                chown root:${ciGroup} /etc/github-runner/token
                chmod 640 /etc/github-runner/token
              fi
              # Config can be read by the group
              if [ -f /etc/github-runner/config ]; then
                chown root:${ciGroup} /etc/github-runner/config
                chmod 640 /etc/github-runner/config
              fi
            fi
          '';
        };
      };
    };

    # Optimize system limits for containers
    systemd.settings.Manager = {
      DefaultLimitNOFILE = 65536;
    };

    # Optimize memory settings for multiple runners
    boot.kernel.sysctl = {
      "vm.max_map_count" = 262144;
      "fs.file-max" = 2097152;
      "kernel.pid_max" = 4194304;
    };

    # Fonts for Playwright browser rendering
    fonts = {
      enableDefaultPackages = true;
      packages = with pkgs; [
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-color-emoji
        liberation_ttf
        fira-code
        fira-code-symbols
        dejavu_fonts
        ubuntu-classic
      ];
    };
  };
}
