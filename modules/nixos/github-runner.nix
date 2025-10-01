{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.github-runners-lab;

  # Configuration for multiple runners
  inherit (cfg) runnerCount;
  runnerNumbers = lib.range 1 runnerCount;

  # Service user for CI processes
  ciUser = "lab-ci";
  ciGroup = "lab-ci";

  # Custom GitHub runner image with PHP 8.2
  runnerImage = "github-runner-php82:latest";

  # Generate systemd services for each runner
  generateRunnerService = runnerNumber: {
    "github-runner-${toString runnerNumber}" = {
      description = "GitHub Action Runner ${toString runnerNumber}";
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
          "RUNNER_NAME=lab-runner-${toString runnerNumber}"
          "RUNNER_WORKDIR=/tmp/runner-work-${toString runnerNumber}"
          "DOCKER_HOST=unix:///var/run/docker.sock"
        ];

        ExecStartPre = pkgs.writeShellScript "github-runner-${toString runnerNumber}-pre" ''
          # Stop and remove any existing runner container
          ${pkgs.docker}/bin/docker stop github-runner-${toString runnerNumber} 2>/dev/null || true
          ${pkgs.docker}/bin/docker rm github-runner-${toString runnerNumber} 2>/dev/null || true
        '';

        ExecStart = pkgs.writeShellScript "github-runner-${toString runnerNumber}-start" ''
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
          RUNNER_NAME="lab-runner-${toString runnerNumber}"

          # Build docker run command based on whether GITHUB_REPO is set
          if [ -z "$GITHUB_REPO" ]; then
            echo "Starting organization runner $RUNNER_NAME for $GITHUB_OWNER"
            ${pkgs.docker}/bin/docker run --rm \
              --name github-runner-${toString runnerNumber} \
              --network lab-net \
              --privileged \
              -v /var/run/docker.sock:/var/run/docker.sock \
              -v /tmp/runner-work-${toString runnerNumber}:/tmp/runner-work-${toString runnerNumber} \
              -e RUNNER_NAME="$RUNNER_NAME" \
              -e ORG_NAME="$GITHUB_OWNER" \
              -e ACCESS_TOKEN="$ACCESS_TOKEN" \
              -e RUNNER_WORKDIR="/tmp/runner-work-${toString runnerNumber}" \
              -e LABELS="self-hosted,docker,lab,runner-${toString runnerNumber},php,nodejs,playwright,php82" \
              -e RUNNER_SCOPE="org" \
              ${runnerImage}
          else
            echo "Starting repository runner $RUNNER_NAME for $GITHUB_OWNER/$GITHUB_REPOSITORY"
            ${pkgs.docker}/bin/docker run --rm \
              --name github-runner-${toString runnerNumber} \
              --network lab-net \
              --privileged \
              -v /var/run/docker.sock:/var/run/docker.sock \
              -v /tmp/runner-work-${toString runnerNumber}:/tmp/runner-work-${toString runnerNumber} \
              -e RUNNER_NAME="$RUNNER_NAME" \
              -e REPO_URL="https://github.com/$GITHUB_OWNER/$GITHUB_REPOSITORY" \
              -e ACCESS_TOKEN="$ACCESS_TOKEN" \
              -e RUNNER_WORKDIR="/tmp/runner-work-${toString runnerNumber}" \
              -e LABELS="self-hosted,docker,lab,runner-${toString runnerNumber},php,nodejs,playwright,php82" \
              -e RUNNER_SCOPE="repo" \
              ${runnerImage}
          fi
        '';

        ExecStop = pkgs.writeShellScript "github-runner-${toString runnerNumber}-stop" ''
          ${pkgs.docker}/bin/docker stop github-runner-${toString runnerNumber} || true
          ${pkgs.docker}/bin/docker rm github-runner-${toString runnerNumber} || true
        '';
      };
    };
  };

  # Generate all runner services
  runnerServices = lib.foldr (a: b: a // b) {} (map generateRunnerService runnerNumbers);

in
{
  options.services.github-runners-lab = {
    enable = mkEnableOption "GitHub Action Runners for lab environment";

    runnerCount = mkOption {
      type = types.int;
      default = 4;
      description = "Number of GitHub runners to create";
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

            # Create and fix permissions for work directories
            for i in {1..${toString runnerCount}}; do
              mkdir -p /tmp/runner-work-$i
              chmod 777 /tmp/runner-work-$i
              echo "Created/fixed permissions for /tmp/runner-work-$i"
            done

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
        noto-fonts-emoji
        liberation_ttf
        fira-code
        fira-code-symbols
        dejavu_fonts
        ubuntu_font_family
      ];
    };
  };
}
