{ config, pkgs, lib, ... }:

let
  pythonEnv = pkgs.python3.withPackages (ps: with ps; [
    pillow
    numpy
    opencv4
  ]);

  garageImageAnalyzer = pkgs.writeShellScriptBin "garage-image-analyzer" ''
    exec ${pythonEnv}/bin/python3 ${./scripts/analyze_garage.py} "$@"
  '';
in
{
  # ========================================
  # Home Assistant Configuration
  # ========================================
  # Smart home automation with:
  # - Unifi Protect camera integration
  # - Hourly garage camera snapshot analysis (brightness, door, car, motion)
  # - Python/OpenCV-based image processing
  #
  # After first deploy, create /var/lib/hass/garage-analysis-config.json
  # with calibrated door_region and parking_region values for your camera.
  # See modules/nixos/scripts/analyze_garage.py for config format.

  services.home-assistant = {
    enable = true;

    package = pkgs.home-assistant;

    extraComponents = [
      "default_config"
      "met"
      "esphome"

      # Camera and media
      "stream"
      "ffmpeg"

      # Unifi integration
      "unifiprotect"

      # Lutron
      "lutron"

      # Scripting
      "shell_command"
    ];

    extraPackages = python3Packages: with python3Packages; [
      pillow
      numpy
      opencv4
      uiprotect
      aiounifi
    ];

    config = {
      default_config = {};

      homeassistant = {
        name = "Garfield Home";
        latitude = 38.244481;
        longitude = -85.688943;
        elevation = 142;
        unit_system = "us_customary";
        time_zone = "America/Kentucky/Louisville";
      };

      http = {
        server_host = "0.0.0.0";
        server_port = 8123;
      };

      # The analyzer operates on a fixed "latest.jpg" path so there is no
      # timestamp-mismatch between the snapshot and analysis actions.
      shell_command = {
        analyze_garage_camera = "${garageImageAnalyzer}/bin/garage-image-analyzer /var/lib/hass/garage-snapshots/latest.jpg";
      };

      automation = [
        {
          alias = "Hourly Garage Camera Analysis";
          description = "Capture and analyze garage camera image every hour";
          trigger = [
            {
              platform = "time_pattern";
              minutes = "0";
            }
          ];
          action = [
            {
              service = "camera.snapshot";
              data = {
                entity_id = "camera.garage";
                filename = "/var/lib/hass/garage-snapshots/latest.jpg";
              };
            }
            {
              delay = "00:00:02";
            }
            {
              service = "shell_command.analyze_garage_camera";
            }
          ];
        }
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [ 8123 ];

  systemd.services.home-assistant = {
    serviceConfig = {
      ExecStartPre = [
        "${pkgs.coreutils}/bin/mkdir -p /var/lib/hass/garage-snapshots"
      ];
    };
  };

  environment.systemPackages = [ garageImageAnalyzer ];
}
