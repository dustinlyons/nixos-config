{ config, pkgs, lib, ... }:

let
  # Python script for garage camera image analysis
  garageImageAnalyzer = pkgs.writeShellScriptBin "garage-image-analyzer" ''
    #!/bin/bash

    # Configuration
    IMAGE_DIR="/var/lib/hass/garage-snapshots"
    TIMESTAMP=$(date +%Y%m%d_%H%M%S)
    IMAGE_PATH="$IMAGE_DIR/garage_$TIMESTAMP.jpg"

    # Ensure directory exists
    mkdir -p "$IMAGE_DIR"

    # The actual image will be passed from Home Assistant
    # Run Python analysis script
    ${pkgs.python3.withPackages(ps: with ps; [
      pillow
      numpy
    ])}/bin/python3 /var/lib/hass/scripts/analyze_garage.py "$1"
  '';
in
{
  # ========================================
  # Home Assistant Configuration
  # ========================================
  # Provides smart home automation with:
  # - Unifi Protect camera integration
  # - Hourly garage camera snapshot analysis
  # - Python-based image processing

  services.home-assistant = {
    enable = true;

    # Use latest version for best Unifi Protect support
    package = pkgs.home-assistant;

    extraComponents = [
      # Core integrations
      "default_config"
      "met"
      "esphome"

      # Camera and media
      "stream"
      "ffmpeg"

      # Unifi integration
      "unifiprotect"

      # Automation and scripting
      "python_script"
      "shell_command"
    ];

    extraPackages = python3Packages: with python3Packages; [
      # Image processing
      pillow
      numpy
      opencv4

      # Unifi Protect
      pyunifiprotect

      # Utilities
      aiounifi
    ];

    config = {
      # Basic configuration
      default_config = {};

      homeassistant = {
        name = "Garfield Home";
        latitude = "!secret latitude";
        longitude = "!secret longitude";
        elevation = "!secret elevation";
        unit_system = "imperial";
        time_zone = "America/Kentucky/Louisville";
      };

      # HTTP interface
      http = {
        server_host = "0.0.0.0";
        server_port = 8123;
        trusted_proxies = [ "127.0.0.1" "::1" ];
      };

      # Enable Python scripts
      python_script = {};

      # Shell commands for image analysis
      shell_command = {
        analyze_garage_camera = "${garageImageAnalyzer}/bin/garage-image-analyzer {{ image_path }}";
      };

      # Automation for hourly garage camera snapshot
      automation = [
        {
          alias = "Hourly Garage Camera Analysis";
          description = "Capture and analyze garage camera image every hour";
          trigger = [
            {
              platform = "time_pattern";
              hours = "*";  # Every hour
              minutes = 0;
            }
          ];
          action = [
            # Take snapshot from camera
            {
              service = "camera.snapshot";
              data = {
                entity_id = "camera.garage";  # Adjust to match your camera entity
                filename = "/var/lib/hass/garage-snapshots/garage_{{ now().strftime('%Y%m%d_%H%M%S') }}.jpg";
              };
            }
            # Wait for file to be written
            {
              delay = "00:00:02";
            }
            # Run analysis script
            {
              service = "shell_command.analyze_garage_camera";
              data = {
                image_path = "/var/lib/hass/garage-snapshots/garage_{{ now().strftime('%Y%m%d_%H%M%S') }}.jpg";
              };
            }
          ];
        }
      ];
    };
  };

  # Open firewall for Home Assistant
  networking.firewall.allowedTCPPorts = [ 8123 ];

  # Ensure Home Assistant has access to image directory
  systemd.services.home-assistant = {
    serviceConfig = {
      # Ensure the garage snapshots directory exists
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /var/lib/hass/garage-snapshots";
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /var/lib/hass/scripts";
    };
  };

  # Create the Python analysis script template
  environment.etc."home-assistant/analyze_garage.py" = {
    text = ''
      #!/usr/bin/env python3
      """
      Garage Camera Image Analysis Script

      This script analyzes images from the garage camera to make assertions.
      Customize the analysis logic below based on your needs.
      """

      import sys
      from PIL import Image
      import numpy as np
      from datetime import datetime

      def analyze_image(image_path):
          """
          Analyze the garage camera image.

          Args:
              image_path: Path to the image file

          Returns:
              Dictionary with analysis results
          """
          try:
              # Load image
              img = Image.open(image_path)
              img_array = np.array(img)

              # Example analyses - customize these for your needs:

              # 1. Check average brightness (detect if lights are on)
              avg_brightness = np.mean(img_array)

              # 2. Check if garage door is open (example: check specific region)
              # You would need to calibrate this based on your camera view
              # door_region = img_array[100:200, 100:200]  # Example coordinates
              # door_open = np.mean(door_region) > threshold

              # 3. Detect motion by comparing with previous image
              # (Would need to store previous image)

              # 4. Check for presence of car (example)
              # car_region = img_array[200:400, 300:500]  # Example coordinates
              # car_present = detect_car(car_region)

              results = {
                  'timestamp': datetime.now().isoformat(),
                  'image_path': image_path,
                  'avg_brightness': float(avg_brightness),
                  # Add your custom assertions here
                  'assertions': {
                      'lights_on': avg_brightness > 128,  # Example threshold
                      # 'garage_door_open': door_open,
                      # 'car_present': car_present,
                  }
              }

              # Log results
              print(f"Analysis complete: {results}")

              # Optionally save results to file
              with open('/var/lib/hass/garage-snapshots/latest_analysis.txt', 'w') as f:
                  f.write(str(results))

              return results

          except Exception as e:
              print(f"Error analyzing image: {e}", file=sys.stderr)
              return None

      if __name__ == '__main__':
          if len(sys.argv) < 2:
              print("Usage: analyze_garage.py <image_path>")
              sys.exit(1)

          image_path = sys.argv[1]
          results = analyze_image(image_path)

          if results:
              sys.exit(0)
          else:
              sys.exit(1)
    '';
    mode = "0755";
  };

  # Add garageImageAnalyzer to system packages
  environment.systemPackages = [ garageImageAnalyzer ];
}
