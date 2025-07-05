{ user, ... }:

let
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
  xdg_dataHome   = "${home}/.local/share";
  xdg_stateHome  = "${home}/.local/state"; in
{
  "${home}/.npmrc" = {
    text = ''
      prefix=/home/dustin/.npm-packages
    '';
  };

  "${xdg_dataHome}/bin/movesinks" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      pacmd set-default-sink $1
      pacmd list-sink-inputs | grep index | while read line
      do
        pacmd move-sink-input `echo $line | cut -f2 -d' '` $1
      done
    '';
  };

  "${xdg_dataHome}/bin/speakers" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Script to change audio format to headphones and check if the sink exists

      # Define the sink name
      SINK_NAME="alsa_output.usb-Audioengine_Audioengine_2_-00.analog-stereo"

      # Check if the sink exists
      if pactl list short sinks | grep -q "$SINK_NAME"; then
        # Sink exists, set it as the default
        pacmd set-default-sink "$SINK_NAME"
        movesinks "$SINK_NAME"
      else
        # Sink does not exist, print message
        echo "Turn on your speakers, stupid."
      fi
    '';
  };

  "${xdg_dataHome}/bin/headphones" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Changes audio format to headphones
      pacmd set-default-sink alsa_output.pci-0000_00_1f.3.analog-stereo
      movesinks alsa_output.pci-0000_00_1f.3.analog-stereo
    '';
  };

  "${xdg_configHome}/swappy/config" = {
    text = ''
      [Default]
      save_dir=$HOME/Pictures/Screenshots
      save_filename_format=screenshot-%Y%m%d-%H%M%S.png
    '';
  };

  "${xdg_configHome}/waybar/modules/workspaces.sh" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      case "$1" in 
      focus-workspace)
          niri msg action "$@" && pkill -SIGRTMIN+8 waybar;;
      up)
          niri msg action focus-workspace-up && pkill -SIGRTMIN+8 waybar;;
      down)
          niri msg action focus-workspace-down && pkill -SIGRTMIN+8 waybar;;
      *)
          workspace_str=""
          active_idx=""
          
          # Get all workspaces for this output
          workspaces=$(niri msg -j workspaces | jq ".[] | select(.output == \"$1\")")
          
          # Build the workspace string with dot indicators
          for ws in $(echo "$workspaces" | jq -c "."); do
              idx=$(echo "$ws" | jq -r ".idx")
              is_active=$(echo "$ws" | jq -r ".is_active")
              
              if [ "$is_active" = "true" ]; then
                  # Active workspace - filled blue dot
                  workspace_str="$workspace_str <span size='x-large' color='#6699cc'>●</span> "
                  active_idx=$idx
              else
                  # Inactive workspace - hollow dot
                  workspace_str="$workspace_str <span size='large' alpha='60%'>○</span> "
              fi
          done
          
          echo -e "{\"text\":\"''${workspace_str}\", \"tooltip\":\"Workspace $active_idx\"}"
      esac
    '';
  };
}
