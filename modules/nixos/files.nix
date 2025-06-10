{ user, ... }:

let
  home           = builtins.getEnv "HOME";
  xdg_configHome = "${home}/.config";
  xdg_dataHome   = "${home}/.local/share";
  xdg_stateHome  = "${home}/.local/state"; in
{

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
}
