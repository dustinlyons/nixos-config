{ pkgs }:

let
  home = builtins.getEnv "HOME"; in
{

  # Raycast script so that "Run Emacs" is available and uses Emacs daemon
  ".local/share/bin/emacsclient".text = ''
    #!/bin/zsh
    #
    # Required parameters:
    # @raycast.schemaVersion 1
    # @raycast.title Run Emacs
    # @raycast.mode silent
    #
    # Optional parameters:
    # @raycast.packageName Emacs
    # @raycast.icon ${home}/.local/share/img/icons/Emacs.icns
    # @raycast.iconDark ${home}/.local/share/img/icons/Emacs.icns

    if [[ $1 = "-t" ]]; then
      # Terminal mode
      ${pkgs.emacs}/bin/emacsclient -t
    else
      # GUI mode
      ${pkgs.emacs}/bin/emacsclient -c
    fi
  '';

  # Script to import Drafts into Emacs org-roam
  ".local/share/bin/import-drafts".text = ''
    #!/bin/sh

    for f in ${home}/.local/state/drafts/*
    do
      if [[ ! "$f" =~ "done" ]]; then
        echo "Importing $f"
        filename="$(head -c 10 $f)"
        output="${home}/.local/share/org-roam/daily/$filename.org"
        echo '\n' >> "$output"
        tail -n +3 $f >> "$output"
        mv $f done
      fi
    done
  '';
}
