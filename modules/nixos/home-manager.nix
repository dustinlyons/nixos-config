{ config, pkgs, lib, inputs, ... }:

let
  user = "dustin";
  xdg_configHome  = "/home/${user}/.config";
  shared-programs = import ../shared/home-manager.nix { inherit config pkgs lib; };
  shared-files = import ../shared/files.nix { inherit config pkgs; };

  # These files are generated when secrets are decrypted at build time
  gpgKeys = [
    "/home/${user}/.ssh/pgp_github.key"
    "/home/${user}/.ssh/pgp_github.pub"
  ];
in
{
  home = {
    enableNixpkgsReleaseCheck = false;
    username = "${user}";
    homeDirectory = "/home/${user}";
    packages = pkgs.callPackage ./packages.nix { inherit inputs; };
    file = shared-files // import ./files.nix { inherit user pkgs; };
    stateVersion = "25.05";
  };

  programs = shared-programs // { 
    gpg.enable = true; 
    plasma = {
      enable = true; 
      panels = [
        {
          floating = true;
          location = "bottom";
          widgets = [
            {
              kickoff = {
                sortAlphabetically = true;
                icon = "plasma-symbolic";
              };
            }
            {
              iconTasks = {
                appearance = {
                  showTooltips = true;
                  highlightWindows = true;
                  indicateAudioStreams = true;
                  fill = true;
                };
                launchers = [
                  "preferred://browser"
                  "applications:systemsettings.desktop"
                  "preferred://filemanager"
                  "applications:Alacritty.desktop"
                ];
              };
            }
            "org.kde.plasma.marginsseparator"
            {
              systemTray.items = {
                shown = [
                  "org.kde.plasma.clipboard"
                  "org.kde.plasma.volume"
                  "org.kde.plasma.brightness"
                  "org.kde.plasma.networkmanagement"
                  "org.kde.plasma.brightness"
                ];
              };
            }
            "org.kde.plasma.digitalclock"
          ];
        }
      ];
    };
  };

  # This installs my GPG signing keys for Github
  systemd.user.services.gpg-import-keys = {
    Unit = {
      Description = "Import gpg keys";
      After = [ "gpg-agent.socket" ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = toString (pkgs.writeScript "gpg-import-keys" ''
        #! ${pkgs.runtimeShell} -el
        ${lib.optionalString (gpgKeys != []) ''
        ${pkgs.gnupg}/bin/gpg --import ${lib.concatStringsSep " " gpgKeys}
        ''}
      '');
    };

    Install = { WantedBy = [ "default.target" ]; };
  };
}
