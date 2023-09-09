{ config, pkgs, lib, ... }:

let
  shared-files = import ../shared/files.nix { inherit config pkgs; };
  user = "%USER%";
  # Define the content of your file as a derivation
  myEmacsLauncher = pkgs.writeScript "emacs-launcher.command" ''
    #!/bin/sh
      emacsclient -c -n &
  '';
  sharedFiles = import ../shared/files.nix { inherit config pkgs; };
  additionalFiles = import ./files.nix { inherit config pkgs; };
in
{
  imports = [
    <home-manager/nix-darwin>
   ./dock
  ];

  # It me
  users.users.${user} = {
    name = "${user}";
    home = "/Users/${user}";
    isHidden = false;
    shell = pkgs.zsh;
  };


  # We use Homebrew to install impure software only (Mac Apps)
  homebrew.enable = true;
  homebrew.onActivation = {
    autoUpdate = true;
    cleanup = "zap";
    upgrade = true;
  };
  homebrew.brewPrefix = "/opt/homebrew/bin";

  # These app IDs are from using the mas CLI app
  # mas = mac app store
  # https://github.com/mas-cli/mas
  #
  # $ nix shell nixpkgs#mas
  # $ mas search <app name>
  #
  homebrew.casks = pkgs.callPackage ./casks.nix {};
  homebrew.masApps = {
    "1password" = 1333542190;
    "canva" = 897446215;
    "drafts" = 1435957248;
    "hidden-bar" = 1452453066;
    "wireguard" = 1451685025;
    "whatsapp-desktop" = 1147396723;
    "yoink" = 457622435;
  };

  # Enable home-manager
  home-manager = {
    useGlobalPkgs = true;
    users.${user} = { pkgs, config, lib, ... }:{
      home.enableNixpkgsReleaseCheck = false;
      home.packages = pkgs.callPackage ./packages.nix {};
      home.file = lib.mkMerge [
        sharedFiles
        additionalFiles
        { "emacs-launcher.command".source = myEmacsLauncher; }
      ];
      home.activation.gpgImportKeys =
        let
          gpgKeys = [
            "/Users/${user}/.ssh/pgp_github.key"
            "/Users/${user}/.ssh/pgp_github.pub"
          ];
          gpgScript = pkgs.writeScript "gpg-import-keys" ''
            #! ${pkgs.runtimeShell} -el
            ${lib.optionalString (gpgKeys != []) ''
              ${pkgs.gnupg}/bin/gpg --import ${lib.concatStringsSep " " gpgKeys}
            ''}
          '';
          plistPath = "$HOME/Library/LaunchAgents/importkeys.plist";
        in
          # Prior to the write boundary: no side effects. After writeBoundary, side effects.
          # We're creating a new plist file, so we need to run this after the writeBoundary
          lib.hm.dag.entryAfter [ "writeBoundary" ] ''
            mkdir -p "$HOME/Library/LaunchAgents"
            cat >${plistPath} <<EOF
            <?xml version="1.0" encoding="UTF-8"?>
            <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
            <plist version="1.0">
            <dict>
              <key>Label</key>
              <string>gpg-import-keys</string>
              <key>ProgramArguments</key>
              <array>
                <string>${gpgScript}</string>
              </array>
              <key>RunAtLoad</key>
              <true/>
            </dict>
            </plist>
            EOF

            /bin/launchctl unload ${plistPath} || true
            /bin/launchctl load ${plistPath}
          '';

      home.stateVersion = "21.11";
      programs = {} // import ../shared/home-manager.nix { inherit config pkgs lib; };

      # Marked broken Oct 20, 2022 check later to remove this
      # https://github.com/nix-community/home-manager/issues/3344
      manual.manpages.enable = false;
    };
  };

  # Fully declarative dock using the latest from Nix Store
  local.dock.enable = true;
  local.dock.entries = [
    { path = "/Applications/Slack.app/"; }
    { path = "/System/Applications/Messages.app/"; }
    { path = "/System/Applications/Facetime.app/"; }
    { path = "/Applications/WhatsApp.app/"; }
    { path = "/Applications/Telegram.app/"; }
    { path = "${pkgs.alacritty}/Applications/Alacritty.app/"; }
    { path = "/System/Applications/Music.app/"; }
    { path = "/System/Applications/News.app/"; }
    { path = "/System/Applications/Photos.app/"; }
    { path = "/System/Applications/Photo Booth.app/"; }
    { path = "/System/Applications/TV.app/"; }
    { path = "/Applications/Asana.app/"; }
    { path = "/Applications/Drafts.app/"; }
    { path = "/System/Applications/Home.app/"; }
    {
      path = toString myEmacsLauncher;
      section = "others";
    }
    {
      path = "${config.users.users.${user}.home}/.local/share/";
      section = "others";
      options = "--sort name --view grid --display folder";
    }
    {
      path = "${config.users.users.${user}.home}/.local/share/downloads";
      section = "others";
      options = "--sort name --view grid --display stack";
    }
  ];

}
