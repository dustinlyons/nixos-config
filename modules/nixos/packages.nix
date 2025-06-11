{ pkgs, inputs }:
with pkgs;
let shared-packages = import ../shared/packages.nix { inherit pkgs; }; in
shared-packages ++ [

  _1password-gui # Password manager

  brlaser # Printer driver

  chromedriver # Chrome webdriver for testing

  claude-code # Coding agent
  inputs.claude-desktop.packages."${pkgs.system}".claude-desktop-with-fhs

  discord # Voice and text chat

  gimp # Image editor
  google-chrome # Web browser

  keepassxc # Password manager

  pavucontrol # Pulse audio controls
  playerctl # Control media players from command line

  qmk # Keyboard firmware toolkit

  screenkey # Display pressed keys on screen
  simplescreenrecorder # Screen recording tool

  unixtools.ifconfig # Network interface configuration
  unixtools.netstat # Network statistics

  vlc # Media player

  xclip # Clipboard utilities

  yubikey-agent # Yubikey SSH agent
  pinentry-qt # GPG pinentry

  zathura # PDF viewer
]
