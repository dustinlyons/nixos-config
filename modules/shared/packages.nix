{ pkgs, ... }:
let
  myPython = pkgs.python3.withPackages (ps: with ps; [
    slpp
    pip
    rich
    virtualenv
    black
  ]);

  myPHP = pkgs.php82.withExtensions ({ enabled, all }: enabled ++ (with all; [
    xdebug
  ]));

  myFonts = import ./fonts.nix { inherit pkgs; };
in
with pkgs; [
  # A
  act # Run Github actions locally
  age # File encryption tool
  age-plugin-yubikey # YubiKey plugin for age encryption
  alacritty # GPU-accelerated terminal emulator
  aspell # Spell checker
  aspellDicts.en # English dictionary for aspell

  # B
  bash-completion # Bash completion scripts
  bat # Cat clone with syntax highlighting
  btop # System monitor and process viewer

  # C
  coreutils # Basic file/text/shell utilities

  # D
  direnv # Environment variable management per directory
  difftastic # Structural diff tool
  du-dust # Disk usage analyzer

  # F
  fd # Fast find alternative
  ffmpeg # Multimedia framework
  fzf # Fuzzy finder

  # G
  gcc # GNU Compiler Collection
  gh # GitHub CLI
  glow # Markdown renderer for terminal
  gnupg # GNU Privacy Guard
  gopls # Go language server

  # H
  htop # Interactive process viewer
  hunspell # Spell checker

  # I
  iftop # Network bandwidth monitor
  imagemagick # Image manipulation toolkit

  # J
  jetbrains.phpstorm # PHP IDE
  jpegoptim # JPEG optimizer
  jq # JSON processor

  # K
  killall # Kill processes by name

  # L
  libfido2 # FIDO2 library

  # M
  myPHP # Custom PHP with extensions
  myPython # Custom Python with packages

  # N
  neofetch # System information tool
  ngrok # Secure tunneling service
  nodePackages_latest.live-server # Development server with live reload
  nodePackages_latest.nodemon # Node.js file watcher
  nodePackages_latest.npm # Node package manager
  nodePackages_latest.prettier # Code formatter

  # O
  openssh # SSH client and server

  # P
  pandoc # Document converter
  php82Packages.composer # PHP dependency manager
  php82Packages.deployer # PHP deployment tool
  php82Packages.php-cs-fixer # PHP code style fixer
  phpunit # PHP testing framework
  pngquant # PNG compression tool

  # R
  ripgrep # Fast text search tool

  # S
  slack # Team communication app
  sqlite # SQL database engine

  # T
  terraform # Infrastructure as code tool
  terraform-ls # Terraform language server
  tflint # Terraform linter
  tmux # Terminal multiplexer
  tree # Directory tree viewer

  # U
  unrar # RAR archive extractor
  unzip # ZIP archive extractor
  uv # Python package installer

  # W
  wget # File downloader

  # Z
  zip # ZIP archive creator
  zsh-powerlevel10k # Zsh theme
] ++ myFonts
