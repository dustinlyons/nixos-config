{ pkgs, ... }:
let
  myPython = pkgs.python3.withPackages (ps: with ps; [
    slpp
    pip
    rich
    mysql-connector
    virtualenv
    black
    requests
    faker
    textual
    pyqt5
    pyyaml
    feedparser
    python-dateutil
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
  ghostty # GPU-accelerated terminal emulator
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
  flyctl # Fly.io tools
  fzf # Fuzzy finder

  # G
  go # Go
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
  intelephense # PHP LSP server

  # J
  jetbrains.phpstorm # PHP IDE
  jpegoptim # JPEG optimizer
  jq # JSON processor

  # K
  killall # Kill processes by name

  # L
  linear-cli # Linear project management CLI
  lnav # Log file navigator
  libfido2 # FIDO2 library

  # M
  myPHP # Custom PHP with extensions
  myPython # Custom Python with packages

  # N
  ncurses # Terminal control library with terminfo database
  ncdu # Disk space utility
  neofetch # System information tool
  ngrok # Secure tunneling service
  nodejs_20 # Node.js JavaScript runtime (includes npm)

  # O
  openssh # SSH client and server

  # P
  pandoc # Document converter
  php82Packages.composer # PHP dependency manager
  php82Packages.deployer # PHP deployment tool
  php82Packages.php-cs-fixer # PHP code style fixer
  php82Packages.phpstan # PHP static analysis tool
  phpactor # PHP language server with better refactoring support
  phpunit # PHP testing framework
  pngquant # PNG compression tool

  # Q
  qt5.qtbase # Qt5 base library with platform plugins

  # R
  ripgrep # Fast text search tool
  repomix # AI tooling

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
  zed-editor
  zip # ZIP archive creator
  zsh-powerlevel10k # Zsh theme
] ++ myFonts
