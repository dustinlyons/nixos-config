{ pkgs }:

with pkgs; [
  # General packages for development and system management
  alacritty
  bash-completion
  bat
  btop
  coreutils
  killall
  openssh
  sqlite
  wget
  zip

  # Encryption and security tools
  age
  gnupg

  # Cloud-related tools and SDKs
  docker
  docker-compose

  # Media-related packages
  emacs-all-the-icons-fonts
  dejavu_fonts
  fd
  font-awesome
  hack-font
  noto-fonts
  noto-fonts-color-emoji
  meslo-lgs-nf

  # Node.js development tools
  nodejs_24

  # Text and terminal utilities
  htop
  jetbrains-mono
  jq
  ripgrep
  tree
  tmux
  unzip
  zsh-powerlevel10k
  
  # Development tools
  curl
  gh
  terraform
  kubectl
  awscli2
  lazygit
  fzf
  direnv
  
  # Programming languages and runtimes
  go
  rustc
  cargo
  openjdk

  # Python packages
  python3
  virtualenv
]
