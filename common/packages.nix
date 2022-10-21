{ pkgs }:

with pkgs; [
  alacritty
  aspell
  aspellDicts.en
  awscli2
  bash-completion
  bat # A cat(1) clone with syntax highlighting
  bats # A bash testing framework
  btop
  coreutils
  difftastic
  du-dust
  docker
  docker-compose
  fira-code
  flyctl
  fd
  fzf
  font-awesome
  gcc
  gh # github
  git-filter-repo
  glow # CLI markdown viewer
  gnupg
  google-cloud-sdk
  gopls
  hack-font
  home-manager
  htop
  hunspell
  iftop
  jq

  # This is broken on MacOS for now
  # https://github.com/NixOS/nixpkgs/issues/172165 
  # keepassxc

  killall
  libfido2
  nodePackages.live-server
  nodePackages.npm
  nodejs
  ngrok
  openssh
  pandoc
  pinentry
  python3
  virtualenv
  ripgrep
  roboto
  slack
  sqlite
  ssm-session-manager-plugin
  terraform
  terraform-ls
  tree
  tmux
  unrar
  unzip
  vim
  wget
  zip
  zsh-powerlevel10k
  meslo-lgs-nf # Meslo Nerd Font patch for powerlevel10
]
