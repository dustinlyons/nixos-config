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
  du-dust
  docker
  docker-compose
  fira-code
  flyctl
  fd
  fzf
  font-awesome
  gcc
  gh
  git-filter-repo
  gnupg
  google-cloud-sdk
  gopls
  hack-font
  highlight
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
  openssh
  pandoc
  pinentry
  python3
  ranger
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
  vscode
  wget
  zip
  zsh-powerlevel10k
  meslo-lgs-nf
]
