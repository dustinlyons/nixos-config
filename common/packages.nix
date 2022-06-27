{ pkgs }:

with pkgs; [
  alacritty
  aspell
  aspellDicts.en
  awscli2
  bash-completion
  bat # A cat(1) clone with syntax highlighting
  bats # A bash testing framework
  bottom
  coreutils
  du-dust
  docker
  docker-compose
  fira-code
  flyctl
  font-awesome
  gcc
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
]
