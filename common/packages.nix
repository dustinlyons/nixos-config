{ pkgs }:

with pkgs; [
  act # run github actions locally
  alacritty
  aspell
  aspellDicts.en
  awscli2
  bash-completion
  bat # A cat(1) clone with syntax highlighting
  btop
  cloudflared
  coreutils
  difftastic
  dejavu_fonts
  docker
  docker-compose
  du-dust
  emacs-all-the-icons-fonts
  flyctl
  ffmpeg
  fd
  fzf
  font-awesome
  gcc
  gh # github
  git-filter-repo
  glow # CLI markdown viewer
  gnupg
  google-cloud-sdk
  go
  gopls
  hack-font
  home-manager
  htop
  hunspell
  iftop
  jetbrains-mono
  jq

  # This is broken on MacOS for now
  # https://github.com/NixOS/nixpkgs/issues/172165 
  # keepassxc

  killall
  libfido2
  neofetch
  nodePackages.live-server
  nodePackages.nodemon
  nodePackages.npm
  nodejs
  noto-fonts
  noto-fonts-emoji
  ngrok
  openssh
  pandoc
  pinentry
  python39
  python39Packages.virtualenv
  ripgrep
  slack
  sqlite
  ssm-session-manager-plugin
  terraform
  terraform-ls
  tflint
  tree
  tmux
  unrar
  unzip
  wget
  zip
  zsh-powerlevel10k
  meslo-lgs-nf # Meslo Nerd Font patch for powerlevel10
]
