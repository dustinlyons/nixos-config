{ pkgs, lib, ... }:

{

  go.enable = true;
  zsh.enable = true;
  zsh.autocd = false;
  zsh.cdpath = [ "~/State/Projects/Code/" ];

  zsh.dirHashes = {
    code = "$HOME/State/Projects/Code";
    nixos-config = "$HOME/State/Projects/Code/nixos-config";
  };

  zsh.plugins = [
    {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
    }
    {
        name = "powerlevel10k-config";
        src = lib.cleanSource ./config;
        file = "p10k.zsh";
    }
  ];

  zsh.initExtraFirst = ''
    if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
      . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
      . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
    fi
    export PATH=$HOME/.npm-packages/bin:$PATH
    export PATH=$NIX_USER_PROFILE_DIR/profile/bin:$PATH
    export PATH=$HOME/bin:$PATH
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

    # Cypress is a dev toom for end-to-end testing
    export CYPRESS_INSTALL_BINARY=0
    export CYPRESS_RUN_BINARY=$(which Cypress)

    # Remove history data we don't want to see
    export HISTIGNORE="pwd:ls:cd"

    # Emacs is my editor
    export ALTERNATE_EDITOR=""
    export EDITOR="emacsclient -t"
    export VISUAL="emacsclient -c -a emacs"
    alias e='emacsclient -t'

    # Enter nix-shell
    alias s="nix-shell '<nixpkgs>' -A $1"

    # Local global npm packages
    alias yarn=$HOME/.npm-new-global/bin/yarn

    # bat all the things
    alias cat=bat
    alias more=bat

    # Use difftastic, syntax-aware diffing
    alias diff=difft

    # Always color ls
    alias ls='ls --color'

    # Weather report in your terminal
    alias weather='curl http://wttr.in'

    # Reboot into Windows for Steam Big Picture
    alias windows='systemctl reboot --boot-loader-entry=auto-windows'
  '';

  git = {
    enable = true;
    ignores = [ "*.swp" ];
    userName = "Dustin Lyons";
    userEmail = "dustin@dlyons.dev";
    lfs = {
      enable = true;
    };
    extraConfig = {
      credential.helper = "netlify";
      init.defaultBranch = "main";
      core = { 
	    editor = "vim";
        autocrlf = "input";
      };
      commit.gpgsign = true;
      pull.rebase = true;
      rebase.autoStash = true;
    };
  };

  alacritty = {
    enable = true;
    settings = {
      cursor = {
        style = "Block";
      };

      window = {
        opacity = 1.0;
        padding = {
          x = 24;
          y = 24;
        };
      };

      font = {
        normal = {
          family = "MesloLGS NF";
          style = "Regular";
        };
        size = lib.mkMerge [
          (lib.mkIf pkgs.stdenv.hostPlatform.isLinux 10)
          (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin 14)
        ];
      };

      dynamic_padding = true;
      decorations = "full";
      title = "Terminal";
      class = {
        instance = "Alacritty";
        general = "Alacritty";
      };

      colors = {
        primary = {
          background = "0x1f2528";
          foreground = "0xc0c5ce";
        };

        normal = {
          black = "0x1f2528";
          red = "0xec5f67";
          green = "0x99c794";
          yellow = "0xfac863";
          blue = "0x6699cc";
          magenta = "0xc594c5";
          cyan = "0x5fb3b3";
          white = "0xc0c5ce";
        };

        bright = {
          black = "0x65737e";
          red = "0xec5f67";
          green = "0x99c794";
          yellow = "0xfac863";
          blue = "0x6699cc";
          magenta = "0xc594c5";
          cyan = "0x5fb3b3";
          white = "0xd8dee9";
        };
      };
    };
  };
}
