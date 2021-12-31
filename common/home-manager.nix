{ pkgs, ... }:

{

  zsh.enable = true;
  zsh.autocd = false;
  zsh.cdpath = [ "~/State/Projects/Code/" ];
  zsh.oh-my-zsh = {
    enable = true;
    theme = "robbyrussell";
  };

  zsh.dirHashes = {
    Code = "$HOME/State/Projects/Code";
    Config = "$HOME/State/Projects/Code/nixos-config";
    Downloads = "$HOME/State/Inbox/Downloads";
    Screenshots = "$HOME/State/Inbox/Screenshots";
    Wallpaper = "$HOME/State/Resources/Wallpaper";
  };

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

    # Open file window from within terminal
    alias open='nautilus --browser . > /dev/null 2>&1 &'

    # Ranger is a terminal app to browse files
    alias r='ranger'

    # Always color ls
    alias ls='ls --color'

    # Cypress is a dev tool for end-to-end testing
    export CYPRESS_INSTALL_BINARY=0
    export CYPRESS_RUN_BINARY=$(which Cypress)
  '';

  git = {
    enable = true;
    ignores = [ "*.swp" ];
    userName = "Dustin Lyons";
    userEmail = "hello@dustinlyons.co";
    attributes = [ "* filter=trimWhitespace" ];
    lfs = {
      enable = true;
    };
    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "vim";
      filter.trimWhitespace.clean = "git-stripspace";
    };
  };

  alacritty = {
    enable = true;
    settings = {
      background_opacity =  1.0;
      cursor = {
        style = "Block";
      };

      window = {
        padding = {
          x = 8;
          y = 8;
        };
      };

      font = {
        normal = {
          family = "Hack";
          style = "Regular";
        };

        size = 14;

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
