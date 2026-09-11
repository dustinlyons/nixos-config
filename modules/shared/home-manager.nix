{ config, pkgs, lib, ... }:

let name = "Dustin Lyons";
    user = "dustin";
    email = "dustin@dlyons.dev"; in
{

  direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };

  # Fuzzy finder. The binary alone does nothing for daily use -- the value is
  # in the zsh integration, which binds Ctrl-T (paste a file path) and Alt-C
  # (cd into a subdirectory). Ctrl-R is claimed by atuin below.
  fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "fd --type f --hidden --exclude .git";
    defaultOptions = [ "--height 40%" "--layout=reverse" "--border" ];

    # fzf and atuin both bind Ctrl-R. atuin's init is sourced after fzf's, so
    # atuin's binding wins at runtime -- this setting doesn't unbind anything,
    # it just declares that fzf isn't claiming the key, which is what silences
    # home-manager's conflict warning. Ctrl-T and Alt-C are untouched.
    historyWidget.zsh.command = "";
  };

  # Syntax-highlighting `cat`. Note the module does NOT wire bat into `man` on
  # its own -- MANPAGER is exported in initContent below to do that.
  bat = {
    enable = true;
    config = {
      theme = "base16";
      style = "numbers,changes,header";
    };
  };

  # `z conductly` jumps to the directory from anywhere, ranked by how often
  # and how recently it has been visited.
  zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

  # Replaces `ls` with a listing that carries git status and icons. The zsh
  # integration defines ls/ll/la/lt/lla, which is why the hand-written
  # `alias ls` is gone from initContent below.
  eza = {
    enable = true;
    enableZshIntegration = true;
    icons = "auto";
    git = true;
    extraOptions = [ "--group-directories-first" ];
  };

  # Shell history in a searchable SQLite database, recording directory, exit
  # code and duration. Local-only until `atuin login` is run.
  #
  # Ordering: atuin's init lands at order 1000, after fzf's keybindings at
  # 910, so atuin wins Ctrl-R without an explicit rebind. Up-arrow is left
  # alone so it stays plain zsh history.
  atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = [ "--disable-up-arrow" ];
    settings = {
      style = "compact";
      inline_height = 20;

      # Carried over from the hand-written ~/.config/atuin/config.toml that
      # predates this module (an atuin install from Nov 2025, whose history.db
      # is still in ~/.local/share/atuin). Everything else in that file was the
      # stock commented-out template; these two were the only live settings.
      enter_accept = true;
      sync.records = true;
    };
  };

  zsh = {
    enable = true;
    autocd = false;
    cdpath = [ "~/.local/share/src" ];

    # Ghost-writes the rest of a command from history; right-arrow accepts.
    autosuggestion.enable = true;

    # Colors a command red until it resolves to something runnable.
    syntaxHighlighting.enable = true;
    plugins = [
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
    initContent = lib.mkMerge [
      (lib.mkBefore ''
      if [[ -f /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
        . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
        . /nix/var/nix/profiles/default/etc/profile.d/nix.sh
      fi

      # Save and restore last directory
      LAST_DIR_FILE="$HOME/.zsh_last_dir"
      
      # Save directory on every cd
      function chpwd() {
        echo "$PWD" > "$LAST_DIR_FILE"
      }
      
      # Restore last directory on startup
      if [[ -f "$LAST_DIR_FILE" ]] && [[ -r "$LAST_DIR_FILE" ]]; then
        last_dir="$(cat "$LAST_DIR_FILE")"
        if [[ -d "$last_dir" ]]; then
          cd "$last_dir"
        fi
      fi

      export TERM=xterm-256color

      # Define PATH variables
      export PATH=$HOME/.pnpm-packages/bin:$HOME/.pnpm-packages:$PATH
      export PATH=$HOME/.npm-packages/bin:$HOME/bin:$PATH
      export PATH=$HOME/.composer/vendor/bin:$PATH
      export PATH=$HOME/.local/share/bin:$PATH
      export PATH=$HOME/.local/share/src/conductly/bin:$PATH
      export PATH=$HOME/.local/share/src/conductly/utils:$PATH
      export PYTHONPATH="$HOME/.local-pip/packages:$PYTHONPATH"

      # Remove history data we don't want to see
      export HISTIGNORE="pwd:ls:cd"

      # Ripgrep alias
      alias search='rg -p --glob "!node_modules/*" --glob "!vendor/*" "$@"'

      # Render man pages through bat. programs.bat does not set this itself.
      export MANPAGER="sh -c 'col -bx | bat -l man -p'"
      export MANROFFOPT="-c"

      # Emacs is my editor
      export ALTERNATE_EDITOR=""
      export EDITOR="emacsclient -t"
      export VISUAL="emacsclient -c -a emacs"
      e() {
          emacsclient -t "$@"
      }
      
      # Laravel Artisan
      alias art='php artisan'

      # Use difftastic, syntax-aware diffing
      alias diff=difft

      # `ls` and friends are aliased to eza by programs.eza.enableZshIntegration
      
      # SSH wrapper functions with terminal color changes
      ssh-production() {
          # Change terminal background to dark red
          printf '\033]11;#3d1515\007'
          command ssh production "$@"
          # Reset terminal background
          printf '\033]11;#1f2528\007'
      }
      
      ssh-staging() {
          # Change terminal background to dark orange
          printf '\033]11;#3d2915\007'
          command ssh staging "$@"
          # Reset terminal background
          printf '\033]11;#1f2528\007'
      }
      
      ssh-droplet() {
          # Change terminal background to dark green
          printf '\033]11;#153d15\007'
          command ssh droplet "$@"
          # Reset terminal background
          printf '\033]11;#1f2528\007'
      }
      
      # Override ssh command to detect known hosts. Matches the Host aliases
      # from ~/.ssh/config_external only — never raw addresses, since this
      # file is public.
      ssh() {
          case "$1" in
              production)
                  # Change terminal background to dark red
                  printf '\033]11;#3d1515\007'
                  command ssh "$@"
                  # Reset terminal background
                  printf '\033]11;#1f2528\007'
                  ;;
              staging)
                  # Change terminal background to dark orange
                  printf '\033]11;#3d2915\007'
                  command ssh "$@"
                  # Reset terminal background
                  printf '\033]11;#1f2528\007'
                  ;;
              droplet)
                  # Change terminal background to dark green
                  printf '\033]11;#153d15\007'
                  command ssh "$@"
                  # Reset terminal background
                  printf '\033]11;#1f2528\007'
                  ;;
              *)
                  command ssh "$@"
                  ;;
          esac
      }
      
      # Tmux aliases for devenv sessions
      alias atlas='tmux -S /run/user/1000/tmux-atlas attach -t atlas'
      alias conductly='tmux -S /run/user/1000/tmux-conductly attach -t conductly'
      alias river='tmux -S /run/user/1000/tmux-river attach -t river'

      # macOS-style open command using Nautilus
      ${lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
        alias open="xdg-open"
        alias rxp="/home/dustin/.local/share/src/restxp/restxp"

        # Reboot to Windows partition (Linux only)
        alias windows='sudo systemctl reboot --boot-loader-entry=auto-windows'
      ''}
      
      # Screenshot function with path selection
      screenshot() {
          local project_path
          case "$1" in
              conductly|c)
                  project_path="/home/dustin/.local/share/src/conductly"
                  ;;
              bitcoin-noobs|b)
                  project_path="/home/dustin/.local/share/src/bitcoin-noobs"
                  ;;
              *)
                  echo "Usage: screenshot [conductly|c|bitcoin-noobs|b]"
                  echo "  conductly (c) - Save to conductly project"
                  echo "  bitcoin-noobs (b) - Save to bitcoin-noobs project"
                  return 1
                  ;;
          esac
          
          # Prompt user for filename
          echo -n "Enter screenshot filename (without .png extension): "
          read -r user_filename
          
          # Use user input or fallback to timestamp if empty
          if [[ -n "$user_filename" ]]; then
              local filename="$user_filename.png"
          else
              local filename="screenshot-$(date +'%Y%m%d-%H%M%S').png"
          fi
          
          spectacle -r -b -o "$project_path/$filename"
          echo "Screenshot saved to: $project_path/$filename"
      }
    '')

      # fzf-tab replaces zsh's tab-completion menu with a fuzzy-searchable one.
      # It has to be sourced after compinit (home-manager emits that at order
      # 570) and before zsh-autosuggestions (order 700), which wraps the
      # completion widget -- so it cannot go in the `plugins` list above.
      (lib.mkOrder 600 ''
        source ${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh
      '')

      # fzf's own zsh integration is sourced at order 910 and rebinds Tab to
      # its `fzf-completion` widget, silently overwriting what fzf-tab bound at
      # 600. fzf-tab still has to load early so zsh-autosuggestions wraps its
      # widget correctly, so reclaim the key here rather than moving the load.
      (lib.mkOrder 920 ''
        bindkey '^I' fzf-tab-complete
      '')
    ];
  };

  git = {
    enable = true;
    ignores = [ "*.swp" ];
    lfs = {
      enable = true;
    };
    settings = {
      user.name = name;
      user.email = email;
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

  vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [ vim-airline vim-airline-themes vim-tmux-navigator ];
    settings = { ignorecase = true; };
    extraConfig = ''
      "" General
      set number
      set history=1000
      set nocompatible
      set modelines=0
      set encoding=utf-8
      set scrolloff=3
      set showmode
      set showcmd
      set hidden
      set wildmenu
      set wildmode=list:longest
      set cursorline
      set ttyfast
      set nowrap
      set ruler
      set backspace=indent,eol,start
      set laststatus=2
      " Don't use clipboard=unnamedplus, use macOS pbcopy/pbpaste instead

      " Dir stuff
      set nobackup
      set nowritebackup
      set noswapfile
      set backupdir=~/.config/vim/backups
      set directory=~/.config/vim/swap

      " Relative line numbers for easy movement
      set relativenumber
      set rnu

      "" Whitespace rules
      set tabstop=8
      set shiftwidth=2
      set softtabstop=2
      set expandtab

      "" Searching
      set incsearch
      set gdefault

      "" Statusbar
      set nocompatible " Disable vi-compatibility
      set laststatus=2 " Always show the statusline
      let g:airline_theme='bubblegum'
      let g:airline_powerline_fonts = 1

      "" Local keys and such
      let mapleader=","
      let maplocalleader=" "

      "" Change cursor on mode
      :autocmd InsertEnter * set cul
      :autocmd InsertLeave * set nocul

      "" File-type highlighting and configuration
      syntax on
      filetype on
      filetype plugin on
      filetype indent on

      "" macOS clipboard integration
      vnoremap <Leader>. :w !pbcopy<CR><CR>
      nnoremap <Leader>, :r !pbpaste<CR>

      "" Move cursor by display lines when wrapping
      nnoremap j gj
      nnoremap k gk

      "" Map leader-q to quit out of window
      nnoremap <leader>q :q<cr>

      "" Move around split
      nnoremap <C-h> <C-w>h
      nnoremap <C-j> <C-w>j
      nnoremap <C-k> <C-w>k
      nnoremap <C-l> <C-w>l

      "" Easier to yank entire line
      nnoremap Y y$

      "" Move buffers
      nnoremap <tab> :bnext<cr>
      nnoremap <S-tab> :bprev<cr>

      "" Like a boss, sudo AFTER opening the file to write
      cmap w!! w !sudo tee % >/dev/null

      let g:startify_lists = [
        \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
        \ { 'type': 'sessions',  'header': ['   Sessions']       },
        \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      }
        \ ]

      let g:startify_bookmarks = [
        \ '~/.local/share/src',
        \ ]

      let g:airline_theme='bubblegum'
      let g:airline_powerline_fonts = 1
      '';
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

      # Fix for shell path when launching from desktop
      # When launching from desktop, $SHELL may point to /bin/zsh instead of
      # the Nix-managed shell, causing environment issues
      terminal.shell = {
        program = "${pkgs.zsh}/bin/zsh";
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

  ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [
      (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
        "/home/${user}/.ssh/config_external"
      )
      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
        "/Users/${user}/.ssh/config_external"
      )
    ];
    settings = {
      "*" = {
        # Set the default values we want to keep
        SendEnv = [ "LANG" "LC_*" ];
        HashKnownHosts = true;
      };
      #"github.com" = {
      #  IdentitiesOnly = true;
      #  IdentityFile = [
      #    (lib.mkIf pkgs.stdenv.hostPlatform.isLinux
      #      "/home/${user}/.ssh/id_github"
      #    )
      #    (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin
      #      "/Users/${user}/.ssh/id_github"
      #    )
      #  ];
      #};
    };
  };

  tmux = {
    enable = true;
    shell = "${pkgs.zsh}/bin/zsh";
    sensibleOnTop = false;
    plugins = with pkgs.tmuxPlugins; [
      vim-tmux-navigator
      sensible  # Re-enabled with workaround below
      yank
      prefix-highlight
      {
        plugin = power-theme;
        extraConfig = ''
           set -g @tmux_power_theme 'gold'
        '';
      }
      {
        plugin = resurrect; # Used by tmux-continuum

        # Use XDG data directory
        # https://github.com/tmux-plugins/tmux-resurrect/issues/348
        extraConfig = ''
          set -g @resurrect-dir '/Users/dustin/.cache/tmux/resurrect'
          set -g @resurrect-capture-pane-contents 'on'
          set -g @resurrect-pane-contents-area 'visible'
        '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '5' # minutes
        '';
      }
    ];
    terminal = "screen-256color";
    prefix = "C-x";
    escapeTime = 10;
    historyLimit = 50000;
    extraConfig = ''
      # Remove Vim mode delays
      set -g focus-events on

      # Enable full mouse support
      set -g mouse on

      # -----------------------------------------------------------------------------
      # Key bindings
      # -----------------------------------------------------------------------------

      # Unbind default keys
      unbind C-b
      unbind '"'
      unbind %

      # Split panes, vertical or horizontal
      bind-key x split-window -v
      bind-key v split-window -h

      # Cycle windows with prefix + arrows (-r allows repeating without the prefix)
      bind-key -r Right next-window
      bind-key -r Left previous-window

      # Move around panes with vim-like bindings (h,j,k,l)
      bind-key -n M-k select-pane -U
      bind-key -n M-h select-pane -L
      bind-key -n M-j select-pane -D
      bind-key -n M-l select-pane -R

      # Smart pane switching with awareness of Vim splits.
      # This is copy paste from https://github.com/christoomey/vim-tmux-navigator
      is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
        | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?)(diff)?$'"
      bind-key -n 'C-h' if-shell "$is_vim" 'send-keys C-h'  'select-pane -L'
      bind-key -n 'C-j' if-shell "$is_vim" 'send-keys C-j'  'select-pane -D'
      bind-key -n 'C-k' if-shell "$is_vim" 'send-keys C-k'  'select-pane -U'
      bind-key -n 'C-l' if-shell "$is_vim" 'send-keys C-l'  'select-pane -R'
      tmux_version='$(tmux -V | sed -En "s/^tmux ([0-9]+(.[0-9]+)?).*/\1/p")'
      if-shell -b '[ "$(echo "$tmux_version < 3.0" | bc)" = 1 ]' \
        "bind-key -n 'C-\\' if-shell \"$is_vim\" 'send-keys C-\\'  'select-pane -l'"
      if-shell -b '[ "$(echo "$tmux_version >= 3.0" | bc)" = 1 ]' \
        "bind-key -n 'C-\\' if-shell \"$is_vim\" 'send-keys C-\\\\'  'select-pane -l'"

      bind-key -T copy-mode-vi 'C-h' select-pane -L
      bind-key -T copy-mode-vi 'C-j' select-pane -D
      bind-key -T copy-mode-vi 'C-k' select-pane -U
      bind-key -T copy-mode-vi 'C-l' select-pane -R
      bind-key -T copy-mode-vi 'C-\' select-pane -l
      
      # Darwin-specific fix for tmux 3.5a with sensible plugin
      # This MUST be at the very end of the config
      set -g default-command "$SHELL"
      '';
    };
}
