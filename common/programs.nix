{ pkgs, ... }:

{
  zsh = { 
   enable = true;
   autocd = true;
   cdpath = [ "~/State/Projects/Code/" ];
   oh-my-zsh = {
     enable = true;
     theme = "robbyrussell";
     plugins = [ "docker" "emacs" ];
   };
   initExtraFirst = ''
    export PATH=$HOME/.npm-packages/bin:$PATH
    export PATH=$NIX_USER_PROFILE_DIR/profile/bin:$PATH
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm 
   '';
  };

  alacritty = {
   enable = true;
  };

  git = {
    enable = true;
    userName = "Dustin Lyons";
    userEmail = "hello@dustinlyons.co";
    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "vim";
    };
  };
}
