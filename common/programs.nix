{ pkgs, ... }:

{
  zsh = { 
   enable = true;
   autocd = true;
   cdpath = [ "~/State/Projects/Code/" ];
   oh-my-zsh = {
     enable = true;
     theme = "robbyrussell";
     plugins = [ "macos" "git" ];
   };
   initExtraFirst = ''
    export PATH=$NIX_USER_PROFILE_DIR/profile/bin/:$PATH
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
    };
  };
}
