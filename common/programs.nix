{ pkgs, ... }:

{
  alacritty = {
   enable = true;
  };

  git = {
    enable = true;
    userName = "Dustin Lyons";
    userEmail = "hello@dustinlyons.co";
    editor = "vim";
    extraConfig = {
      init.defaultBranch = "main";
    };
  };
}
