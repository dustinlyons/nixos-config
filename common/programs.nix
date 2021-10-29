{ pkgs }:

{
 # alacritty = {
 #   enable = true;
 # };

  git = {
    enable = true;
    userName = "Dustin Lyons";
    userEmail = "hello@dustinlyons.co";
    extraConfig = {
      init.defaultBranch = "main";
    };
  };
}
