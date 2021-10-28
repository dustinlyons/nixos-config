{ pkgs }:

{
  emacs = {
    extraConfig = ''
(package-initialize)
(require 'org-install)
(file-truename (org-babel-load-file "~/.config/emacs/config.org"))
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
