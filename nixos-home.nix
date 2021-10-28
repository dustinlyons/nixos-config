{ pkgs, ... }:

let 
  home = builtins.getEnv "HOME";
  common-programs = import ./common/programs.nix { pkgs = pkgs; }; in
{
  home.packages = pkgs.callPackage ./nixos-packages.nix {};
  home.username = "dustin";
  home.homeDirectory = "/home/dustin";

  programs = common-programs // { 
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
  };
}

