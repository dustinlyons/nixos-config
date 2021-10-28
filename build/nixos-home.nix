{ pkgs, lib, config, ... }:

let home = builtins.getEnv "HOME";
in {

  home.packages = pkgs.callPackage ./nixos-packages.nix {};
  home.username = "dustin";
  home.homeDirectory = "/home/dustin";

  programs = {
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
