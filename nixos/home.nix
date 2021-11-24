{ pkgs, ... }:

let 
  home = builtins.getEnv "HOME";
  common-programs = import ../common/home.nix { pkgs = pkgs; }; in
{
  home.packages = pkgs.callPackage ./packages.nix {};
  home.username = "dustin";
  home.homeDirectory = "/home/dustin";

  programs = common-programs // { };
}
