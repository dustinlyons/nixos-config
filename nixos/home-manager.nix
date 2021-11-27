{ pkgs, ... }:

let
  home = builtins.getEnv "HOME";
  common-programs = import ../common/home-manager.nix { pkgs = pkgs; }; in
{
  home = {
    packages = pkgs.callPackage ./packages.nix {};
    username = "dustin";
    homeDirectory = "/home/dustin";
  };

  programs = common-programs // {
  };
}
