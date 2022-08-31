{ pkgs, lib, ... }:

let
  home = builtins.getEnv "HOME";
  common-programs = import ../common/home-manager.nix { pkgs = pkgs; lib = lib; }; in
{
  home = {
    enableNixpkgsReleaseCheck = false;
    packages = pkgs.callPackage ./packages.nix {};
    username = "dustin";
    homeDirectory = "/home/dustin";
    stateVersion = "21.05";
  };

  # TODO: Clean this up. Import these so we use Nix composability.
  programs = common-programs // {
    alacritty.settings.font = {
      normal = {
        family = "Hack";
        style = "Regular";
      };
      size = 10;
    };
  };
}
