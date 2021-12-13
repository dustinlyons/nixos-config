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

  # TODO: Clean this up. Import these so we use Nix composability.
  programs = common-programs // {
    git = {
      enable = true;
      ignores = [ "*.swp" ];
      userName = "Dustin Lyons";
      userEmail = "hello@dustinlyons.co";
      lfs = {
        enable = true;
      };
      extraConfig = {
        init.defaultBranch = "main";
        core.editor = "vim";
        credential.helper = "netlify";
      };
    };
  };
}
