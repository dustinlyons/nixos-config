{ pkgs, ... }:

let
  home = builtins.getEnv "HOME";
  common-programs = import ../common/home-manager.nix { pkgs = pkgs; }; in
{
  home = {
    enableNixpkgsReleaseCheck = false;
    packages = pkgs.callPackage ./packages.nix {};
    username = "dustin";
    homeDirectory = "/home/dustin";
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
