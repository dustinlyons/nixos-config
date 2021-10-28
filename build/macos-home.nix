{ config, pkgs, ... }:

{
  imports = [ <home-manager/nix-darwin> ];

  users.users.dustin = {
    name = "dustin";
    home = "/Users/dustin";
  };

  home-manager.users.dustin = { pkgs, ... }: {
    home.packages = pkgs.callPackage ./macos-packages.nix {};
  };

}
