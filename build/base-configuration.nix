{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # Link to this machine's nix config
      ./nixos-config.nix
    ];
}

