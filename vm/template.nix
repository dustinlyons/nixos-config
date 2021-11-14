{ nixpkgs ? <nixpkgs>, system ? "x86_64-linux" }:

let
  iso-config = { pkgs, ... }: {
    imports = [
      "${nixpkgs}/nixos/modules/installer/cd-dvd/iso-image.nix"
      "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
    ];

    networking.hostName = "base"; 
    users.extraUsers.root.password = "password";
    environment.systemPackages = with pkgs; [ vim git screen ];

  };

  evalNixos = configuration: import "${nixpkgs}/nixos" {
    inherit system configuration;
  };

in { iso = (evalNixos iso-config).config.system.build.isoImage; }
