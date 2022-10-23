{ pkgs, lib, ... }:

let
  # Import local .nix files in directory
  folder = ./.;
  toImport = name: value: folder + ("/" + name);
  filterCaches = key: value: value == "regular" && lib.hasSuffix ".nix" key && !(lib.hasPrefix "default" key);
  imports = lib.mapAttrsToList toImport (lib.filterAttrs filterCaches (builtins.readDir folder));
in {
  inherit imports;

  # Set list of cache
  nix.settings.substituters = ["https://cache.nixos.org/"];
}
