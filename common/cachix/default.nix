{ pkgs, lib, ... }:

let
  # Import local nix files in directory
  folder = ./.;
  toImport = name: value: folder + ("/" + name);

  # Filer for "regular", "directory", "symlink", or "unknown"
  filterCaches = key: value: value == "regular" && lib.hasSuffix ".nix" key && !(lib.hasPrefix "default" key);
  imports = lib.mapAttrsToList toImport (lib.filterAttrs filterCaches (builtins.readDir folder));
in {
  inherit imports;

  # Set list of caches
  nix.settings.substituters = ["https://cache.nixos.org/"];
}
