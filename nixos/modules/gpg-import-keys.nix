{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.gpg.autoImport;

  isPath = x: builtins.substring 0 1 (toString x) == "/";

  gpgKeys = filter (k: isPath k) cfg.keys;

in {
  options.programs.gpg = {
    autoImport = {
      keys = mkOption {
        description = "List of gpg keys to automatically import";
        type = types.listOf (types.either types.str types.path);
        default = [];
      };
    };
  };

  config = {
  };
}
