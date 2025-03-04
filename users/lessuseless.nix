{ config, pkgs, ... }:

{
  home-manager.users.lessuseless = { pkgs, ... }: {
    home.packages = with pkgs; [
      # Add packages specific to lessuseless
    ];

    # Additional user-specific settings go here
  };
}
