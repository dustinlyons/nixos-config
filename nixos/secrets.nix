{ config, pkgs, agenix, secrets, ... }:

let user = "dustin"; in
{

  age.identityPaths = [
    "/home/${user}/.ssh/id_ed25519"
  ];

  # age.secrets."syncthing-cert" = {
  #   symlink = true;
  #   path = "/home/${user}/.config/syncthing/cert.pem";
  #   file =  "${secrets}/nixos/syncthing-cert.age";
  #   mode = "644";
  #   owner = "${user}";
  #   group = "staff";
  # };

  # age.secrets."syncthing-key" = {
  #   symlink = true;
  #   path = "/home/{$user}/.config/syncthing/key.pem";
  #   file =  "${secrets}/nixos/syncthing-key.age";
  #   mode = "600";
  #   owner = "${user}";
  #   group = "staff";
  # };

  age.secrets."github-ssh-key" = {
    symlink = true;
    path = "/home/${user}/.ssh/id_github";
    file =  "${secrets}/github-ssh-key.age";
    mode = "600";
    owner = "${user}";
  };

  age.secrets."github-signing-key" = {
    symlink = true;
    path = "/home/${user}/.ssh/pgp_github.key";
    file =  "${secrets}/github-signing-key.age";
    mode = "600";
    owner = "${user}";
  };

}
