{ config, pkgs, agenix, secrets, ... }:

let user = "%USER%"; in
{
  age.identityPaths = [
    "/home/${user}/.ssh/id_ed25519"
  ];

  # Your secrets go here
}
