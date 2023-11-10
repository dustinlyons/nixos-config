{ config, pkgs, agenix, secrets, ... }:

let user = "%USER%"; in
{
  age.identityPaths = [ 
    "/Users/${user}/.ssh/id_ed25519"
  ];

  # Your secrets go here
}
