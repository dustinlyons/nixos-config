{ config, pkgs, agenix, secrets, ... }:

let user = "%USER%"; in
{
  
  age.identityPaths = [ 
    "/Users/${user}/.ssh/id_ed25519"
  ];

  age.secrets."syncthing-cert" = {
    symlink = true;
    path = "/Users/${user}/Library/Application Support/Syncthing/cert.pem";
    file =  "${secrets}/darwin-syncthing-cert.age";
    mode = "644";
    owner = "${user}";
    group = "staff";
  };

  age.secrets."syncthing-key" = {
    symlink = true;
    path = "/Users/${user}/Library/Application Support/Syncthing/key.pem";
    file =  "${secrets}/darwin-syncthing-key.age";
    mode = "600";
    owner = "${user}";
    group = "staff";
  };

  age.secrets."github-ssh-key" = {
    symlink = true;
    path = "/Users/${user}/.ssh/id_github";
    file =  "${secrets}/github-ssh-key.age";
    mode = "600";
    owner = "${user}";
    group = "staff";
  };

  age.secrets."github-signing-key" = {
    symlink = false;
    path = "/Users/${user}/.ssh/pgp_github.key";
    file =  "${secrets}/github-signing-key.age";
    mode = "600";
    owner = "${user}";
  };

}
