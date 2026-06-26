{ secrets, ... }:

# agenix-managed secrets for garfield.
#
# These are the only pieces of garfield's runtime state that are NOT
# reproducible by `nixos-rebuild` and would otherwise have to be hand-restored
# after a disaster. Each is encrypted (in the nix-secrets repo) to both the
# garfield host key and dustin@dlyons.dev, so the machine can decrypt them
# unattended at boot and they remain recoverable off-box if garfield dies.
#
# Decryption identity: the host key. System services (n8n, github-runner) need
# their secrets during early boot, before any user logs in.
{
  age.identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

  age.secrets = {
    # n8n credential-encryption key. Decrypted to /run/agenix/n8n-encryption-key
    # (root-only); n8n.nix installs it to the LoadCredential path at boot.
    n8n-encryption-key.file = "${secrets}/n8n-encryption-key.age";

    # GitHub Actions runner registration. agenix places these directly at the
    # paths the runner module reads, owned root:lab-ci so the CI user can read.
    github-runner-token = {
      file = "${secrets}/github-runner-token.age";
      path = "/etc/github-runner/token";
      owner = "root";
      group = "lab-ci";
      mode = "640";
    };

    github-runner-config = {
      file = "${secrets}/github-runner-config.age";
      path = "/etc/github-runner/config";
      owner = "root";
      group = "lab-ci";
      mode = "640";
    };
  };
}
