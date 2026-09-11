# Linear CLI overlay - wrapper using npx
final: prev: {
  linear-cli = prev.writeShellScriptBin "linear" ''
    # Linear CLI wrapper using npx
    # Uses evangodon's linear-cli package
    
    export PATH="${prev.nodejs_24}/bin:$PATH"

    # Run using npx with cache. Version pinned: an unpinned `npx --yes` would
    # execute whatever is on the registry at that moment, as this user.
    exec ${prev.nodejs_24}/bin/npx --yes @egcli/lr@0.18.0 "$@"
  '';
}