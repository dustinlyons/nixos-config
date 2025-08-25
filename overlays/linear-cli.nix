# Linear CLI overlay - wrapper using npx
final: prev: {
  linear-cli = prev.writeShellScriptBin "linear" ''
    # Linear CLI wrapper using npx
    # Uses evangodon's linear-cli package
    
    export PATH="${prev.nodejs_20}/bin:$PATH"
    
    # Run using npx with cache
    exec ${prev.nodejs_20}/bin/npx --yes @egcli/lr "$@"
  '';
}