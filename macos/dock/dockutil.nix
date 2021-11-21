with (import <nixpkgs> { });
derivation {
  name = "dockutil-2.0.5";
  builder = "${bash}/bin/bash";
  args = [
    "-xeuc"
    ''
      ${unzip}/bin/unzip $src
      ${coreutils}/bin/mkdir -p $out/bin
      ${coreutils}/bin/mv dockutil-2.0.5/scripts/dockutil $out/bin/dockutil
    ''
  ];
  src = fetchurl {
    url = "https://github.com/kcrawford/dockutil/archive/2.0.5.zip";
    sha256 = "0b18awdaimf3gc4dhxx6lpivvx4li7j8kci648ssz39fwmbknlam";
  };
  system = builtins.currentSystem;
}
