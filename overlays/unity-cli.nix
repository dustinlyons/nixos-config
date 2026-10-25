# Unity's standalone terminal CLI (`unity`): installs Editors and modules,
# creates/opens projects, authenticates, and prints JSON/TSV for scripts.
# Unity ships it as one Native-AOT C# binary from their CDN with a checksum
# manifest; upstream's install.sh would drop it in ~/.local/bin. Packaging it
# here keeps the machine declarative and lets agents rely on it being present.
#
# It is still labelled experimental by Unity. To bump: read the version and
# linux-x64 sha256 from
#   https://public-cdn.cloud.unity3d.com/hub/prod/cli/latest-beta.json
# and update `version` + `hash` below (`nix hash convert --hash-algo sha256 <hex>`
# turns the manifest's hex digest into the SRI form). `unity self-update`
# cannot work from the read-only Nix store, so bumping here is the update path.
#
# Two layers:
#
#   unity-cli-unwrapped — the binary with its ELF interpreter and libgcc_s
#     rewritten to the Nix store (autoPatchelfHook). `unity --version` works
#     from here, but not much else: the CLI does not dlopen by soname, it probes
#     distro paths (/usr/lib/x86_64-linux-gnu/libsqlite3.so.0, /usr/lib64/…)
#     and refuses to start when they are missing. LD_LIBRARY_PATH cannot help.
#
#   unity-cli — the unwrapped binary run inside the FHS sandbox nixpkgs builds
#     for Unity Hub (bubblewrap presenting a normal /usr, /lib), so those fixed
#     paths exist. Sharing the Hub's sandbox also means an Editor launched by
#     `unity open` inherits the exact environment the Hub would have given it,
#     which is the configuration known to work on NixOS. sqlite is added to
#     that sandbox via the `unityhub` override below, so the Hub and the CLI
#     see the same libraries.
self: super: with super; {
  # Unity Hub with the runtime libraries the standalone CLI also needs.
  unityhub = super.unityhub.override {
    extraLibs = pkgs: with pkgs; [ sqlite openssl zlib icu krb5 ];
  };

  unity-cli-unwrapped = stdenv.mkDerivation rec {
    pname = "unity-cli-unwrapped";
    version = "1.0.0-beta.10";

    src = fetchurl {
      url = "https://public-cdn.cloud.unity3d.com/hub/prod/cli/${version}/unity-linux-x64";
      hash = "sha256-EKUUZADAktoGeDJ+Fn7cf327NU8bP9dqVTWZDe4SXac=";
    };

    dontUnpack = true;
    dontStrip = true; # already stripped upstream

    nativeBuildInputs = [ autoPatchelfHook ];
    buildInputs = [ stdenv.cc.cc.lib ]; # libgcc_s.so.1

    installPhase = ''
      runHook preInstall
      install -Dm755 $src $out/bin/unity
      runHook postInstall
    '';

    meta = with lib; {
      description = "Unity's standalone CLI binary, patched for the Nix store (use unity-cli)";
      homepage = "https://docs.unity.com/en-us/unity-cli/use-unity-cli";
      license = licenses.unfree;
      sourceProvenance = with sourceTypes; [ binaryNativeCode ];
      platforms = [ "x86_64-linux" ];
      mainProgram = "unity";
    };
  };

  unity-cli = stdenv.mkDerivation {
    pname = "unity-cli";
    inherit (self.unity-cli-unwrapped) version meta;

    dontUnpack = true;
    nativeBuildInputs = [ makeWrapper ];

    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      makeWrapper ${self.unityhub.fhsEnv}/bin/unityhub-fhs-env $out/bin/unity \
        --add-flags ${self.unity-cli-unwrapped}/bin/unity \
        --argv0 unity
      runHook postInstall
    '';

    passthru = { unwrapped = self.unity-cli-unwrapped; };
  };
}
