final: prev: {
  sentry-cli = prev.stdenv.mkDerivation rec {
    pname = "sentry-cli";
    version = "3.3.5";

    src = prev.fetchurl {
      url = "https://github.com/getsentry/sentry-cli/releases/download/${version}/sentry-cli-Linux-x86_64";
      sha256 = "1nzcr8586wq318c74qjv70g4l7ifka7kcvfwkpf7r5acfvda7gc1";
    };

    dontUnpack = true;
    dontBuild = true;

    nativeBuildInputs = [ prev.autoPatchelfHook ];
    buildInputs = [ prev.stdenv.cc.cc.lib ];

    installPhase = ''
      install -Dm755 $src $out/bin/sentry-cli
    '';

    meta = with prev.lib; {
      description = "A command line utility to work with Sentry";
      homepage = "https://github.com/getsentry/sentry-cli";
      license = licenses.bsd3;
      platforms = [ "x86_64-linux" ];
    };
  };
}
