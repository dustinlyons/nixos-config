final: prev: {
  newrelic-cli = prev.stdenv.mkDerivation rec {
    pname = "newrelic-cli";
    version = "0.111.5";

    src = prev.fetchurl {
      url = "https://github.com/newrelic/newrelic-cli/releases/download/v${version}/newrelic-cli_${version}_Linux_x86_64.tar.gz";
      sha256 = "1wkr8jcsdmkhaaz1c3ii7nzw7713mmwgv0dhvbi2b0q8zr00ccz7";
    };

    sourceRoot = ".";

    dontBuild = true;

    installPhase = ''
      install -Dm755 newrelic $out/bin/newrelic
    '';

    meta = with prev.lib; {
      description = "The New Relic CLI";
      homepage = "https://github.com/newrelic/newrelic-cli";
      license = licenses.asl20;
      platforms = [ "x86_64-linux" ];
    };
  };
}
