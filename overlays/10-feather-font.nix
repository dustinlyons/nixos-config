self: super: with super; {

  feather-font = let
    version = "1.0";
    pname = "feather-font";
  in stdenv.mkDerivation {
    name = "${pname}-${version}";

    src = fetchzip {
      url = "https://github.com/dustinlyons/feather-font/archive/refs/tags/${version}.zip";
      sha256 = "1rdq37bk81k7h7bsdg26djldg0wfgzqhwsqy2y5akp9wh1r76g7b";

      postFetch = ''
        mkdir -p $out/share/fonts/truetype
        unzip -D -j $src/feather.ttf -d $out/share/fonts/truetype/
      '';
    };

    installPhase = ''
      mkdir -p $out/share/fonts/truetype
      cp -p $src/share/fonts/truetype/feather.ttf $out/share/fonts/truetype/
    '';

    meta = with lib; {
      homepage = "https://www.feathericons.com/";
      description = "Set of font icons from the open source collection Feather Icons";
      license = licenses.mit;
      maintainers = [ maintainers.dlyons ];
      platforms = platforms.all;
    };
  };
}
