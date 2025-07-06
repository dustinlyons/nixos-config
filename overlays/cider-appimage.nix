self: super: with super; {
  cider-appimage = appimageTools.wrapType2 rec {
    pname = "cider";
    version = "3.0.2";

    src = fetchurl {
      url = "http://10.0.10.2:8080/cider-v${version}-linux-x64.AppImage";
      sha256 = "sha256-XVBhMgSNJAYTRpx5GGroteeOx0APIzuHCbf+kINT2eU=";
    };

    nativeBuildInputs = [ makeWrapper ];

    extraInstallCommands =
      let
        contents = appimageTools.extract {
          inherit version src;
          pname = "Cider";
        };
      in
      ''
        wrapProgram $out/bin/${pname} \
           --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WaylandWindowDecorations" \
           --add-flags "--no-sandbox --disable-gpu-sandbox"

        if [ -f ${contents}/Cider.desktop ]; then
          install -m 444 -D ${contents}/Cider.desktop $out/share/applications/${pname}.desktop
          substituteInPlace $out/share/applications/${pname}.desktop \
            --replace-warn 'Exec=Cider' 'Exec=${pname}'
        fi
      '';

    meta = with lib; {
      description = "A new cross-platform Apple Music experience built on Electron and Vue.js";
      homepage = "https://cider.sh";
      license = licenses.unfree;
      mainProgram = "cider";
      platforms = platforms.linux;
    };
  };
}