self: super: with super; {
  cider-appimage = appimageTools.wrapType2 rec {
    pname = "cider";
    version = "3.0.2";

    src = builtins.path {
      path = ./cider-v3.0.2-linux-x64.AppImage;
      name = "cider-v${version}-linux-x64.AppImage";
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