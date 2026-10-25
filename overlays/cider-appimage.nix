self: super: with super; {
  cider-appimage = appimageTools.wrapType2 rec {
    pname = "cider";
    version = "3.1.2";

    # Paid app with no public download; served from garfield's LAN file host
    # (modules/nixos/appimage-host.nix).
    src = fetchurl {
      url = "http://10.0.10.134:8088/cider-v3.1.2-linux-x64.AppImage";
      hash = "sha256-1syFQAvx4OdM2y03nP31r+YapH69ijd8XhEp4WxNxOo=";
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

        # The .desktop file says Icon=cider; without this KDE shows a generic icon
        install -m 444 -D ${contents}/Cider.png $out/share/icons/hicolor/256x256/apps/${pname}.png
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
