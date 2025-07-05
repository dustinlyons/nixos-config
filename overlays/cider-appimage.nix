self: super: with super; {
  cider-appimage = appimageTools.wrapType2 rec {
    pname = "cider";
    version = "3.0.2";

    src = ./cider-v3.0.2-linux-x64.AppImage;

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
           --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations --enable-wayland-ime=true}}" \
           --add-flags "--no-sandbox --disable-gpu-sandbox"

        install -m 444 -D ${contents}/Cider.desktop $out/share/applications/${pname}.desktop
        substituteInPlace $out/share/applications/${pname}.desktop \
          --replace-warn 'Exec=Cider' 'Exec=${pname}'
        install -Dm444 ${contents}/usr/share/icons/hicolor/256x256/cider-linux----arch-------version---.png \
                       $out/share/icons/hicolor/256x256/apps/cider.png
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