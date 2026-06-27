self: super: with super; {
  curseforge-appimage = appimageTools.wrapType2 rec {
    pname = "curseforge";
    version = "latest";

    src = ./curseforge-latest-linux.AppImage;

    nativeBuildInputs = [ makeWrapper ];

    extraInstallCommands =
      let
        contents = appimageTools.extract {
          inherit version src;
          pname = "CurseForge";
        };
      in
      ''
        wrapProgram $out/bin/${pname} \
           --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WaylandWindowDecorations" \
           --add-flags "--no-sandbox --disable-gpu-sandbox"

        if [ -f ${contents}/curseforge-app.desktop ]; then
          install -m 444 -D ${contents}/curseforge-app.desktop $out/share/applications/${pname}.desktop
          substituteInPlace $out/share/applications/${pname}.desktop \
            --replace-warn 'Exec=curseforge-app' 'Exec=${pname}'
        fi
      '';

    meta = with lib; {
      description = "CurseForge - Mod manager for Minecraft and other games";
      homepage = "https://www.curseforge.com";
      license = licenses.unfree;
      mainProgram = "curseforge";
      platforms = platforms.linux;
    };
  };
}
