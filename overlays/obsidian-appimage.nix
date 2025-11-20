self: super: with super; {
  obsidian-appimage = appimageTools.wrapType2 rec {
    pname = "obsidian";
    version = "1.9.14";

    src = ./obsidian-v1.9.14-linux-x64.AppImage;

    nativeBuildInputs = [ makeWrapper ];

    extraInstallCommands =
      let
        contents = appimageTools.extract {
          inherit version src;
          pname = "Obsidian";
        };
      in
      ''
        wrapProgram $out/bin/${pname} \
           --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WaylandWindowDecorations" \
           --add-flags "--no-sandbox --disable-gpu-sandbox"

        if [ -f ${contents}/Obsidian.desktop ]; then
          install -m 444 -D ${contents}/Obsidian.desktop $out/share/applications/${pname}.desktop
          substituteInPlace $out/share/applications/${pname}.desktop \
            --replace-warn 'Exec=Obsidian' 'Exec=${pname}'
        fi
      '';

    meta = with lib; {
      description = "A cross-platform note taking application";
      homepage = "https://obsidian.md";
      license = licenses.unfree;
      mainProgram = "obsidian";
      platforms = platforms.linux;
    };
  };
} 
