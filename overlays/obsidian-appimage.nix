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

        # The AppImage's .desktop runs AppRun directly; point it at our wrapper
        # (which already passes --no-sandbox) and ship its icons so KDE can find
        # the Icon=obsidian it names.
        install -m 444 -D ${contents}/obsidian.desktop $out/share/applications/${pname}.desktop
        substituteInPlace $out/share/applications/${pname}.desktop \
          --replace-fail 'Exec=AppRun --no-sandbox' 'Exec=${pname}'

        mkdir -p $out/share/icons
        cp -r --no-preserve=mode ${contents}/usr/share/icons/hicolor $out/share/icons/
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
