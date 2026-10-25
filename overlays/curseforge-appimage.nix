self: super: with super; {
  curseforge-appimage = appimageTools.wrapType2 rec {
    pname = "curseforge";
    version = "1.321.1-39714";

    # Upstream only publishes a "latest" link whose contents change with every
    # release, so a pinned copy is served from garfield's LAN file host
    # (modules/nixos/appimage-host.nix).
    src = fetchurl {
      url = "http://10.0.10.134:8088/curseforge-${version}-linux.AppImage";
      hash = "sha256-4DQZNlrJGY1gGAyqB74+vhhI9lCDPAEQrayhSX5G0Uc=";
    };

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

        # The AppImage's .desktop runs AppRun directly; point it at our wrapper
        # (which already passes --no-sandbox) and ship its icons so KDE can find
        # the Icon=curseforge it names.
        install -m 444 -D ${contents}/curseforge.desktop $out/share/applications/${pname}.desktop
        substituteInPlace $out/share/applications/${pname}.desktop \
          --replace-fail 'Exec=AppRun --no-sandbox' 'Exec=${pname}'

        mkdir -p $out/share/icons
        cp -r --no-preserve=mode ${contents}/usr/share/icons/hicolor $out/share/icons/
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
