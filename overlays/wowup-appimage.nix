self: super: with super; {
  wowup-appimage = appimageTools.wrapType2 rec {
    pname = "wowup";
    version = "2.20.0";

    src = fetchurl {
      url = "https://github.com/WowUp/WowUp/releases/download/v${version}/WowUp-${version}.AppImage";
      hash = "sha256-oDlmL/1N+6Q4zBExLlnILL4LSoz+aF2tSA/x+WpsZ4A=";
    };

    nativeBuildInputs = [ makeWrapper ];

    extraInstallCommands =
      let
        contents = appimageTools.extract {
          inherit version src;
          pname = "WowUp";
        };
      in
      ''
        wrapProgram $out/bin/${pname} \
          --add-flags "--disable-gpu-sandbox --disable-features=UseOzonePlatform" \
          --set ELECTRON_OZONE_PLATFORM_HINT "" \
          --unset WAYLAND_DISPLAY

        if [ -f ${contents}/wowup.desktop ]; then
          install -m 444 -D ${contents}/wowup.desktop $out/share/applications/${pname}.desktop
          substituteInPlace $out/share/applications/${pname}.desktop \
            --replace-warn 'Exec=wowup' 'Exec=${pname}'
        fi
      '';

    meta = with lib; {
      description = "WowUp is the community trusted World of Warcraft addon updater";
      homepage = "https://wowup.io";
      license = licenses.gpl3;
      mainProgram = "wowup";
      platforms = platforms.linux;
    };
  };
}