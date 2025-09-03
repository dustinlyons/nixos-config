self: super: with super; {
  tableplus-appimage = appimageTools.wrapType2 rec {
    pname = "tableplus";
    version = "latest";

    src = fetchurl {
      url = "https://tableplus.com/release/linux/x64/TablePlus-x64.AppImage";
      hash = "sha256-3qMFdjtwnGyGTZqHgKRA9RKH4cQgOJ9RsS/0hzH+tKU=";
    };

    nativeBuildInputs = [ makeWrapper ];

    extraInstallCommands =
      let
        contents = appimageTools.extract {
          inherit version src;
          pname = "TablePlus";
        };
      in
      ''
        # TablePlus needs access to system libraries
        wrapProgram $out/bin/${pname} \
          --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ gnutls ]}"

        if [ -f ${contents}/tableplus-appimage.desktop ]; then
          install -m 444 -D ${contents}/tableplus-appimage.desktop $out/share/applications/${pname}.desktop
          substituteInPlace $out/share/applications/${pname}.desktop \
            --replace-warn 'Exec=tableplus-appimage' 'Exec=${pname}'
        fi
      '';

    meta = with lib; {
      description = "Modern, native, and friendly GUI tool for relational databases";
      homepage = "https://tableplus.com";
      license = licenses.unfree;
      mainProgram = "tableplus";
      platforms = platforms.linux;
    };
  };
}
