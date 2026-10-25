self: super: with super; {
  eden-appimage =
    let
      pname = "eden";
      version = "0.2.1";

      # The GitHub mirror of Eden's releases returns 451; eden-emu.dev serves them.
      src = fetchurl {
        url = "https://git.eden-emu.dev/eden-emu/eden/releases/download/v${version}/Eden-Linux-v${version}-amd64-clang-pgo.AppImage";
        hash = "sha256-eii/mIsGSIMZiXIr26qQqzE3G0A4CBmYE+DqfIslum0=";
      };

      # Eden ships a DwarFS AppImage (uruntime), which appimageTools.extract
      # can't read since it only understands squashfs. Unpack it with
      # dwarfsextract and hand the tree to wrapAppImage instead.
      extracted = runCommand "${pname}-${version}-extracted" {
        nativeBuildInputs = [ dwarfs ];
      } ''
        mkdir $out
        dwarfsextract -i ${src} -O auto -o $out
      '';
    in
    appimageTools.wrapAppImage {
      inherit pname version;
      src = extracted;

      # The .desktop already runs Exec=eden, which is our wrapper; ship it with
      # the icon it names so KDE lists Eden under Games.
      extraInstallCommands = ''
        install -m 444 -D ${extracted}/dev.eden_emu.eden.desktop $out/share/applications/dev.eden_emu.eden.desktop
        install -m 444 -D ${extracted}/dev.eden_emu.eden.svg $out/share/icons/hicolor/scalable/apps/dev.eden_emu.eden.svg
      '';

      meta = with lib; {
        description = "Nintendo Switch emulator derived from Yuzu and Sudachi";
        homepage = "https://eden-emu.dev";
        license = licenses.gpl3Plus;
        mainProgram = "eden";
        platforms = platforms.linux;
      };
    };
}
